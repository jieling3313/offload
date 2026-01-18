// Special Function Unit - Integrates all custom operations for acceleration

package sfu

import chisel3._
import chisel3.util._
import riscv.core.SFUOp

object SFUDebug {
  val DEBUG_SFU = false  // Change to true to enable debug output

  // Conditional printf helper
  def debugPrintf(fmt: String, args: Bits*): Unit = {
    if (DEBUG_SFU) {
      printf(fmt, args: _*)
    }
  }
}

/**
 * Common wait counter utilities for SFU accelerators
 * (FPDivider: 8 cycles, InvSqrt: 11 cycles)
 * Helper functions for common wait counter patterns
 */
object WaitCounterHelper {
  /**
   * Check if wait counter is in range [start, end)
   * Example: waitInRange(counter, 1, 8) returns true for counter values 1,2,3,4,5,6,7
   */
  def waitInRange(counter: UInt, start: Int, end: Int): Bool = {
    (counter >= start.U) && (counter < end.U)
  }

  // Check if wait counter equals target value
  def waitEquals(counter: UInt, target: Int): Bool = {
    counter === target.U
  }

  /**
   * Increment wait counter with saturation at max value
   * Prevents overflow for long waits
   */
  def incrementWait(counter: UInt, max: Int = 255): UInt = {
    Mux(counter < max.U, counter + 1.U, counter)
  }
}

/**
 * Common pipeline latency constants
 * Centralized definition for all SFU sub-modules
 */
object PipelineLatency {
  val FP_MULTIPLIER = 1   // FPMultiplier: 1 cycle (combinational)
  val FP_ADDER = 1        // FPAdder: 1 cycle
  val FP_SUBTRACTOR = 1   // FPSubtractor: 1 cycle
  val FP_DIVIDER = 8      // FPDivider: 8 cycles (Newton-Raphson with LUT)
  val EXPONENTIAL = 5     // ExponentialApproximator: 5 cycles
  val INV_SQRT = 11       // InvSqrt: 11 cycles (2 Newton-Raphson iterations)
}

/**
 * Special Function Unit (SFU)
 *
 * Main module integrating all sub-units:
 * - ExponentialApproximator (for exp operations)
 * - InvSqrt (for fast inverse square root)
 * - VectorAccumulator (for reduction operations)
 *
 * Supports custom instructions:
 * - VEXP: Vector exponential
 * - VRSQRT: Inverse square root
 * - VREDSUM: Vector reduction sum
 * - SOFTMAX: Complete softmax operation
 * - RMSNORM: RMS normalization
 *
 * Interface Signals:
 * Inputs:
 *  op (4-bit): Operation code from SFUOp definitions
 *  start (bool): Command to begin operation
 *  in1, in2 (32-bit): Operands or vector length
 *  vec_in (32-bit): Vector element input for streaming operations
 *  vec_in_valid (bool): Vector input validity signal
 * Outputs:
 *  out (32-bit): Scalar result
 *  vec_out (32-bit): Vector element output
 *  vec_out_valid (bool): Vector output validity
 *  busy (bool): SFU is currently executing
 *  done (bool): Operation complete (pulsed for one cycle)
 *  valid (bool): Result in io.out is valids
 */
class SpecialFunctionUnit extends Module {
  val io = IO(new Bundle {
    // Control
    val op = Input(UInt(4.W))         // SFU operation code
    val start = Input(Bool())         // Start operation

    // Scalar inputs (for single-value operations)
    val in1 = Input(UInt(32.W))       // Input operand 1
    val in2 = Input(UInt(32.W))       // Input operand 2 (or vector length)

    // Vector inputs (for streaming operations)
    val vec_in = Input(UInt(32.W))    // Vector element input
    val vec_in_valid = Input(Bool())  // Vector input valid

    // Outputs
    val out = Output(UInt(32.W))      // Scalar result
    val vec_out = Output(UInt(32.W))  // Vector element output
    val vec_out_valid = Output(Bool()) // Vector output valid

    // Status
    val busy = Output(Bool())         // SFU is busy
    val done = Output(Bool())         // Operation complete
    val valid = Output(Bool())        // Result valid
  })

  // Instantiate sub-modules
  val exp_unit = Module(new ExponentialApproximator)
  val rsqrt_unit = Module(new InvSqrt)
  val accumulator = Module(new VectorAccumulator)
  val rmsnorm_unit = Module(new RMSNormAccelerator)

  // State machine for complex operations
  val sIdle :: sExecuting :: sDone :: Nil = Enum(3)
  val state = RegInit(sIdle)

  // Current operation register
  val current_op = RegInit(SFUOp.NOP)

  // Result registers
  val result_reg = RegInit(0.U(32.W))
  val valid_reg = RegInit(false.B)

  // Track the last operation
  val last_op_processed = RegInit(SFUOp.NOP)

  // Counter to sDone state (2 cycles)
  // For the instruction to leave EX
  val done_counter = RegInit(0.U(1.W))

  // Busy for 1 cycle after leaving sDone
  val prev_state = RegNext(state, sIdle)
  val just_left_sDone = (prev_state === sDone) && (state === sIdle)

  // if is a new operation
  val is_new_operation = io.op =/= last_op_processed

  // Clear last_op_processed when start signal goes low (instruction left EX)
  when(!io.start) {
    last_op_processed := SFUOp.NOP
  }

  // Output result
  // !!! The busy hold mechanism ensures ex2mem captures the result
  io.out := result_reg

  // Vector outputs for RMSNORM, SOFTMAX
  io.vec_out := Mux(current_op === SFUOp.RMSNORM, rmsnorm_unit.io.out, 0.U)
  io.vec_out_valid := Mux(current_op === SFUOp.RMSNORM, rmsnorm_unit.io.out_valid, false.B)

  // Problem: When >= 2 custom instructions execute:
  //   1. First instruction completes, busy: 1->0, pipeline unstalls
  //   2. Second instruction enters EX, start asserts, busy: 0->1
  //   3. This 0->1 transition happens in the same cycle, but ID2EX PipelineRegister
  //      uses combinational bypass (out := in when stall=false), so it already
  //      started capturing the next instruction before busy could re-assert
  //
  // Solution 1: Make busy signal combinational with io.start signal
  //   When io.start asserts (custom instruction enters EX), busy becomes true
  //   IMMEDIATELY in the same cycle, preventing ID2EX from bypassing.
  //
  // Solution 2: Hold busy for 1 cycle after sDone
  //   This allows the old instruction to leave EX, clearing operation_done,
  //   before the new instruction can enter and re-assert operation_done.

  // Combinational busy: true when executing, done, just left done, or about to start a new operation
  io.busy := (state === sExecuting) || (state === sDone) ||
             just_left_sDone ||  // Hold busy for 1 cycle after sDone
             (state === sIdle && io.start && is_new_operation)  // Combinational start detection
  io.done := false.B
  io.valid := valid_reg

  // Default sub-module inputs
  exp_unit.io.in := 0.U
  exp_unit.io.valid := false.B

  rsqrt_unit.io.in := 0.U
  rsqrt_unit.io.valid := false.B

  accumulator.io.start := false.B
  accumulator.io.in := 0.U
  accumulator.io.in_valid := false.B
  accumulator.io.length := 0.U

  rmsnorm_unit.io.start := false.B
  rmsnorm_unit.io.length := 0.U
  rmsnorm_unit.io.gain := 0.U
  rmsnorm_unit.io.in := 0.U
  rmsnorm_unit.io.in_valid := false.B

  /* State Machine */

  switch(state) {
    is(sIdle) {
      valid_reg := false.B

      // Start new operation only if different from the last operation
      // Prevents restarting when instruction stays in EX during "sDone",
      // Allows different custom instructions to execute
      when(io.start && is_new_operation) {
        current_op := io.op
        state := sExecuting
        last_op_processed := io.op  // Mark operation
        SFUDebug.debugPrintf("[SFU] New instruction detected, op=%d (was %d), in1=0x%x\n", io.op, last_op_processed, io.in1)

        // Initialize operation based on opcode
        switch(io.op) {
          is(SFUOp.EXP) {
            // Single exponential operation
            SFUDebug.debugPrintf("[SFU] Starting EXP operation, input=0x%x\n", io.in1)
            exp_unit.io.in := io.in1
            exp_unit.io.valid := true.B
          }

          is(SFUOp.RSQRT) {
            // Single inverse square root
            SFUDebug.debugPrintf("[SFU] Starting RSQRT operation, input=0x%x\n", io.in1)
            rsqrt_unit.io.in := io.in1
            rsqrt_unit.io.valid := true.B
          }

          is(SFUOp.SUM) {
            // Vector reduction sum
            accumulator.io.start := true.B
            accumulator.io.length := io.in2(15, 0)
          }

          is(SFUOp.SOFTMAX) {
            // Complex softmax operation
            // Will be handled in multi-cycle execution
          }

          is(SFUOp.RMSNORM) {
            // RMSNorm operation
            // in1 = gain, in2[15:0] = length
            SFUDebug.debugPrintf("[SFU] Starting RMSNORM operation, gain=0x%x, length=%d\n", io.in1, io.in2(15, 0))
            rmsnorm_unit.io.start := true.B
            rmsnorm_unit.io.length := io.in2(15, 0)
            rmsnorm_unit.io.gain := io.in1
          }
        }
      }
    }

    is(sExecuting) {
      // Handle different operations
      switch(current_op) {
        is(SFUOp.EXP) {
          when(exp_unit.io.out_valid) {
            SFUDebug.debugPrintf("[SFU] EXP result ready: 0x%x, transitioning to sDone\n", exp_unit.io.out)
            result_reg := exp_unit.io.out
            valid_reg := true.B
            state := sDone
          }
        }

        is(SFUOp.RSQRT) {
          when(rsqrt_unit.io.out_valid) {
            SFUDebug.debugPrintf("[SFU] RSQRT result ready: 0x%x, transitioning to sDone\n", rsqrt_unit.io.out)
            result_reg := rsqrt_unit.io.out
            valid_reg := true.B
            state := sDone
          }
        }

        is(SFUOp.SUM) {
          // Feed vector elements to accumulator
          accumulator.io.in := io.vec_in
          accumulator.io.in_valid := io.vec_in_valid

          when(accumulator.io.done) {
            result_reg := accumulator.io.out
            valid_reg := true.B
            state := sDone
          }
        }

        is(SFUOp.SOFTMAX) {
          // TODO: Implement full softmax
          state := sDone
        }

        is(SFUOp.RMSNORM) {
          // Feed vector elements to RMSNorm
          rmsnorm_unit.io.in := io.vec_in
          rmsnorm_unit.io.in_valid := io.vec_in_valid

          when(rmsnorm_unit.io.done) {
            SFUDebug.debugPrintf("[SFU] RMSNORM operation complete\n")
            valid_reg := true.B
            state := sDone
          }
        }
      }
    }

    is(sDone) {
      // Stay in sDone for 2 cycles to ensure:
      // 1. Result is stable for ex2mem to capture
      // 2. Instruction has time to leave EX stage so operation_done can be cleared
      when(done_counter === 0.U) {
        SFUDebug.debugPrintf("[SFU] sDone cycle 1: result_reg=0x%x\n", result_reg)
        done_counter := 1.U
        io.done := false.B  // Not done yet
      }.otherwise {
        SFUDebug.debugPrintf("[SFU] sDone cycle 2: result_reg=0x%x, transitioning to sIdle\n", result_reg)
        done_counter := 0.U
        state := sIdle
        io.done := true.B  // Now done
      }
      // Keep valid_reg true during sDone so tests can verify it when done asserts
      // !!! valid_reg will be cleared when returning to sIdle
    }
  }
}

/**
 * Softmax Accelerator
 *
 * Dedicated module for Softmax operation
 * Implements: y_i = exp(x_i - max(x)) / sum(exp(x_j - max(x)))
 *
 * Multi-pass algorithm:
 * Pass 1: Collect inputs and find max value
 * Pass 2: Compute exp(x - max) and accumulate sum
 * Pass 3: Divide each exp value by sum
 *
 * Design based on RMSNormAccelerator's two-phase architecture
 */
class SoftmaxAccelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val length = Input(UInt(16.W))

    // Input stream
    val in = Input(UInt(32.W))
    val in_valid = Input(Bool())

    // Output stream
    val out = Output(UInt(32.W))
    val out_valid = Output(Bool())

    val done = Output(Bool())
    val busy = Output(Bool())
  })

  // FP max comparator
  // Returns true if a >= b (IEEE 754 comparison)
  def fpGreaterOrEqual(a: UInt, b: UInt): Bool = {
    val a_sign = a(31)
    val b_sign = b(31)
    val a_mag = a(30, 0)
    val b_mag = b(30, 0)

    Mux(a_sign === b_sign,
      Mux(a_sign === 0.U,
        a_mag >= b_mag,  // Both positive: compare magnitude
        a_mag <= b_mag   // Both negative: reverse comparison
      ),
      b_sign === 1.U     // Different signs: positive is greater
    )
  }

  // Sub-modules
  val exp_unit = Module(new ExponentialApproximator)
  val accumulator = Module(new VectorAccumulator)
  val fp_subtractor = Module(new FPSubtractor)
  val fp_divider = Module(new FPDivider)

  // Internal memory for inputs and intermediate results
  // !!! Use combinational Mem instead of SyncReadMem (0-cycle read latency)
  val input_memory = Mem(256, UInt(32.W))  // Store original inputs
  val exp_memory = Mem(256, UInt(32.W))    // Store exp(x - max) values

  // Registers
  val max_value = RegInit(0.U(32.W))
  val sum_exp = RegInit(0.U(32.W))
  val element_count = RegInit(0.U(16.W))
  val target_length = RegInit(0.U(16.W))
  val wait_counter = RegInit(0.U(8.W))

  // Pipeline registers to keep FP unit inputs stable (critical!)
  val sub_a_reg = RegInit(0.U(32.W))
  val sub_b_reg = RegInit(0.U(32.W))
  val div_a_reg = RegInit(0.U(32.W))
  val div_b_reg = RegInit(0.U(32.W))
  val exp_input_reg = RegInit(0.U(32.W))

  // Flags
  val all_exp_sent = RegInit(false.B)

 /** 
  * States: sIdle -> sCollectInput -> sFindMax -> sComputeExpStart ->
  * sComputeExp -> sAccumulate -> sDivide -> sDone
  */ 
  val sIdle :: sCollectInput :: sFindMax :: sComputeExpStart :: sComputeExp :: sAccumulate :: sDivide :: sDone :: Nil = Enum(8)
  val state = RegInit(sIdle)

  // Default outputs
  io.out := 0.U
  io.out_valid := false.B
  io.done := false.B
  io.busy := (state =/= sIdle)

  // Default sub-module inputs
  exp_unit.io.in := 0.U
  exp_unit.io.valid := false.B
  accumulator.io.start := false.B
  accumulator.io.in := 0.U
  accumulator.io.in_valid := false.B
  accumulator.io.length := 0.U
  fp_subtractor.io.a := 0.U
  fp_subtractor.io.b := 0.U
  fp_divider.io.a := 0.U
  fp_divider.io.b := 0.U

  // State machine
  switch(state) {
    is(sIdle) {
      when(io.start) {
        element_count := 0.U
        target_length := io.length
        wait_counter := 0.U
        all_exp_sent := false.B
        max_value := "h00000000".U(32.W)  // Initialize to zero
        state := sCollectInput
        SFUDebug.debugPrintf("[Softmax] Starting Softmax with length=%d\n", io.length)
      }
    }

    is(sCollectInput) {
      // Phase 1: Collect all input elements into memory
      when(io.in_valid) {
        input_memory.write(element_count, io.in)
        SFUDebug.debugPrintf("[Softmax] Collected input[%d]=0x%x\n", element_count, io.in)
        element_count := element_count + 1.U

        when(element_count === target_length - 1.U) {
          element_count := 0.U
          state := sFindMax
          SFUDebug.debugPrintf("[Softmax] All inputs collected, finding max\n")
        }
      }
    }

    is(sFindMax) {
      // Phase 2: Find maximum value by iterating through memory
      val current_value = input_memory.read(element_count)

      // Update max if current value is greater
      when(fpGreaterOrEqual(current_value, max_value)) {
        max_value := current_value
        SFUDebug.debugPrintf("[Softmax] New max at index %d: 0x%x\n", element_count, current_value)
      }

      element_count := element_count + 1.U

      when(element_count === target_length - 1.U) {
        element_count := 0.U
        state := sComputeExpStart
        // Note: max_value will be updated in next cycle
      }
    }

    is(sComputeExpStart) {
      // Start accumulator before processing first element
      // max_value is now stable
      accumulator.io.start := true.B
      accumulator.io.length := target_length
      SFUDebug.debugPrintf("[Softmax] Max value: 0x%x, starting accumulator with length=%d\n", max_value, target_length)
      state := sComputeExp
    }

    is(sComputeExp) {
      // Phase 3: Compute exp(x - max) for each element
      when(wait_counter === 0.U) {
        // Read input, compute x - max
        val input_value = input_memory.read(element_count)
        sub_a_reg := input_value
        sub_b_reg := max_value

        SFUDebug.debugPrintf("[Softmax] ComputeExp: element %d, x=0x%x, max=0x%x\n",
               element_count, input_value, max_value)
        wait_counter := 1.U

      }.elsewhen(wait_counter === 1.U) {
        // Get subtraction result, prepare for exp_unit
        exp_input_reg := fp_subtractor.io.result
        SFUDebug.debugPrintf("[Softmax] Subtract result: 0x%x, starting exp_unit\n", fp_subtractor.io.result)
        wait_counter := 2.U

      }.elsewhen(wait_counter === 2.U) {
        // Send to exp_unit (cycle 1 of 5)
        exp_unit.io.in := exp_input_reg
        exp_unit.io.valid := true.B
        wait_counter := 3.U

      }.elsewhen(wait_counter >= 3.U && wait_counter < 7.U) {
        // Wait for exp_unit (cycles 2-4 of 5)
        wait_counter := wait_counter + 1.U

      }.elsewhen(wait_counter === 7.U) {
        // Check exp_unit result (cycle 5 of 5)
        when(exp_unit.io.out_valid) {
          // Store exp value to memory
          exp_memory.write(element_count, exp_unit.io.out)
          SFUDebug.debugPrintf("[Softmax] exp[%d]=0x%x stored\n", element_count, exp_unit.io.out)

          element_count := element_count + 1.U
          wait_counter := 0.U

          // Check if all elements processed
          when(element_count === target_length - 1.U) {
            element_count := 0.U
            state := sAccumulate
            SFUDebug.debugPrintf("[Softmax] All exp values computed, starting accumulation\n")
          }
        }.otherwise {
          // exp_unit not ready, wait one more cycle
          wait_counter := wait_counter + 1.U
        }
      }.elsewhen(wait_counter > 7.U) {
        // Continue waiting for exp_unit
        when(exp_unit.io.out_valid) {
          exp_memory.write(element_count, exp_unit.io.out)
          SFUDebug.debugPrintf("[Softmax] exp[%d]=0x%x stored (after extra wait)\n", element_count, exp_unit.io.out)

          element_count := element_count + 1.U
          wait_counter := 0.U

          when(element_count === target_length - 1.U) {
            element_count := 0.U
            state := sAccumulate
            SFUDebug.debugPrintf("[Softmax] All exp values computed, starting accumulation\n")
          }
        }.otherwise {
          wait_counter := wait_counter + 1.U
        }
      }

      // Keep subtractor inputs stable during entire computation
      fp_subtractor.io.a := sub_a_reg
      fp_subtractor.io.b := sub_b_reg
    }

    is(sAccumulate) {
      // Send all exp values to accumulator
      when(!all_exp_sent) {
        val exp_value = exp_memory.read(element_count)

        accumulator.io.in := exp_value
        accumulator.io.in_valid := true.B
        accumulator.io.length := target_length

        SFUDebug.debugPrintf("[Softmax] Sending exp[%d]=0x%x to accumulator\n", element_count, exp_value)

        element_count := element_count + 1.U

        when(element_count === target_length - 1.U) {
          all_exp_sent := true.B
          element_count := 0.U
          SFUDebug.debugPrintf("[Softmax] All exp values sent to accumulator\n")
        }
      }.otherwise {
        // Wait for accumulator to finish
        when(accumulator.io.done) {
          sum_exp := accumulator.io.out
          wait_counter := 0.U
          all_exp_sent := false.B
          state := sDivide
          SFUDebug.debugPrintf("[Softmax] Accumulation done: sum_exp=0x%x\n", accumulator.io.out)
        }
      }
    }

    is(sDivide) {
      // Phase 4: Divide each exp value by sum
      when(wait_counter === 0.U) {
        // Read exp value and prepare division
        val exp_value = exp_memory.read(element_count)
        div_a_reg := exp_value
        div_b_reg := sum_exp

        SFUDebug.debugPrintf("[Softmax] Divide: exp[%d]=0x%x / sum=0x%x\n",
               element_count, exp_value, sum_exp)
        wait_counter := 1.U

      }.elsewhen(wait_counter >= 1.U && wait_counter < 9.U) {
        // Wait for divider (8 cycles latency)
        wait_counter := wait_counter + 1.U

      }.elsewhen(wait_counter === 9.U) {
        // Output result
        io.out := fp_divider.io.result
        io.out_valid := true.B

        SFUDebug.debugPrintf("[Softmax] Output[%d]=0x%x\n", element_count, fp_divider.io.result)

        element_count := element_count + 1.U
        wait_counter := 0.U

        when(element_count === target_length - 1.U) {
          state := sDone
        }
      }

      // Keep divider inputs stable
      fp_divider.io.a := div_a_reg
      fp_divider.io.b := div_b_reg
    }

    is(sDone) {
      io.done := true.B
      SFUDebug.debugPrintf("[Softmax] Done!\n")
      state := sIdle
    }
  }
}


/**
 * RMSNorm Accelerator
 *
 * Dedicated module for RMSNorm operation
 * Implements: y_i = (x_i / sqrt(mean(x²))) * gain
 *
 * Multi-pass algorithm:
 * Pass 1: Square each element (x²)
 * Pass 2: Sum and compute mean (Σ(x²) / N)
 * Pass 3: Compute inverse square root (1/√mean)
 * Pass 4: Normalize each element (x_i * norm_factor * gain)
 */
class RMSNormAccelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val length = Input(UInt(16.W))
    val gain = Input(UInt(32.W))      // Gain parameter (IEEE 754 FP)

    // Input stream
    val in = Input(UInt(32.W))
    val in_valid = Input(Bool())

    // Output stream
    val out = Output(UInt(32.W))
    val out_valid = Output(Bool())

    val done = Output(Bool())
    val busy = Output(Bool())
  })

  // Sub-modules
  val multiplier = Module(new FPMultiplier)
  val accumulator = Module(new VectorAccumulator)
  val divider = Module(new FPDivider)
  val inv_sqrt = Module(new InvSqrt)

  // Internal memory to store original input values
  // Use combinational Mem instead of SyncReadMem to avoid 1-cycle read latency
  val input_memory = Mem(256, UInt(32.W))  // Support up to 256 elements

  // Registers
  val element_count = RegInit(0.U(16.W))
  val target_length = RegInit(0.U(16.W))
  val sum_squares = RegInit(0.U(32.W))
  val mean_squares = RegInit(0.U(32.W))
  val norm_factor = RegInit(0.U(32.W))  // 1/sqrt(mean)
  val gain_reg = RegInit(0.U(32.W))

  // Pipeline registers
  val current_input = RegInit(0.U(32.W))     // Store current input being processed
  val squared_value = RegInit(0.U(32.W))
  val temp_mult_result = RegInit(0.U(32.W))  // Store intermediate multiply result
  val wait_counter = RegInit(0.U(8.W))       // For waiting pipeline latencies
  val divider_a_reg = RegInit(0.U(32.W))     // Store divider input A
  val divider_b_reg = RegInit(0.U(32.W))     // Store divider input B
  val mult_a_reg = RegInit(0.U(32.W))        // Store multiplier input A
  val mult_b_reg = RegInit(0.U(32.W))        // Store multiplier input B
  val mem_read_value = RegInit(0.U(32.W))    // Store value read from memory (1 cycle delay)
  val all_data_sent = RegInit(false.B)       // Track if all elements sent to accumulator

/** 
  * States: sIdle -> sCollectInput -> sSquare -> sAccumulate 
  * -> sMean -> sInvSqrt -> sNormalize -> sDone
  */
  val sIdle :: sCollectInput :: sSquare :: sAccumulate :: sMean :: sInvSqrt :: sNormalize :: sDone :: Nil = Enum(8)
  val state = RegInit(sIdle)

  // Default outputs
  io.out := 0.U
  io.out_valid := false.B
  io.done := false.B
  io.busy := (state =/= sIdle)

  // Default sub-module inputs
  multiplier.io.a := 0.U
  multiplier.io.b := 0.U
  accumulator.io.start := false.B
  accumulator.io.in := 0.U
  accumulator.io.in_valid := false.B
  accumulator.io.length := 0.U
  divider.io.a := 0.U
  divider.io.b := 0.U
  inv_sqrt.io.in := 0.U
  inv_sqrt.io.valid := false.B

  // State machine
  switch(state) {
    is(sIdle) {
      when(io.start) {
        element_count := 0.U
        target_length := io.length
        gain_reg := io.gain
        wait_counter := 0.U
        all_data_sent := false.B
        state := sCollectInput
        SFUDebug.debugPrintf("[RMSNorm] Starting RMSNorm with length=%d, gain=0x%x\n", io.length, io.gain)
      }
    }

    is(sCollectInput) {
      // Collect all input elements into memory
      when(io.in_valid) {
        input_memory.write(element_count, io.in)
        SFUDebug.debugPrintf("[RMSNorm] Collected input[%d]=0x%x\n", element_count, io.in)
        element_count := element_count + 1.U

        // When all inputs collected, start processing
        when(element_count === target_length - 1.U) {
          element_count := 0.U
          state := sSquare
          SFUDebug.debugPrintf("[RMSNorm] All inputs collected, starting computation\n")
        }
      }
    }

    is(sSquare) {
      // Start accumulator before processing first element
      when(element_count === 0.U) {
        accumulator.io.start := true.B
        accumulator.io.length := target_length
        SFUDebug.debugPrintf("[RMSNorm] Starting accumulator with length=%d\n", target_length)
      }

      // Read from memory (combinational - immediate result)
      val input_value = input_memory.read(element_count)

      // Square the value
      mult_a_reg := input_value
      mult_b_reg := input_value

      SFUDebug.debugPrintf("[RMSNorm] Square: element %d, x=0x%x\n", element_count, input_value)

      element_count := element_count + 1.U
      state := sAccumulate
    }

    is(sAccumulate) {
      // Compute x² and accumulate
      multiplier.io.a := mult_a_reg
      multiplier.io.b := mult_b_reg

      accumulator.io.length := target_length

      when(!all_data_sent) {
        // Send x² to accumulator
        accumulator.io.in := multiplier.io.result
        accumulator.io.in_valid := true.B
        SFUDebug.debugPrintf("[RMSNorm] Accumulate: element %d, x²=0x%x\n", element_count - 1.U, multiplier.io.result)

        // Check
        when(element_count === target_length) {
          all_data_sent := true.B
          SFUDebug.debugPrintf("[RMSNorm] All elements sent, waiting for accumulator\n")
        }.otherwise {
          state := sSquare
        }
      }.otherwise {
        // All data sent, wait for accumulator to finish
        when(accumulator.io.done) {
          sum_squares := accumulator.io.out
          element_count := 0.U
          wait_counter := 8.U       // Divider (8 cycles)
          all_data_sent := false.B  // Reset for next operation
          state := sMean
          SFUDebug.debugPrintf("[RMSNorm] Sum complete: Σ(x²)=0x%x\n", accumulator.io.out)
        }
      }
    }

    is(sMean) {
      // Compute mean = sum / N
      when(wait_counter === 8.U) {
        // Prepare division inputs
        val length_float = MuxLookup(target_length, 0x3f800000.U(32.W))(Seq(
          1.U  -> "h3f800000".U(32.W),  // 1.0
          2.U  -> "h40000000".U(32.W),  // 2.0
          3.U  -> "h40400000".U(32.W),  // 3.0
          4.U  -> "h40800000".U(32.W),  // 4.0
          5.U  -> "h40a00000".U(32.W),  // 5.0
          6.U  -> "h40c00000".U(32.W),  // 6.0
          7.U  -> "h40e00000".U(32.W),  // 7.0
          8.U  -> "h41000000".U(32.W),  // 8.0
          16.U -> "h41800000".U(32.W),  // 16.0
          32.U -> "h42000000".U(32.W),  // 32.0
          64.U -> "h42800000".U(32.W),  // 64.0
          128.U -> "h43000000".U(32.W), // 128.0
          256.U -> "h43800000".U(32.W)  // 256.0
        ))
        divider_a_reg := sum_squares
        divider_b_reg := length_float

        SFUDebug.debugPrintf("[RMSNorm] Computing mean: sum=0x%x / N=%d (float=0x%x)\n", sum_squares, target_length, length_float)
        wait_counter := wait_counter - 1.U
      }.elsewhen(wait_counter > 0.U) {
        wait_counter := wait_counter - 1.U
      }.elsewhen(wait_counter === 0.U) {
        // Mean result
        mean_squares := divider.io.result
        wait_counter := 11.U  // InvSqrt (11 cycles)
        state := sInvSqrt
        SFUDebug.debugPrintf("[RMSNorm] Mean computed: mean(x²)=0x%x\n", divider.io.result)
      }

      // Keep divider inputs stable during entire sMean state
      divider.io.a := divider_a_reg
      divider.io.b := divider_b_reg
    }

    is(sInvSqrt) {
      // Compute 1/sqrt(mean)
      when(wait_counter === 11.U) {
        inv_sqrt.io.in := mean_squares
        inv_sqrt.io.valid := true.B
        SFUDebug.debugPrintf("[RMSNorm] Computing InvSqrt: input=0x%x\n", mean_squares)
        wait_counter := wait_counter - 1.U
      }.elsewhen(wait_counter > 0.U) {
        wait_counter := wait_counter - 1.U
      }.elsewhen(wait_counter === 0.U) {
        // Check if InvSqrt result is ready
        when(inv_sqrt.io.out_valid) {
          norm_factor := inv_sqrt.io.out
          element_count := 0.U
          wait_counter := 0.U
          state := sNormalize
          SFUDebug.debugPrintf("[RMSNorm] InvSqrt computed: 1/√mean=0x%x\n", inv_sqrt.io.out)
        }
      }
    }

    is(sNormalize) {
      when(wait_counter === 0.U) {
        // Read original value from memory (combinational - immediate)
        val original_value = input_memory.read(element_count)
        // First multiply: x * norm_factor
        mult_a_reg := original_value
        mult_b_reg := norm_factor
        wait_counter := 1.U
        SFUDebug.debugPrintf("[RMSNorm] Normalize step 1: x[%d]=0x%x * norm_factor=0x%x\n",
               element_count, original_value, norm_factor)
      }.elsewhen(wait_counter === 1.U) {
        // Capture first multiply result
        temp_mult_result := multiplier.io.result
        wait_counter := 2.U
        SFUDebug.debugPrintf("[RMSNorm] Normalize step 1 result: temp=0x%x\n", multiplier.io.result)
      }.elsewhen(wait_counter === 2.U) {
        // Second multiply: (x * norm_factor) * gain
        mult_a_reg := temp_mult_result
        mult_b_reg := gain_reg
        wait_counter := 3.U
        SFUDebug.debugPrintf("[RMSNorm] Normalize step 2: temp=0x%x * gain=0x%x\n",
               temp_mult_result, gain_reg)
      }.elsewhen(wait_counter === 3.U) {
        // Capture second multiply result and output
        io.out := multiplier.io.result
        io.out_valid := true.B
        SFUDebug.debugPrintf("[RMSNorm] Output[%d]=0x%x\n", element_count, multiplier.io.result)

        element_count := element_count + 1.U

        when(element_count === target_length - 1.U) {
          state := sDone
        }.otherwise {
          wait_counter := 0.U
        }
      }

      // Keep multiplier inputs stable
      multiplier.io.a := mult_a_reg
      multiplier.io.b := mult_b_reg
    }

    is(sDone) {
      io.done := true.B
      SFUDebug.debugPrintf("[RMSNorm] Done!\n")
      state := sIdle
    }
  }
}