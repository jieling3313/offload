// Special Function Unit - Integrates all custom operations for acceleration

package sfu

import chisel3._
import chisel3.util._
import riscv.core.SFUOp

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

  // State machine for complex operations
  val sIdle :: sExecuting :: sDone :: Nil = Enum(3)
  val state = RegInit(sIdle)

  // Current operation register
  val current_op = RegInit(SFUOp.NOP)

  // Result registers
  val result_reg = RegInit(0.U(32.W))
  val valid_reg = RegInit(false.B)

  // Default outputs
  io.out := result_reg
  io.vec_out := 0.U
  io.vec_out_valid := false.B
  io.busy := state =/= sIdle
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

  /* State Machine */

  switch(state) {
    is(sIdle) {
      valid_reg := false.B

      when(io.start) {
        current_op := io.op
        state := sExecuting

        // Initialize operation based on opcode
        switch(io.op) {
          is(SFUOp.EXP) {
            // Single exponential operation
            exp_unit.io.in := io.in1
            exp_unit.io.valid := true.B
          }

          is(SFUOp.RSQRT) {
            // Single inverse square root
            rsqrt_unit.io.in := io.in1
            rsqrt_unit.io.valid := true.B
          }

          is(SFUOp.SUM) {
            // Vector reduction sum
            accumulator.io.start := true.B
            accumulator.io.length := io.in2(15, 0)
          }

          is(SFUOp.SOFTMAX) {
            // TODO
          }

          is(SFUOp.RMSNORM) {
            // TODO
          }
        }
      }
    }

    is(sExecuting) {
      switch(current_op) {
        is(SFUOp.EXP) {
          when(exp_unit.io.out_valid) {
            result_reg := exp_unit.io.out
            valid_reg := true.B
            state := sDone
          }
        }

        is(SFUOp.RSQRT) {
          when(rsqrt_unit.io.out_valid) {
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
          // TODO
          state := sDone
        }

        is(SFUOp.RMSNORM) {
          // TODO
          state := sDone
        }
      }
    }

    is(sDone) {
      io.done := true.B
      valid_reg := false.B
      state := sIdle
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
 * Pass 1: Find max value
 * Pass 2: Compute exp(x - max) and accumulate sum
 * Pass 3: Divide each exp value by sum
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
  })

  // Sub-modules
  val exp_unit = Module(new ExponentialApproximator)
  val accumulator = Module(new VectorAccumulator)
  val fp_subtractor = Module(new FPSubtractor)
  val fp_divider = Module(new FPDivider)

  // Internal memory for intermediate results (exp values)
  val exp_memory = SyncReadMem(256, UInt(32.W))  // Support up to 256 elements

  // Registers
  val max_value = RegInit(0.U(32.W))
  val sum_value = RegInit(0.U(32.W))
  val element_count = RegInit(0.U(16.W))
  val target_length = RegInit(0.U(16.W))

  // States
  val sIdle :: sFindMax :: sComputeExp :: sDivide :: sDone :: Nil = Enum(5)
  val state = RegInit(sIdle)

  // Default outputs
  io.out := 0.U
  io.out_valid := false.B
  io.done := false.B

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
        max_value := 0.U
        element_count := 0.U
        target_length := io.length
        state := sFindMax
      }
    }

    is(sFindMax) {
      // Find maximum value in input vector
      when(io.in_valid) {
        // Compare with current max
        // TODO: Use FP comparator
        max_value := io.in  // Placeholder
        element_count := element_count + 1.U

        when(element_count === target_length - 1.U) {
          element_count := 0.U
          state := sComputeExp
        }
      }
    }

    is(sComputeExp) {
      // Compute exp(x - max) for each element
      when(io.in_valid) {
        // Subtract max
        fp_subtractor.io.a := io.in
        fp_subtractor.io.b := max_value

        // Compute exp
        exp_unit.io.in := fp_subtractor.io.result
        exp_unit.io.valid := true.B

        // Store exp value
        when(exp_unit.io.out_valid) {
          exp_memory.write(element_count, exp_unit.io.out)

          // Accumulate sum
          accumulator.io.in := exp_unit.io.out
          accumulator.io.in_valid := true.B

          element_count := element_count + 1.U

          when(element_count === target_length - 1.U) {
            sum_value := accumulator.io.out
            element_count := 0.U
            state := sDivide
          }
        }
      }
    }

    is(sDivide) {
      // Divide each exp value by sum
      val exp_val = exp_memory.read(element_count)

      fp_divider.io.a := exp_val
      fp_divider.io.b := sum_value

      io.out := fp_divider.io.result
      io.out_valid := true.B

      element_count := element_count + 1.U

      when(element_count === target_length - 1.U) {
        state := sDone
      }
    }

    is(sDone) {
      io.done := true.B
      state := sIdle
    }
  }
}

/**
 * FP Divider (placeholder)
 */
