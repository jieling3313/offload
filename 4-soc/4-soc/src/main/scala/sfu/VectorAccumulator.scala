// Vector Accumulator for reduction operations (sum, mean, etc.)

package sfu

import chisel3._
import chisel3.util._

/**
 * Vector Accumulator for Reduction Operations
 *
 * Performs streaming accumulation of vector elements
 * Used for operations like:
 * - Sum reduction (for Softmax normalization)
 * - Mean calculation (for RMSNorm)
 * - Dot product
 *
 * Supports IEEE 754 single-precision floating-point
 *
 * Latency: N cycles for N elements (streaming)
 */
class VectorAccumulator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())        // Start accumulation
    val in = Input(UInt(32.W))       // Input element (IEEE 754 float)
    val in_valid = Input(Bool())     // Input valid
    val length = Input(UInt(16.W))   // Vector length
    val out = Output(UInt(32.W))     // Accumulated result
    val done = Output(Bool())        // Accumulation complete
  })

  // States
  val sIdle :: sAccumulating :: sDone :: Nil = Enum(3)
  val state = RegInit(sIdle)

  // Accumulator register
  val accumulator = RegInit(0.U(32.W))

  // Element counter
  val count = RegInit(0.U(16.W))
  val target_length = RegInit(0.U(16.W))

  // Floating-point adder instance
  val fp_adder = Module(new FPAdder)

  // Default values for FP adder (to avoid uninitialized reference errors)
  fp_adder.io.a := 0.U
  fp_adder.io.b := 0.U

  // Default outputs
  io.out := accumulator
  io.done := false.B

  // State machine
  switch(state) {
    is(sIdle) {
      when(io.start) {
        accumulator := 0.U  // Reset accumulator (0.0 in IEEE 754 is 0x00000000)
        count := 0.U
        target_length := io.length
        state := sAccumulating
      }
    }

    is(sAccumulating) {
      when(io.in_valid) {
        // Add current input to accumulator
        fp_adder.io.a := accumulator
        fp_adder.io.b := io.in
        accumulator := fp_adder.io.result

        count := count + 1.U

        when(count === target_length - 1.U) {
          state := sDone
        }
      }
    }

    is(sDone) {
      io.done := true.B
      state := sIdle
    }
  }
}

/**
 * Floating-Point Adder (IEEE 754 Single Precision)
 *
 * Computes a + b
 *
 * Steps:
 * 1. Align exponents
 * 2. Add mantissas
 * 3. Normalize result
 * 4. Round
 */
class FPAdder extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
    val result = Output(UInt(32.W))
  })

  // Check for zero operands first (exp == 0 and mant == 0)
  val a_is_zero = io.a === 0.U
  val b_is_zero = io.b === 0.U

  // If one operand is zero, return the other
  when(a_is_zero && b_is_zero) {
    io.result := 0.U
  }.elsewhen(a_is_zero) {
    io.result := io.b
  }.elsewhen(b_is_zero) {
    io.result := io.a
  }.otherwise {
    // Both operands are non-zero, perform normal addition

    // Extract fields from operand A
    val sign_a = io.a(31)
    val exp_a = io.a(30, 23)
    val mant_a = Cat(1.U(1.W), io.a(22, 0))  // Add implicit leading 1

    // Extract fields from operand B
    val sign_b = io.b(31)
    val exp_b = io.b(30, 23)
    val mant_b = Cat(1.U(1.W), io.b(22, 0))

  /* Step 1: Exponent alignment */

  // Determine which operand has larger exponent
  val exp_diff = Wire(SInt(9.W))
  exp_diff := exp_a.asSInt - exp_b.asSInt

  val larger_exp = Wire(UInt(8.W))
  val aligned_mant_a = Wire(UInt(26.W))  // Extra bits for rounding
  val aligned_mant_b = Wire(UInt(26.W))

  when(exp_diff >= 0.S) {
    // Exponent A is larger or equal
    larger_exp := exp_a
    aligned_mant_a := Cat(mant_a, 0.U(2.W))  // Add guard bits
    aligned_mant_b := Cat(mant_b, 0.U(2.W)) >> exp_diff.asUInt
  }.otherwise {
    // Exponent B is larger
    larger_exp := exp_b
    aligned_mant_a := Cat(mant_a, 0.U(2.W)) >> (-exp_diff).asUInt
    aligned_mant_b := Cat(mant_b, 0.U(2.W))
  }

  /* Step 2: Add or subtract mantissas based on signs */

  val effective_sign_a = sign_a
  val effective_sign_b = sign_b

  val mant_sum = Wire(UInt(27.W))  // Result may need extra bit
  val result_sign = Wire(Bool())

  when(effective_sign_a === effective_sign_b) {
    // Same sign: add mantissas
    mant_sum := aligned_mant_a +& aligned_mant_b
    result_sign := effective_sign_a
  }.otherwise {
    // Different signs: subtract mantissas
    when(aligned_mant_a >= aligned_mant_b) {
      mant_sum := aligned_mant_a - aligned_mant_b
      result_sign := effective_sign_a
    }.otherwise {
      mant_sum := aligned_mant_b - aligned_mant_a
      result_sign := effective_sign_b
    }
  }

  /* Step 3: Normalize result */

  // Find leading 1 position for normalization
  val normalized_mant = Wire(UInt(23.W))
  val normalized_exp = Wire(UInt(8.W))

  // Check if overflow occurred (bit 26 is set)
  when(mant_sum(26) === 1.U) {
    // Right shift by 1, increment exponent
    normalized_mant := mant_sum(25, 3)
    normalized_exp := larger_exp + 1.U
  }.elsewhen(mant_sum(25) === 1.U) {
    // No normalization needed
    normalized_mant := mant_sum(24, 2)
    normalized_exp := larger_exp
  }.otherwise {
    // Need to left shift (find leading 1)
    // Simplified: just use as-is (proper implementation needs priority encoder)
    normalized_mant := mant_sum(22, 0)
    normalized_exp := larger_exp
  }

  /* Step 4: Assemble result */

    // Handle special case: result is zero
    val is_zero = mant_sum === 0.U

    io.result := Mux(is_zero,
      0.U(32.W),
      Cat(result_sign, normalized_exp, normalized_mant)
    )
  }
}

/**
 * Vector Mean Calculator
 *
 * Computes mean of a vector by accumulating sum and dividing by length
 * Used in RMSNorm calculation
 */
class VectorMean extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val in = Input(UInt(32.W))       // Input element
    val in_valid = Input(Bool())
    val length = Input(UInt(16.W))
    val out = Output(UInt(32.W))     // Mean value
    val done = Output(Bool())
  })

  // Use vector accumulator for sum
  val accumulator = Module(new VectorAccumulator)

  accumulator.io.start := io.start
  accumulator.io.in := io.in
  accumulator.io.in_valid := io.in_valid
  accumulator.io.length := io.length

  // States
  val sIdle :: sAccumulating :: sDividing :: sDone :: Nil = Enum(4)
  val state = RegInit(sIdle)

  val sum = RegInit(0.U(32.W))
  val count = RegInit(0.U(16.W))

  io.done := false.B
  io.out := 0.U

  switch(state) {
    is(sIdle) {
      when(io.start) {
        count := io.length
        state := sAccumulating
      }
    }

    is(sAccumulating) {
      when(accumulator.io.done) {
        sum := accumulator.io.out
        state := sDividing
      }
    }

    is(sDividing) {
      // Divide sum by count
      // TODO: Implement FP division or use reciprocal approximation
      // For now, placeholder
      io.out := sum
      state := sDone
    }

    is(sDone) {
      io.done := true.B
      state := sIdle
    }
  }
}

/**
 * TODO for complete implementation:
 *
 * 1. Proper leading-zero counter for normalization
 *    - Use priority encoder to find leading 1
 *    - Shift mantissa and adjust exponent accordingly
 *
 * 2. Rounding logic
 *    - Implement round-to-nearest-even (banker's rounding)
 *    - Handle guard, round, and sticky bits
 *
 * 3. Special case handling
 *    - NaN propagation
 *    - Infinity handling
 *    - Zero detection (positive/negative zero)
 *
 * 4. Overflow/underflow detection
 *    - Saturate to max/min representable values
 *    - Set appropriate flags
 *
 * 5. FP division for VectorMean
 *    - Either implement full FP divider
 *    - Or use reciprocal approximation + multiplication
 *
 * 6. Pipelining optimization
 *    - Multi-cycle FP addition if needed for timing
 *    - Add pipeline registers
 */
