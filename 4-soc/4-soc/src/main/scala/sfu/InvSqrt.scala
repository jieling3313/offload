// Fast Inverse Square Root (1/sqrt(x)) using Newton-Raphson iteration
// Based on the famous Quake III algorithm

package sfu

import chisel3._
import chisel3.util._

/**
 * Fast Inverse Square Root Calculator
 *
 * Algorithm: Newton-Raphson iteration with magic constant initial guess
 *
 * y = 1/sqrt(x)
 *
 * Method:
 * 1. Magic constant trick for initial approximation
 * 2. Newton-Raphson refinement: y_new = y * (1.5 - 0.5 * x * y * y)
 *
 * Accuracy:
 * - 1 iteration: ~1% error
 * - 2 iterations: ~0.1% error
 *
 * Latency: 3-4 cycles (pipelined)
 *
 * Uses IEEE 754 single-precision floating-point format
 */
class InvSqrt extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(32.W))       // Input (IEEE 754 float)
    val out = Output(UInt(32.W))     // Output: 1/sqrt(input)
    val valid = Input(Bool())        // Input valid
    val ready = Output(Bool())       // Ready for new input
    val out_valid = Output(Bool())   // Output valid
  })

  // Pipeline stages
  val stage1_reg = Reg(UInt(32.W))
  val stage2_reg = Reg(UInt(32.W))
  val stage3_reg = Reg(UInt(32.W))

  val valid1 = RegNext(io.valid, false.B)
  val valid2 = RegNext(valid1, false.B)
  val valid3 = RegNext(valid2, false.B)

  // Constants
  val MAGIC_CONSTANT = "h5f3759df".U(32.W)  // Quake III magic number
  val THREE_HALVES = "h3fc00000".U(32.W)    // 1.5 in IEEE 754
  val HALF = "h3f000000".U(32.W)            // 0.5 in IEEE 754

  /* Stage 1: Magic constant initial guess */

  // The Quake III trick:
  // Treat float bits as integer, shift right by 1, subtract from magic constant

  val input_bits = io.in
  val half_input = input_bits >> 1  // Logical shift right

  // Initial approximation
  val y0 = MAGIC_CONSTANT - half_input

  stage1_reg := y0

  /* Stage 2: First Newton-Raphson iteration */

  // Iteration formula: y1 = y0 * (1.5 - 0.5 * x * y0 * y0)
  //
  // This requires:
  // 1. y0 * y0
  // 2. x * (y0 * y0)
  // 3. 0.5 * x * (y0 * y0)
  // 4. 1.5 - 3.( 0.5 * x * (y0 * y0) )
  // 5. y0 * 4. ( 1.5 - 3.( 0.5 * x * (y0 * y0) ) )

  val y0_s2 = stage1_reg
  val x_s2 = RegNext(input_bits)

  // Placeholder for FP operations
  // TODO: Use proper IEEE 754 multiplier and subtractor

  // y0 * y0
  val y0_squared = Wire(UInt(32.W))
  y0_squared := y0_s2

  // 0.5 * x * y0^2
  val half_x_y0sq = Wire(UInt(32.W))
  half_x_y0sq := x_s2 

  // 1.5 - (0.5 * x * y0^2)
  val factor = Wire(UInt(32.W))
  factor := THREE_HALVES

  // y0 * factor
  val y1 = Wire(UInt(32.W))
  y1 := y0_s2

  stage2_reg := y1

  /* Stage 3: Second Newton-Raphson iteration (optional) */

  // Same formula: y2 = y1 * (1.5 - 0.5 * x * y1 * y1)
  // This improves accuracy to ~0.1% error

  val y1_s3 = stage2_reg
  val x_s3 = RegNext(RegNext(input_bits))

  // Repeat the Newton-Raphson step
  val y1_squared = Wire(UInt(32.W))
  y1_squared := y1_s3

  val half_x_y1sq = Wire(UInt(32.W))
  half_x_y1sq := x_s3

  val factor2 = Wire(UInt(32.W))
  factor2 := THREE_HALVES

  val y2 = Wire(UInt(32.W))
  y2 := y1_s3

  stage3_reg := y2

  /* Output */

  io.out := stage3_reg
  io.out_valid := valid3
  io.ready := true.B  // Always ready in this implementation
}

/**
 * Floating-Point Multiplier Module (IEEE 754)
 *
 * This is a helper module for FP multiplication
 * Used internally by **InvSqrt** for Newton-Raphson iterations
 */
class FPMultiplier extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
    val result = Output(UInt(32.W))
  })

  // Extract components
  val sign_a = io.a(31)
  val exp_a = io.a(30, 23)
  val mant_a = Cat(1.U(1.W), io.a(22, 0))  // Add implicit leading 1

  val sign_b = io.b(31)
  val exp_b = io.b(30, 23)
  val mant_b = Cat(1.U(1.W), io.b(22, 0))

  // Result sign: XOR of input signs
  val sign_result = sign_a ^ sign_b

  // Result exponent: sum of exponents minus bias (127)
  val exp_sum = exp_a +& exp_b
  val exp_result = exp_sum - 127.U

  // Mantissa multiplication (24 bits x 24 bits = 48 bits)
  val mant_product = mant_a * mant_b

  // Normalize: if bit 47 is set, shift right and increment exponent
  val normalized_mant = Wire(UInt(23.W))
  val normalized_exp = Wire(UInt(8.W))

  when(mant_product(47) === 1.U) {
    normalized_mant := mant_product(46, 24)
    normalized_exp := exp_result + 1.U
  }.otherwise {
    normalized_mant := mant_product(45, 23)
    normalized_exp := exp_result
  }

  // Assemble result
  io.result := Cat(sign_result, normalized_exp, normalized_mant)
}

/**
 * Floating-Point Subtractor Module (IEEE 754)
 *
 * Computes a - b for positive operands
 * Simplified implementation
 */
class FPSubtractor extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
    val result = Output(UInt(32.W))
  })

  // Extract components
  val sign_a = io.a(31)
  val exp_a = io.a(30, 23)
  val mant_a = Cat(1.U(1.W), io.a(22, 0))

  val sign_b = io.b(31)
  val exp_b = io.b(30, 23)
  val mant_b = Cat(1.U(1.W), io.b(22, 0))

  // Align exponents
  val exp_diff = exp_a.asSInt - exp_b.asSInt

  val aligned_mant_a = Wire(UInt(24.W))
  val aligned_mant_b = Wire(UInt(24.W))
  val result_exp = Wire(UInt(8.W))

  when(exp_diff(7) === 1.U) {  // exp_a < exp_b
    aligned_mant_a := mant_a >> (-exp_diff).asUInt(4, 0)
    aligned_mant_b := mant_b
    result_exp := exp_b
  }.otherwise {  // exp_a >= exp_b
    aligned_mant_a := mant_a
    aligned_mant_b := mant_b >> exp_diff.asUInt(4, 0)
    result_exp := exp_a
  }

  // Subtract mantissas
  val mant_result = aligned_mant_a - aligned_mant_b

  // Normalize (find leading 1)
  // Simplified: just use upper 23 bits
  val normalized_mant = mant_result(22, 0)

  // Result sign (assume a > b for simplicity)
  val sign_result = 0.U

  io.result := Cat(sign_result, result_exp, normalized_mant)
}

/**
 * TODO for complete implementation:
 *
 * 1. Integrate FPMultiplier and FPSubtractor into InvSqrt
 *    - Replace placeholder operations with actual modules
 *
 * 2. Handle special cases:
 *    - Input = 0: return Inf
 *    - Input < 0: return NaN (invalid)
 *    - Input = Inf: return 0
 *    - Input = NaN: return NaN
 *
 * 3. Optimize FP arithmetic modules:
 *    - Proper rounding (round-to-nearest-even)
 *    - Handle denormalized numbers
 *    - Overflow/underflow detection
 *
 * 4. Pipeline optimization:
 *    - Balance stages for critical path
 *    - Add bypass logic if needed
 *
 * 5. Accuracy validation:
 *    - Test against reference sqrt() implementation
 *    - Measure max error across input range
 */
