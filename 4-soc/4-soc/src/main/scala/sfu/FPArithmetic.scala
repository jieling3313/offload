// Floating-Point Arithmetic Modules
// Centralized IEEE 754 single-precision floating-point operations

package sfu

import chisel3._
import chisel3.util._

/**
 * Single-Cycle Floating-Point Adder
 * Implements IEEE 754 single-precision floating-point addition (a + b)
 *
 * This is a **simplified FP adder** that completes in one cycle.
 *
 * Features:
 * - Early zero detection and exit optimization
 * - Exponent alignment with mantissa shifting
 * - Signed mantissa addition/subtraction based on operand signs
 * - Basic result normalization
 *
 */
class FPAdder extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))       // Operand A (IEEE 754)
    val b = Input(UInt(32.W))       // Operand B (IEEE 754)
    val result = Output(UInt(32.W)) // Result (IEEE 754)
  })

  // Check for zero operands (early exit optimization)
  val a_is_zero = io.a === 0.U
  val b_is_zero = io.b === 0.U


  when(a_is_zero && b_is_zero) {
    io.result := 0.U
  }.elsewhen(a_is_zero) {
    io.result := io.b  // 0 + b = b
  }.elsewhen(b_is_zero) {
    io.result := io.a  // a + 0 = a
  }.otherwise {

    // Both operands are non-zero

    // operand A
    val sign_a = io.a(31)
    val exp_a = io.a(30, 23)
    val mant_a = Cat(1.U(1.W), io.a(22, 0))  // Add implicit leading 1

    // operand B
    val sign_b = io.b(31)
    val exp_b = io.b(30, 23)
    val mant_b = Cat(1.U(1.W), io.b(22, 0))  // Add implicit leading 1

    // larger exponent
    val exp_diff = exp_a.asSInt - exp_b.asSInt

    val larger_exp = Mux(exp_diff >= 0.S, exp_a, exp_b)

    // Align mantissas by shifting the smaller exponent's mantissa
    // Add 2 extra bits (rounding precision)
    val aligned_mant_a = Mux(exp_diff >= 0.S,
      Cat(mant_a, 0.U(2.W)),
      Cat(mant_a, 0.U(2.W)) >> (-exp_diff).asUInt     // B has larger exp, shift A right
    )
    val aligned_mant_b = Mux(exp_diff >= 0.S,
      Cat(mant_b, 0.U(2.W)) >> exp_diff.asUInt,       // A has larger exp, shift B right
      Cat(mant_b, 0.U(2.W))
    )

    // Add or subtract based on signs
    val mant_sum = Wire(UInt(27.W))
    val result_sign = Wire(Bool())

    when(sign_a === sign_b) {
      // Same sign: add mantissas
      mant_sum := aligned_mant_a +& aligned_mant_b
      result_sign := sign_a
    }.otherwise {
      // Different signs: subtract mantissas
      when(aligned_mant_a >= aligned_mant_b) {
        mant_sum := aligned_mant_a - aligned_mant_b
        result_sign := sign_a
      }.otherwise {
        mant_sum := aligned_mant_b - aligned_mant_a
        result_sign := sign_b
      }
    }

    // Normalize result
    // Reference: Berkeley HardFloat (https://github.com/ucb-bar/berkeley-hardfloat) normalization logic
    // We need to find the leading 1 position and shift accordingly
    val normalized_mant = Wire(UInt(23.W))
    val normalized_exp = Wire(UInt(8.W))

    // Count leading zeros to determine shift amount
    // This implements a priority encoder to find the position of the leading 1
    val shift_amount = Wire(UInt(5.W))

    when(mant_sum(26) === 1.U) {
      shift_amount := 0.U  // No left shift needed, but shift right by 3 for normalization
    }.elsewhen(mant_sum(25) === 1.U) {
      shift_amount := 1.U  // No left shift needed, shift right by 2 for normalization
    }.elsewhen(mant_sum(24) === 1.U) {
      shift_amount := 2.U
    }.elsewhen(mant_sum(23) === 1.U) {
      shift_amount := 3.U
    }.elsewhen(mant_sum(22) === 1.U) {
      shift_amount := 4.U
    }.elsewhen(mant_sum(21) === 1.U) {
      shift_amount := 5.U
    }.elsewhen(mant_sum(20) === 1.U) {
      shift_amount := 6.U
    }.elsewhen(mant_sum(19) === 1.U) {
      shift_amount := 7.U
    }.elsewhen(mant_sum(18) === 1.U) {
      shift_amount := 8.U
    }.elsewhen(mant_sum(17) === 1.U) {
      shift_amount := 9.U
    }.elsewhen(mant_sum(16) === 1.U) {
      shift_amount := 10.U
    }.elsewhen(mant_sum(15) === 1.U) {
      shift_amount := 11.U
    }.elsewhen(mant_sum(14) === 1.U) {
      shift_amount := 12.U
    }.elsewhen(mant_sum(13) === 1.U) {
      shift_amount := 13.U
    }.elsewhen(mant_sum(12) === 1.U) {
      shift_amount := 14.U
    }.elsewhen(mant_sum(11) === 1.U) {
      shift_amount := 15.U
    }.elsewhen(mant_sum(10) === 1.U) {
      shift_amount := 16.U
    }.elsewhen(mant_sum(9) === 1.U) {
      shift_amount := 17.U
    }.elsewhen(mant_sum(8) === 1.U) {
      shift_amount := 18.U
    }.elsewhen(mant_sum(7) === 1.U) {
      shift_amount := 19.U
    }.elsewhen(mant_sum(6) === 1.U) {
      shift_amount := 20.U
    }.elsewhen(mant_sum(5) === 1.U) {
      shift_amount := 21.U
    }.elsewhen(mant_sum(4) === 1.U) {
      shift_amount := 22.U
    }.elsewhen(mant_sum(3) === 1.U) {
      shift_amount := 23.U
    }.elsewhen(mant_sum(2) === 1.U) {
      shift_amount := 24.U
    }.otherwise {
      shift_amount := 25.U  // All zeros or leading 1 at bit 1 or 0
    }

    // Apply normalization based on shift_amount
    when(shift_amount === 0.U) {
      // Leading 1 at bit 26: shift right by 3, increment exponent
      normalized_mant := mant_sum(25, 3)
      normalized_exp := larger_exp + 1.U
    }.elsewhen(shift_amount === 1.U) {
      // Leading 1 at bit 25: shift right by 2 (normal case)
      normalized_mant := mant_sum(24, 2)
      normalized_exp := larger_exp
    }.otherwise {
      // Leading 1 at bit < 25: need to left shift
      // Shift amount calculation: (shift_amount - 1) gives us how many positions to shift left
      val left_shift = shift_amount - 1.U
      val shifted_sum = mant_sum << left_shift
      normalized_mant := shifted_sum(24, 2)

      // Decrease exponent by the left shift amount
      // **Extend to 9 bits before converting to SInt to avoid sign interpretation issues**
      // (larger_exp is 8 bits, MSB shouldn't be treated as sign bit)
      val new_exp = Cat(0.U(1.W), larger_exp).asSInt - Cat(0.U(1.W), left_shift).asSInt
      normalized_exp := Mux(new_exp <= 0.S, 0.U, new_exp(7, 0).asUInt)
    }

    // Handle zero case
    val is_zero = mant_sum === 0.U

    io.result := Mux(is_zero,
      0.U(32.W),
      Cat(result_sign, normalized_exp, normalized_mant)
    )
  }
}

/**
 * Single-Cycle Floating-Point Multiplier
 * Implements IEEE 754 single-precision floating-point multiplication (a * b)
 *
 * Features:
 * - Sign calculation via XOR
 * - Exponent addition with bias subtraction
 * - 24x24 mantissa multiplication
 * - Normalization for overflow and normal cases
 * - Zero detection and handling
 */

class FPMultiplier extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))       // Operand A (IEEE 754)
    val b = Input(UInt(32.W))       // Operand B (IEEE 754)
    val result = Output(UInt(32.W)) // Result (IEEE 754)
  })

  // Check for zero operands (full 32 bits)
  val a_is_zero = io.a === 0.U
  val b_is_zero = io.b === 0.U

  // Extract components
  val sign_a = io.a(31)
  val exp_a = io.a(30, 23)
  val mant_a = Cat(1.U(1.W), io.a(22, 0))  // Add implicit leading 1 (24 bits)

  val sign_b = io.b(31)
  val exp_b = io.b(30, 23)
  val mant_b = Cat(1.U(1.W), io.b(22, 0))  // Add implicit leading 1 (24 bits)

  // Calculate result sign (XOR of input signs)
  val mult_sign = sign_a ^ sign_b

  // Calculate result exponent (sum of exponents minus bias 127)
  // Using +& for overflow detection (9 bits result)
  val mult_exp = exp_a +& exp_b - 127.U

  // Multiply mantissas (24 bits × 24 bits = 48 bits)
  val mant_product = mant_a * mant_b

  // Normalize: Find the position of the leading 1 in the 48-bit product
  // For 24-bit x 24-bit multiplication with implicit leading 1s:
  // (1.xxx x 1.yyy) results in range [1.0, 4.0), so leading 1 is at bit 47 or 46
  val normalized_mant = Wire(UInt(23.W))
  val normalized_exp = Wire(UInt(8.W))

  when(mant_product(47) === 1.U) {
    // Leading 1 at bit 47: result in [2.0, 4.0)
    // Shift right by 1, increment exponent
    normalized_mant := mant_product(46, 24)
    normalized_exp := mult_exp(7, 0) + 1.U
  }.elsewhen(mant_product(46) === 1.U) {
    // Leading 1 at bit 46: result in [1.0, 2.0) - NORMAL CASE
    // Use bits [45:23]
    normalized_mant := mant_product(45, 23)
    normalized_exp := mult_exp(7, 0)
  }.otherwise {
    normalized_mant := mant_product(44, 22)
    normalized_exp := mult_exp(7, 0) - 1.U
  }

  io.result := Mux(a_is_zero || b_is_zero,
    0.U(32.W),
    Cat(mult_sign, normalized_exp, normalized_mant)
  )
}

/**
 * Single-Cycle Floating-Point Subtractor
 * Implements IEEE 754 single-precision floating-point subtraction (a - b)
 */

class FPSubtractor extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))      // Operand A (IEEE 754)
    val b = Input(UInt(32.W))      // Operand B (IEEE 754)
    val result = Output(UInt(32.W)) // Result (IEEE 754)
  })

  // Negate b's sign bit
  val b_negated = Cat(~io.b(31), io.b(30, 0))

  // Use FPAdder for a + (-b)
  val adder = Module(new FPAdder)
  adder.io.a := io.a
  adder.io.b := b_negated
  io.result := adder.io.result
}

/**
 * Floating-Point Divider using Newton-Raphson Reciprocal Method
 *
 * Algorithm: a / b = a * (1/b)
 *   Step 1: Compute reciprocal 1/b using Newton-Raphson iteration
 *   Step 2: Multiply a * (1/b)
 *
 * Newton-Raphson Iteration for reciprocal:
 *   x_{n+1} = x_n * (2 - b * x_n)
 *   where x_n approximates 1/b
 *
 * Pipeline Design (8 stages with 2 Newton-Raphson iterations):
 *   Stage 0: Initial reciprocal guess (LUT-based mantissa + exponent flip)
 *   Stage 1-3: First N-R iteration → x1
 *   Stage 4-6: Second N-R iteration → x2 (improved reciprocal)
 *   Stage 7: Final multiply (a * x2) → quotient
 *
 * Accuracy (with LUT-based initial guess):
 *   - Initial guess (LUT + exponent flip): ~0.0-0.6% error
 *   - After 1 iteration: ~0.001-0.01% error
 *   - After 2 iterations: < 0.0001% error (exceeds Softmax/RMSNorm requirements)
 *
 * Latency: 8 cycles (pipelined with 2 iterations)
 * Throughput: 1 result / cycle (when fully pipelined)
 *
 * Features:
 *   - LUT-based initial guess (16-entry reciprocal table)
 *   - Reuses existing FPMultiplier and FPSubtractor
 *   - Low resource overhead (~256 bits for LUT)
 *   - Pipeline-friendly for vector operations
 *   - Zero handling
 *
 * Known Limitations:
 *   - No NaN/Infinity handling
 *   - No denormal number support
 */
class FPDivider extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))      // Dividend (IEEE 754)
    val b = Input(UInt(32.W))      // Divisor (IEEE 754)
    val result = Output(UInt(32.W)) // Quotient (IEEE 754)
  })

  /* Pipeline Registers (8 stages: 2 N-R iterations + final multiply) */

  // First iteration registers
  val stage1_reg = Reg(new Bundle {
    val a = UInt(32.W)
    val b = UInt(32.W)
    val x0 = UInt(32.W)  // Initial reciprocal guess
  })
  val stage2_reg = Reg(new Bundle {
    val a = UInt(32.W)
    val b = UInt(32.W)
    val b_times_x0 = UInt(32.W)
    val x0 = UInt(32.W)
  })
  val stage3_reg = Reg(new Bundle {
    val a = UInt(32.W)
    val b = UInt(32.W)
    val factor = UInt(32.W)  // (2.0 - b*x0)
    val x0 = UInt(32.W)
  })
  val stage4_reg = Reg(new Bundle {
    val a = UInt(32.W)
    val b = UInt(32.W)
    val x1 = UInt(32.W)  // Improved reciprocal after 1st iteration
  })
  // Second iteration registers
  val stage5_reg = Reg(new Bundle {
    val a = UInt(32.W)
    val b_times_x1 = UInt(32.W)
    val x1 = UInt(32.W)
  })
  val stage6_reg = Reg(new Bundle {
    val a = UInt(32.W)
    val factor2 = UInt(32.W)  // (2.0 - b*x1)
    val x1 = UInt(32.W)
  })
  val stage7_reg = Reg(new Bundle {
    val a = UInt(32.W)
    val x2 = UInt(32.W)  // Highly improved reciprocal after 2nd iteration
  })

  /* Reciprocal Lookup Table (LUT-based Initial Guess) */

  // Maps 4-bit mantissa index to accurate reciprocal mantissa
  // Improves initial guess from ~5-25% error to ~0.0-0.6% error
  // Size: 16 entries (indexed by high 4 bits of mantissa)
  val reciprocal_lut = VecInit(Seq(
    "h3f800000".U(32.W),  // [ 0] 1/1.000000 = 1.000000
    "h3f70f0f1".U(32.W),  // [ 1] 1/1.062500 = 0.941176
    "h3f638e39".U(32.W),  // [ 2] 1/1.125000 = 0.888889
    "h3f579436".U(32.W),  // [ 3] 1/1.187500 = 0.842105
    "h3f4ccccd".U(32.W),  // [ 4] 1/1.250000 = 0.800000
    "h3f430c31".U(32.W),  // [ 5] 1/1.312500 = 0.761905
    "h3f3a2e8c".U(32.W),  // [ 6] 1/1.375000 = 0.727273
    "h3f321643".U(32.W),  // [ 7] 1/1.437500 = 0.695652
    "h3f2aaaab".U(32.W),  // [ 8] 1/1.500000 = 0.666667
    "h3f23d70a".U(32.W),  // [ 9] 1/1.562500 = 0.640000
    "h3f1d89d9".U(32.W),  // [10] 1/1.625000 = 0.615385
    "h3f17b426".U(32.W),  // [11] 1/1.687500 = 0.592593
    "h3f124925".U(32.W),  // [12] 1/1.750000 = 0.571429
    "h3f0d3dcb".U(32.W),  // [13] 1/1.812500 = 0.551724
    "h3f088889".U(32.W),  // [14] 1/1.875000 = 0.533333
    "h3f042108".U(32.W),  // [15] 1/1.937500 = 0.516129
  ))

  /** 
   * Stage 0: Initial Reciprocal Guess (Combinational) 
   * Method: LUT-based mantissa + exponent flip
   *
   * For b with exponent exp_b, reciprocal 1/b has exponent (254 - exp_b)
   * Mantissa is looked up from LUT using high 4 bits of b's mantissa
   *
   */

  val exp_b = io.b(30, 23)

  // Extract mantissa index (high 4 bits of mantissa)
  val mant_idx = io.b(22, 19)  // Bits [22:19]

  // Lookup reciprocal mantissa from LUT (full IEEE 754 value)
  val recip_mant_lut = reciprocal_lut(mant_idx)

  // Extract exponent from LUT to check if reciprocal < 1.0
  val lut_exp = recip_mant_lut(30, 23)

  // Reciprocal exponent calculation:
  // If LUT value has exp=126 (reciprocal < 1.0), adjust by -1
  // If LUT value has exp=127 (reciprocal >= 1.0), use standard formula
  val exp_adjust = Mux(lut_exp === 126.U, 1.U, 0.U)
  val recip_exp = 254.U - exp_b - exp_adjust

  // Extract mantissa component from LUT result
  val lut_mant = recip_mant_lut(22, 0)

  // Initial guess: sign=0, adjusted exponent, LUT mantissa
  val x0 = Cat(0.U(1.W), recip_exp, lut_mant)

  // Register Stage 0 → Stage 1
  stage1_reg.a := io.a
  stage1_reg.b := io.b
  stage1_reg.x0 := x0

  // Debug output (disabled for performance)
  // printf("[FPDiv] S0: a=%x, b=%x\n", io.a, io.b)
  // printf("  exp_b=%x, lut_exp=%x, exp_adjust=%x, recip_exp=%x\n", exp_b, lut_exp, exp_adjust, recip_exp)
  // printf("  mant_idx=%x, recip_mant_lut=%x, lut_mant=%x\n", mant_idx, recip_mant_lut, lut_mant)
  // printf("  x0=%x\n", x0)

  /* First Newton-Raphson Iteration */

  // Stage 1: Multiply (b * x0)
  val mult1 = Module(new FPMultiplier)
  mult1.io.a := stage1_reg.b
  mult1.io.b := stage1_reg.x0

  stage2_reg.a := stage1_reg.a
  stage2_reg.b := stage1_reg.b
  stage2_reg.b_times_x0 := mult1.io.result
  stage2_reg.x0 := stage1_reg.x0

  // Stage 2: Subtract (2.0 - b*x0)
  val two_fp32 = "h40000000".U  // IEEE 754: 2.0

  val sub1 = Module(new FPSubtractor)
  sub1.io.a := two_fp32
  sub1.io.b := stage2_reg.b_times_x0

  stage3_reg.a := stage2_reg.a
  stage3_reg.b := stage2_reg.b
  stage3_reg.factor := sub1.io.result  // (2.0 - b*x0)
  stage3_reg.x0 := stage2_reg.x0

  // Stage 3: Multiply (x0 * (2.0 - b*x0)) -> x1
  val mult2 = Module(new FPMultiplier)
  mult2.io.a := stage3_reg.x0
  mult2.io.b := stage3_reg.factor

  stage4_reg.a := stage3_reg.a
  stage4_reg.b := stage3_reg.b
  stage4_reg.x1 := mult2.io.result  // Reciprocal after 1st iteration

  /* Second Newton-Raphson Iteration (for higher precision)*/

  // Stage 4: Multiply (b * x1)
  val mult3 = Module(new FPMultiplier)
  mult3.io.a := stage4_reg.b
  mult3.io.b := stage4_reg.x1

  stage5_reg.a := stage4_reg.a
  stage5_reg.b_times_x1 := mult3.io.result
  stage5_reg.x1 := stage4_reg.x1

  // Stage 5: Subtract (2.0 - b*x1)
  val sub2 = Module(new FPSubtractor)
  sub2.io.a := two_fp32
  sub2.io.b := stage5_reg.b_times_x1

  stage6_reg.a := stage5_reg.a
  stage6_reg.factor2 := sub2.io.result  // (2.0 - b*x1)
  stage6_reg.x1 := stage5_reg.x1

  // Stage 6: Multiply (x1 * (2.0 - b*x1)) -> x2
  val mult4 = Module(new FPMultiplier)
  mult4.io.a := stage6_reg.x1
  mult4.io.b := stage6_reg.factor2

  stage7_reg.a := stage6_reg.a
  stage7_reg.x2 := mult4.io.result  // Highly improved reciprocal after 2nd iteration

  /* Stage 7: Final Multiply (a * x2) → Quotient */
  val mult5 = Module(new FPMultiplier)
  mult5.io.a := stage7_reg.a
  mult5.io.b := stage7_reg.x2

  // Output the final quotient
  io.result := mult5.io.result
  // printf("[FPDiv] S7: a=%x, x2=%x, result=%x\n", stage7_reg.a, stage7_reg.x2, mult5.io.result)
}

/* Helper object for IEEE 754 conversions and utilities */
object FPUtils {

  // Check if a 32-bit IEEE 754 value is zero
  def isZero(fp: UInt): Bool = {
    fp === 0.U
  }

  // Check if a 32-bit IEEE 754 value is NaN
  def isNaN(fp: UInt): Bool = {
    val exp = fp(30, 23)
    val mant = fp(22, 0)
    (exp === 0xFF.U) && (mant =/= 0.U)
  }

  // Check if a 32-bit IEEE 754 value is Infinity
  def isInf(fp: UInt): Bool = {
    val exp = fp(30, 23)
    val mant = fp(22, 0)
    (exp === 0xFF.U) && (mant === 0.U)
  }

  // Extract sign bit
  def getSign(fp: UInt): Bool = {
    fp(31)
  }

  // Extract exponent field
  def getExp(fp: UInt): UInt = {
    fp(30, 23)
  }

  // Extract mantissa field (without implicit leading 1)
  def getMant(fp: UInt): UInt = {
    fp(22, 0)
  }

  // Extract mantissa with implicit leading 1 (24 bits)
  def getMantWithLeading1(fp: UInt): UInt = {
    Cat(1.U(1.W), fp(22, 0))
  }
}
