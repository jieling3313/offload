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
 * TODO:Floating-Point Divider (Placeholder)
 *
 * 
 */
class FPDivider extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))      // Dividend (IEEE 754)
    val b = Input(UInt(32.W))      // Divisor (IEEE 754)
    val result = Output(UInt(32.W)) // Quotient (IEEE 754)
  })
  io.result := io.a
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
