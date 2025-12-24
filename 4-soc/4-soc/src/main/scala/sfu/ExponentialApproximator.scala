// Exponential Approximation using Piecewise Linear Interpolation

package sfu

import chisel3._
import chisel3.util._

/**
 * 使用分段線性方法的指數逼近器
 * 算法: 對於第 i 段, exp(x) ≈ a_i * x + b_i
 * 輸入範圍: [-10, 10] (超出範圍的值會飽和)
 * 段數: 16 段
 * Latency: 5 cycles (pipelined)
 *  Pipeline Design (5-stage):
 *    Stage 1: Range check + Segment selection
 *    Stage 2: LUT lookup for coefficients
 *    Stage 3: FP Multiply - Sign & Exponent calculation
 *    Stage 4: FP Multiply - Mantissa multiplication & Normalization
 *    Stage 5: FP Addition (a*x + b)
 * w/ IEEE 754 single-precision floating-point format
 */

class ExponentialApproximator extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(32.W))       // Input (IEEE 754 float)
    val out = Output(UInt(32.W))     // Output (IEEE 754 float)
    val valid = Input(Bool())        // Input valid
    val ready = Output(Bool())       // Ready for new input
    val out_valid = Output(Bool())   // Output valid
  })

  /* Pipeline Registers (5 stages)*/

  val stage1_reg = Reg(UInt(32.W))
  val stage2_reg = Reg(new Bundle {
    val input = UInt(32.W)
    val coeff_a = UInt(32.W)
    val coeff_b = UInt(32.W)
  })
  val stage3_reg = Reg(new Bundle {
    val input = UInt(32.W)
    val coeff_a = UInt(32.W)
    val coeff_b = UInt(32.W)
    val mult_sign = Bool()
    val mult_exp = UInt(8.W)
  })
  val stage4_reg = Reg(new Bundle {
    val mult_result = UInt(32.W)
    val coeff_b = UInt(32.W)
  })
  val stage5_reg = Reg(UInt(32.W))

  // Valid signals for each stage
  val valid1 = RegNext(io.valid, false.B)
  val valid2 = RegNext(valid1, false.B)
  val valid3 = RegNext(valid2, false.B)
  val valid4 = RegNext(valid3, false.B)
  val valid5 = RegNext(valid4, false.B)

  /* Stage 1: Range Check and Segment Selection */

  // Extract IEEE 754 components
  val sign = io.in(31)
  val exponent = io.in(30, 23)
  val mantissa = io.in(22, 0)

  // Range check based on exponent
  // exp(x) for x > 10 saturates to large value (0x41200000 = +10.0)
  // exp(x) for x < -10 saturates to ~0 (0xC1200000 = -10.0)
  val is_large_positive = exponent >= "b10000010".U  // > ~10
  val is_large_negative = sign === 1.U && exponent >= "b10000010".U  // < -10

  // input to valid range
  val input_clamped = Wire(UInt(32.W))
  input_clamped := Mux(is_large_positive,
                      "h41200000".U,  // +10.0 in IEEE 754
                      Mux(is_large_negative,
                          "hC1200000".U,  // -10.0 in IEEE 754
                          io.in))

  // Compute segment index (4 bits, 0-15)
  // Segment mapping: [-10, 10] → [0, 15]
  // Each segment covers: 20 / 16 = 1.25 units
  val segment_idx = Wire(UInt(4.W))

  // Extract biased exponent (remove IEEE 754 bias of 127)
  val exp_biased = exponent.asSInt - 127.S

  // Simplified segment selection based on exponent
  val segment_approx = Mux(sign === 0.U,
    // Positive numbers: map to upper segments (8-15)
    Mux(exp_biased >= 0.S,
      (8.U + exp_biased.asUInt)(3, 0),
      8.U),
    // Negative numbers: map to lower segments (0-7)
    Mux(exp_biased >= 0.S,
      (7.U - exp_biased.asUInt)(3, 0),
      0.U)
  )

  segment_idx := Mux(segment_approx > 15.U, 15.U, segment_approx)

  // Store for next stage: input and segment index
  stage1_reg := Cat(segment_idx, input_clamped(27, 0))

  /* Stage 2: LUT Lookup for Coefficients */
  
  val segment_s2 = stage1_reg(31, 28)
  val input_s2 = Cat(0.U(4.W), stage1_reg(27, 0))

  // Lookup tables for piecewise linear coefficients
  // Format: For i, exp(x) ≈ a[i] * x + b[i]

  // **These coefficients are PLACEHOLDERS and need to be computed offline (compute_lut_coefficients.py)**
  // for optimal accuracy across the input range.
  // Coefficient computation method:
  // For each segment [x_min, x_max]:
  //   1. Sample points at x_min and x_max
  //   2. Fit linear function: y = a*x + b
  //   3. Minimize max error over segment

  val lut_a = VecInit(Seq(
    // Segments for negative values (exp approaches 0)
    "h38B910EA".U,  // Segment  0: [-10.00,  -8.75), a ≈ 8.8246e-05
    "h39A17C6B".U,  // Segment  1: [ -8.75,  -7.50), a ≈ 3.0801e-04
    "h3A8CE90F".U,  // Segment  2: [ -7.50,  -6.25), a ≈ 1.0751e-03
    "h3B75E9AD".U,  // Segment  3: [ -6.25,  -5.00), a ≈ 3.7523e-03
    "h3C56947B".U,  // Segment  4: [ -5.00,  -3.75), a ≈ 1.3097e-02
    "h3D3B3D4C".U,  // Segment  5: [ -3.75,  -2.50), a ≈ 4.5713e-02
    "h3E2361E9".U,  // Segment  6: [ -2.50,  -1.25), a ≈ 1.5955e-01
    "h3F0E90B2".U,  // Segment  7: [ -1.25,   0.00), a ≈ 5.5690e-01

    // Segments for positive values (exp grows)
    "h3FF8CCFD".U,  // Segment  8: [  0.00,   1.25), a ≈ 1.9438e+00
    "h40D91998".U,  // Segment  9: [  1.25,   2.50), a ≈ 6.7844e+00
    "h41BD7037".U,  // Segment 10: [  2.50,   3.75), a ≈ 2.3680e+01
    "h42A54D1B".U,  // Segment 11: [  3.75,   5.00), a ≈ 8.2651e+01
    "h43903D4E".U,  // Segment 12: [  5.00,   6.25), a ≈ 2.8848e+02
    "h447BB8FD".U,  // Segment 13: [  6.25,   7.50), a ≈ 1.0069e+03
    "h455BA649".U,  // Segment 14: [  7.50,   8.75), a ≈ 3.5144e+03
    "h463FA9BF".U   // Segment 15: [  8.75,  10.00), a ≈ 1.2266e+04
  ))

  val lut_b = VecInit(Seq(
    // intercept of each segment
    "h3A709D8B".U,  // Segment  0: [-10.00,  -8.75), b ≈ 9.1787e-04
    "h3B38B9B2".U,  // Segment  1: [ -8.75,  -7.50), b ≈ 2.8187e-03
    "h3C0B2BE6".U,  // Segment  2: [ -7.50,  -6.25), b ≈ 8.4944e-03
    "h3CCC7448".U,  // Segment  3: [ -6.25,  -5.00), b ≈ 2.4958e-02
    "h3D90E02F".U,  // Segment  4: [ -5.00,  -3.75), b ≈ 7.0740e-02
    "h3E425216".U,  // Segment  5: [ -3.75,  -2.50), b ≈ 1.8977e-01
    "h3EED0241".U,  // Segment  6: [ -2.50,  -1.25), b ≈ 4.6291e-01
    "h3F6B6A1C".U,  // Segment  7: [ -1.25,   0.00), b ≈ 9.1959e-01
    "h3F47ACE9".U,  // Segment  8: [  0.00,   1.25), b ≈ 7.7998e-01
    "hC0B8420D".U,  // Segment  9: [  1.25,   2.50), b ≈ -5.7581e+00
    "hC246CA17".U,  // Segment 10: [  2.50,   3.75), b ≈ -4.9697e+01
    "hC38A6314".U,  // Segment 11: [  3.75,   5.00), b ≈ -2.7677e+02
    "hC4A5D452".U,  // Segment 12: [  5.00,   6.25), b ≈ -1.3266e+03
    "hC5B80832".U,  // Segment 13: [  6.25,   7.50), b ≈ -5.8890e+03
    "hC6C2E769".U,  // Segment 14: [  7.50,   8.75), b ≈ -2.4948e+04
    "hC7C8048C".U   // Segment 15: [  8.75,  10.00), b ≈ -1.0241e+05
  ))

  val coeff_a_s2 = lut_a(segment_s2)
  val coeff_b_s2 = lut_b(segment_s2)

  // Store for next stage
  stage2_reg.input := input_s2
  stage2_reg.coeff_a := coeff_a_s2
  stage2_reg.coeff_b := coeff_b_s2


  /* Stage 3: FP Multiply - Sign & Exponent Calculation */

  // Extract operands
  val input_s3 = stage2_reg.input
  val coeff_a_s3 = stage2_reg.coeff_a

  val sign_input = input_s3(31)
  val exp_input = input_s3(30, 23)
  val mant_input = Cat(1.U(1.W), input_s3(22, 0))  // Add implicit leading 1

  val sign_a = coeff_a_s3(31)
  val exp_a = coeff_a_s3(30, 23)
  val mant_a = Cat(1.U(1.W), coeff_a_s3(22, 0))

  // Calculate result sign (XOR of input signs)
  val mult_sign_s3 = sign_input ^ sign_a

  // Calculate result exponent (sum of exponents minus bias)
  // exp_result = exp_a + exp_input - 127
  val mult_exp_s3 = exp_a +& exp_input - 127.U

  // Store for next stage
  stage3_reg.input := input_s3
  stage3_reg.coeff_a := coeff_a_s3
  stage3_reg.coeff_b := stage2_reg.coeff_b
  stage3_reg.mult_sign := mult_sign_s3
  stage3_reg.mult_exp := mult_exp_s3(7, 0)


  /* Stage 4: FP Multiply - Mantissa Multiplication & Normalization */

  val input_s4 = stage3_reg.input
  val coeff_a_s4 = stage3_reg.coeff_a
  val mult_sign_s4 = stage3_reg.mult_sign
  val mult_exp_s4 = stage3_reg.mult_exp

  // Extract mantissas (24 bits each with implicit leading 1)
  val mant_input_s4 = Cat(1.U(1.W), input_s4(22, 0))
  val mant_a_s4 = Cat(1.U(1.W), coeff_a_s4(22, 0))

  // Multiply mantissas (24 bits × 24 bits = 48 bits)
  val mant_product = mant_input_s4 * mant_a_s4

  // Normalize: check if bit 47 is set (result >= 2.0)
  val normalized_mant_s4 = Wire(UInt(23.W))
  val normalized_exp_s4 = Wire(UInt(8.W))

  when(mant_product(47) === 1.U) {
    // Overflow: shift right by 1, increment exponent
    normalized_mant_s4 := mant_product(46, 24)
    normalized_exp_s4 := mult_exp_s4 + 1.U
  }.otherwise {
    // Normal: use bits [45:23]
    normalized_mant_s4 := mant_product(45, 23)
    normalized_exp_s4 := mult_exp_s4
  }

  // Assemble multiplication result
  val mult_result_s4 = Cat(mult_sign_s4, normalized_exp_s4, normalized_mant_s4)

  // Store for next stage
  stage4_reg.mult_result := mult_result_s4
  stage4_reg.coeff_b := stage3_reg.coeff_b

  /* Stage 5: FP Addition (a*x + b) */

  val mult_result_s5 = stage4_reg.mult_result
  val coeff_b_s5 = stage4_reg.coeff_b

  // Instantiate FP adder for final addition
  val fp_adder = Module(new FPAdderSingleCycle)
  fp_adder.io.a := mult_result_s5
  fp_adder.io.b := coeff_b_s5

  val final_result = fp_adder.io.result

  stage5_reg := final_result

  /* Output Assignment */

  io.out := stage5_reg
  io.out_valid := valid5
  io.ready := true.B  // Always ready in this implementation
}

/**
 * Single-Cycle Floating-Point Adder
 * This is a simplified FP adder that completes in one cycle.
 * For production use, this should be replaced with a fully
 * compliant IEEE 754 adder with proper rounding and special case handling.
 */
class FPAdderSingleCycle extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
    val result = Output(UInt(32.W))
  })

  // Extract components from operand A
  val sign_a = io.a(31)
  val exp_a = io.a(30, 23)
  val mant_a = Cat(1.U(1.W), io.a(22, 0))

  // Extract components from operand B
  val sign_b = io.b(31)
  val exp_b = io.b(30, 23)
  val mant_b = Cat(1.U(1.W), io.b(22, 0))

  // Determine which operand has larger exponent
  val exp_diff = exp_a.asSInt - exp_b.asSInt

  val larger_exp = Mux(exp_diff >= 0.S, exp_a, exp_b)
  val aligned_mant_a = Mux(exp_diff >= 0.S,
    Cat(mant_a, 0.U(2.W)),
    Cat(mant_a, 0.U(2.W)) >> (-exp_diff).asUInt
  )
  val aligned_mant_b = Mux(exp_diff >= 0.S,
    Cat(mant_b, 0.U(2.W)) >> exp_diff.asUInt,
    Cat(mant_b, 0.U(2.W))
  )

  // Add or subtract based on signs
  val mant_sum = Wire(UInt(27.W))
  val result_sign = Wire(Bool())

  when(sign_a === sign_b) {
    // Same sign: add
    mant_sum := aligned_mant_a +& aligned_mant_b
    result_sign := sign_a
  }.otherwise {
    // Different signs: subtract
    when(aligned_mant_a >= aligned_mant_b) {
      mant_sum := aligned_mant_a - aligned_mant_b
      result_sign := sign_a
    }.otherwise {
      mant_sum := aligned_mant_b - aligned_mant_a
      result_sign := sign_b
    }
  }

  // Normalize result
  val normalized_mant = Wire(UInt(23.W))
  val normalized_exp = Wire(UInt(8.W))

  when(mant_sum(26) === 1.U) {
    // Overflow: shift right
    normalized_mant := mant_sum(25, 3)
    normalized_exp := larger_exp + 1.U
  }.elsewhen(mant_sum(25) === 1.U) {
    // Normal
    normalized_mant := mant_sum(24, 2)
    normalized_exp := larger_exp
  }.otherwise {
    // Underflow or zero: simplified handling
    normalized_mant := mant_sum(22, 0)
    normalized_exp := larger_exp
  }

  // Handle zero case
  val is_zero = mant_sum === 0.U

  io.result := Mux(is_zero,
    0.U(32.W),
    Cat(result_sign, normalized_exp, normalized_mant)
  )
}