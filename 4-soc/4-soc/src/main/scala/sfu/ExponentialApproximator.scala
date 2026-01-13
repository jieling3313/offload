// Exponential Approximation using Piecewise Linear Interpolation

package sfu

import chisel3._
import chisel3.util._

/**
 *
 * Algorithm: For segment i, exp(x) ≈ a_i * x + b_i
 *
 * Input range: [-10, 10] (values ​​outside the range will be saturated)
 *
 * 16 segments
 *
 * Latency: 5 cycles (pipelined)
 *
 * Pipeline Design (5-stage):
 *   Stage 1: Range check + Segment selection
 *   Stage 2: LUT lookup for coefficients
 *   Stage 3: FP Multiply - Sign & Exponent calculation
 *   Stage 4: FP Multiply - Mantissa multiplication & Normalization
 *   Stage 5: FP Addition (a*x + b)
 *
 * Precision: Maximum relative error ~23% (average ~5.5%)
 * IEEE 754 single-precision floating-point format
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

  val stage1_reg = Reg(new Bundle {
    val input = UInt(32.W)
    val segment_idx = UInt(4.W)
  })
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

  // Range check using IEEE 754 comparison
  // exp(x) for x > 10 saturates to large value (0x41200000 = +10.0)
  // exp(x) for x < -10 saturates to ~0 (0xC1200000 = -10.0)

  // Define boundary constants
  val pos_10 = "h41200000".U  // +10.0
  val neg_10 = "hC1200000".U  // -10.0

  // IEEE 754 comparison helper (defined below for segment selection)
  // Will be used here once defined

  val is_large_positive = sign === 0.U && (
    (exponent > "b10000010".U) ||  // exp > 130
    (exponent === "b10000010".U && mantissa >= "b001000000000000000000000".U)  // exp==130 and mant >= 0x200000 (i.e., >= 10.0)
  )

  val is_large_negative = sign === 1.U && (
    (exponent > "b10000010".U) ||  // exp > 130
    (exponent === "b10000010".U && mantissa >= "b001000000000000000000000".U)  // exp==130 and mant >= 0x200000 (i.e., <= -10.0)
  )

  // input to valid range
  val input_clamped = Wire(UInt(32.W))
  input_clamped := Mux(is_large_positive,
                      pos_10,  // +10.0
                      Mux(is_large_negative,
                          neg_10,  // -10.0
                          io.in))

  // Compute segment index (4 bits, 0-15)
  // Segment mapping: [-10, 10] → [0, 15]
  // Each segment covers: 20 / 16 = 1.25 units
  val segment_idx = Wire(UInt(4.W))

  // Define 15 boundary values (IEEE 754 format)
  // These boundaries separate the 16 segments
  val boundary_1  = "hC10C0000".U  //  -8.75  (between segment 0 and 1)
  val boundary_2  = "hC0F00000".U  //  -7.50  (between segment 1 and 2)
  val boundary_3  = "hC0C80000".U  //  -6.25  (between segment 2 and 3)
  val boundary_4  = "hC0A00000".U  //  -5.00  (between segment 3 and 4)
  val boundary_5  = "hC0700000".U  //  -3.75  (between segment 4 and 5)
  val boundary_6  = "hC0200000".U  //  -2.50  (between segment 5 and 6)
  val boundary_7  = "hBFA00000".U  //  -1.25  (between segment 6 and 7)
  val boundary_8  = "h00000000".U  //   0.00  (between segment 7 and 8)
  val boundary_9  = "h3FA00000".U  //   1.25  (between segment 8 and 9)
  val boundary_10 = "h40200000".U  //   2.50  (between segment 9 and 10)
  val boundary_11 = "h40700000".U  //   3.75  (between segment 10 and 11)
  val boundary_12 = "h40A00000".U  //   5.00  (between segment 11 and 12)
  val boundary_13 = "h40C80000".U  //   6.25  (between segment 12 and 13)
  val boundary_14 = "h40F00000".U  //   7.50  (between segment 13 and 14)
  val boundary_15 = "h410C0000".U  //   8.75  (between segment 14 and 15)

  // Compare input against boundaries
  // IEEE 754 comparison helper: a < b?
  // Implements IEEE 754 comparison with three cases:
  //   1. Different signs: negative (sign=1) < positive (sign=0)
  //   2. Both positive: compare as unsigned (normal ordering)
  //   3. Both negative: reverse comparison (more negative bits = smaller value)
  def fpLessThan(a: UInt, b: UInt): Bool = {
    val a_sign = a(31)
    val b_sign = b(31)

    Mux(a_sign =/= b_sign,
      a_sign.asBool,  // Different signs: a < b if a is negative (sign=1)
      Mux(a_sign === 0.U,
        a < b,   // Both positive: normal unsigned comparison
        a > b    // Both negative: reverse (ex: 0xC0A00000 < 0xC1200000 i.e. -5.0 > -10.0)
      )
    )
  }

  when(fpLessThan(input_clamped, boundary_1)) {
    segment_idx := 0.U  // [-10.00, -8.75)
  }.elsewhen(fpLessThan(input_clamped, boundary_2)) {
    segment_idx := 1.U  // [-8.75, -7.50)
  }.elsewhen(fpLessThan(input_clamped, boundary_3)) {
    segment_idx := 2.U  // [-7.50, -6.25)
  }.elsewhen(fpLessThan(input_clamped, boundary_4)) {
    segment_idx := 3.U  // [-6.25, -5.00)
  }.elsewhen(fpLessThan(input_clamped, boundary_5)) {
    segment_idx := 4.U  // [-5.00, -3.75)
  }.elsewhen(fpLessThan(input_clamped, boundary_6)) {
    segment_idx := 5.U  // [-3.75, -2.50)
  }.elsewhen(fpLessThan(input_clamped, boundary_7)) {
    segment_idx := 6.U  // [-2.50, -1.25)
  }.elsewhen(fpLessThan(input_clamped, boundary_8)) {
    segment_idx := 7.U  // [-1.25,  0.00)
  }.elsewhen(fpLessThan(input_clamped, boundary_9)) {
    segment_idx := 8.U  // [ 0.00,  1.25)
  }.elsewhen(fpLessThan(input_clamped, boundary_10)) {
    segment_idx := 9.U  // [ 1.25,  2.50)
  }.elsewhen(fpLessThan(input_clamped, boundary_11)) {
    segment_idx := 10.U // [ 2.50,  3.75)
  }.elsewhen(fpLessThan(input_clamped, boundary_12)) {
    segment_idx := 11.U // [ 3.75,  5.00)
  }.elsewhen(fpLessThan(input_clamped, boundary_13)) {
    segment_idx := 12.U // [ 5.00,  6.25)
  }.elsewhen(fpLessThan(input_clamped, boundary_14)) {
    segment_idx := 13.U // [ 6.25,  7.50)
  }.elsewhen(fpLessThan(input_clamped, boundary_15)) {
    segment_idx := 14.U // [ 7.50,  8.75)
  }.otherwise {
    segment_idx := 15.U // [ 8.75, 10.00]
  }

  // Store for next stage: input and segment index
  stage1_reg.input := input_clamped
  stage1_reg.segment_idx := segment_idx

  /* Stage 2: LUT Lookup for Coefficients */

  val segment_s2 = stage1_reg.segment_idx
  val input_s2 = stage1_reg.input

  // Lookup tables for piecewise linear coefficients
  // Format: For i, exp(x) ≈ a[i] * x + b[i]

  // **These coefficients are PLACEHOLDERS and need to be computed offline (compute_lut_coefficients.py)**

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

  // Check for zero operands (check full 32 bits, not just exponent)
  // Note: Only skip multiplication if operand is 0x00000000
  // exp=0, mant!=0 are rare in our range and can be handled normally
  val input_is_zero = input_s4 === 0.U
  val coeff_is_zero = coeff_a_s4 === 0.U

  // Extract mantissas (24 bits each with implicit leading 1)
  val mant_input_s4 = Cat(1.U(1.W), input_s4(22, 0))
  val mant_a_s4 = Cat(1.U(1.W), coeff_a_s4(22, 0))

  // Multiply mantissas (24 bits x 24 bits = 48 bits)
  val mant_product = mant_input_s4 * mant_a_s4

  // Normalize: Find the position of the leading 1 in the 48-bit product
  // For 24-bit x 24-bit multiplication with implicit leading 1s:
  // (1.xxx x 1.yyy) results in range [1.0, 4.0), so leading 1 is at bit 47 or 46
  val normalized_mant_s4 = Wire(UInt(23.W))
  val normalized_exp_s4 = Wire(UInt(8.W))

  when(mant_product(47) === 1.U) {
    // Leading 1 at bit 47: result in [2.0, 4.0)
    // Shift right by 1, increment exponent
    normalized_mant_s4 := mant_product(46, 24)
    normalized_exp_s4 := mult_exp_s4 + 1.U
  }.elsewhen(mant_product(46) === 1.U) {
    // Leading 1 at bit 46: result in [1.0, 2.0)
    // Normal case, use bits [45:23]
    normalized_mant_s4 := mant_product(45, 23)
    normalized_exp_s4 := mult_exp_s4
  }.otherwise {
    normalized_mant_s4 := mant_product(44, 22)
    normalized_exp_s4 := mult_exp_s4 - 1.U
  }
  val mult_result_s4 = Mux(input_is_zero || coeff_is_zero,
    0.U(32.W),
    Cat(mult_sign_s4, normalized_exp_s4, normalized_mant_s4)
  )

  // Store for next stage
  stage4_reg.mult_result := mult_result_s4
  stage4_reg.coeff_b := stage3_reg.coeff_b

  /* Stage 5: FP Addition (a*x + b) */

  val mult_result_s5 = stage4_reg.mult_result
  val coeff_b_s5 = stage4_reg.coeff_b

  // Instantiate FP adder for final addition
  val fp_adder = Module(new FPAdder)
  fp_adder.io.a := mult_result_s5
  fp_adder.io.b := coeff_b_s5

  val final_result = fp_adder.io.result

  stage5_reg := final_result

  /* Output Assignment */

  io.out := stage5_reg
  io.out_valid := valid5
  io.ready := true.B  // Always ready in this implementation
}