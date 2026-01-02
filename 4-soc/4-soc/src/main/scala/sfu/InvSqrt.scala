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
 * - Magic constant only: ~3-4% error
 * - 1 iteration: ~0.5-1% error
 * - 2 iterations: ~0.1% error
 *
 * Pipeline Latency: 11 cycles (was 3 cycles with placeholders)
 * - Stage 0: Magic constant y0
 * - Stages 1-5: First Newton-Raphson iteration
 * - Stages 6-10: Second Newton-Raphson iteration
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

  // Pipeline stages (11 stages total for 2 Newton-Raphson iterations)
  val stage1_reg = Reg(UInt(32.W))
  val stage2_reg = Reg(UInt(32.W))
  val stage3_reg = Reg(UInt(32.W))
  val stage4_reg = Reg(UInt(32.W))
  val stage5_reg = Reg(UInt(32.W))
  val stage6_reg = Reg(UInt(32.W))
  val stage7_reg = Reg(UInt(32.W))
  val stage8_reg = Reg(UInt(32.W))
  val stage9_reg = Reg(UInt(32.W))
  val stage10_reg = Reg(UInt(32.W))
  val stage11_reg = Reg(UInt(32.W))

  // Valid signal pipeline
  val valid1 = RegNext(io.valid, false.B)
  val valid2 = RegNext(valid1, false.B)
  val valid3 = RegNext(valid2, false.B)
  val valid4 = RegNext(valid3, false.B)
  val valid5 = RegNext(valid4, false.B)
  val valid6 = RegNext(valid5, false.B)
  val valid7 = RegNext(valid6, false.B)
  val valid8 = RegNext(valid7, false.B)
  val valid9 = RegNext(valid8, false.B)
  val valid10 = RegNext(valid9, false.B)
  val valid11 = RegNext(valid10, false.B)

  // Input value pipeline
  val x_s1 = RegNext(io.in)
  val x_s2 = RegNext(x_s1)
  val x_s3 = RegNext(x_s2)
  val x_s4 = RegNext(x_s3)
  val x_s5 = RegNext(x_s4)
  val x_s6 = RegNext(x_s5)
  val x_s7 = RegNext(x_s6)
  val x_s8 = RegNext(x_s7)
  val x_s9 = RegNext(x_s8)
  val x_s10 = RegNext(x_s9)

  // Constants
  val MAGIC_CONSTANT = "h5f3759df".U(32.W)  // Quake III magic number
  val THREE_HALVES = "h3fc00000".U(32.W)    // 1.5 in IEEE 754
  val HALF = "h3f000000".U(32.W)            // 0.5 in IEEE 754

  /* Stage 0: Magic constant initial guess */

  val input_bits = io.in
  val half_input = input_bits >> 1  // Logical shift right

  // Initial approximation using magic constant
  val y0 = MAGIC_CONSTANT - half_input

  stage1_reg := y0

  /* First Newton-Raphson Iteration: y1 = y0 * (1.5 - 0.5 * x * y0²) */

  // Stage 1: Compute y0_sq
  val mult_y0_sq = Module(new FPMultiplier)
  mult_y0_sq.io.a := stage1_reg
  mult_y0_sq.io.b := stage1_reg
  val y0_squared = mult_y0_sq.io.result
  stage2_reg := y0_squared

  // Stage 2: Compute x * y0²
  // At this stage, stage2_reg has y0² from input at cycle N
  // We need x from the same input, which is x delayed by 2 cycles
  val mult_x_y0sq = Module(new FPMultiplier)
  mult_x_y0sq.io.a := x_s2  // Fixed: was x_s1, should be x_s2 to align with stage2_reg
  mult_x_y0sq.io.b := stage2_reg
  val x_y0sq = mult_x_y0sq.io.result
  stage3_reg := x_y0sq

  // Stage 3: Compute 0.5 * x * y0²
  val mult_half = Module(new FPMultiplier)
  mult_half.io.a := HALF
  mult_half.io.b := stage3_reg
  val half_x_y0sq = mult_half.io.result
  stage4_reg := half_x_y0sq

  // Stage 4: Compute 1.5 - 0.5 * x * y0²
  val sub = Module(new FPSubtractor)
  sub.io.a := THREE_HALVES
  sub.io.b := stage4_reg
  val factor = sub.io.result
  stage5_reg := factor

  // Stage 5: Compute y1 = y0 * (1.5 - 0.5 * x * y0²)
  // At this stage, stage5_reg has factor from input at cycle N
  // Need y0 from the same input = stage1_reg delayed by 4 cycles
  val y0_s5 = RegNext(RegNext(RegNext(RegNext(stage1_reg))))
  val mult_y1 = Module(new FPMultiplier)
  mult_y1.io.a := y0_s5
  mult_y1.io.b := stage5_reg
  val y1 = mult_y1.io.result
  stage6_reg := y1

  /* Second Newton-Raphson Iteration: y2 = y1 * (1.5 - 0.5 * x * y1²) */

  // Stage 6: Compute y1²
  val mult_y1_sq = Module(new FPMultiplier)
  mult_y1_sq.io.a := stage6_reg
  mult_y1_sq.io.b := stage6_reg
  val y1_squared = mult_y1_sq.io.result
  stage7_reg := y1_squared

  // Stage 7: Compute x * y1²
  // At this stage, stage7_reg has y1² from input at cycle N
  // Need x from the same input = x delayed by 7 cycles
  val mult_x_y1sq = Module(new FPMultiplier)
  mult_x_y1sq.io.a := x_s7
  mult_x_y1sq.io.b := stage7_reg
  val x_y1sq = mult_x_y1sq.io.result
  stage8_reg := x_y1sq

  // Stage 8: Compute 0.5 * x * y1²
  val mult_half2 = Module(new FPMultiplier)
  mult_half2.io.a := HALF
  mult_half2.io.b := stage8_reg
  val half_x_y1sq = mult_half2.io.result
  stage9_reg := half_x_y1sq

  // Stage 9: Compute 1.5 - 0.5 * x * y1²
  val sub2 = Module(new FPSubtractor)
  sub2.io.a := THREE_HALVES
  sub2.io.b := stage9_reg
  val factor2 = sub2.io.result
  stage10_reg := factor2

  // Stage 10: Compute y2 = y1 * (1.5 - 0.5 * x * y1²)
  // At this stage, stage10_reg has factor2 from input at cycle N
  // Need y1 from the same input = stage6_reg delayed by 4 cycles
  val y1_s10 = RegNext(RegNext(RegNext(RegNext(stage6_reg))))
  val mult_y2 = Module(new FPMultiplier)
  mult_y2.io.a := y1_s10
  mult_y2.io.b := stage10_reg
  val y2 = mult_y2.io.result
  stage11_reg := y2

  /* Output */

  io.out := stage11_reg
  io.out_valid := valid11
  io.ready := true.B  // Always ready in this implementation
}
