package sfu

import chisel3._
import chisel3.stage.ChiselStage

/**
 * Verilog Generator for SFU modules
 * Used for PPA (Power/Performance/Area) analysis
 */
object VerilogGenerator extends App {
  println("================================================================")
  println("           Generating Verilog for SFU Modules")
  println("================================================================")

  // Create output directory
  val outputDir = "generated/sfu"
  new java.io.File(outputDir).mkdirs()

  println("\n[1/6] Generating SpecialFunctionUnit.v...")
  (new ChiselStage).emitVerilog(
    new SpecialFunctionUnit(),
    args = Array("--target-dir", outputDir)
  )

  println("\n[2/6] Generating ExponentialApproximator.v...")
  (new ChiselStage).emitVerilog(
    new ExponentialApproximator(),
    args = Array("--target-dir", outputDir)
  )

  println("\n[3/6] Generating InvSqrt.v...")
  (new ChiselStage).emitVerilog(
    new InvSqrt(),
    args = Array("--target-dir", outputDir)
  )

  println("\n[4/6] Generating FPDivider.v...")
  (new ChiselStage).emitVerilog(
    new FPDivider(),
    args = Array("--target-dir", outputDir)
  )

  println("\n[5/6] Generating RMSNormAccelerator.v...")
  (new ChiselStage).emitVerilog(
    new RMSNormAccelerator(),
    args = Array("--target-dir", outputDir)
  )

  println("\n[6/6] Generating SoftmaxAccelerator.v...")
  (new ChiselStage).emitVerilog(
    new SoftmaxAccelerator(),
    args = Array("--target-dir", outputDir)
  )

  println("\n================================================================")
  println("           Verilog Generation Complete!")
  println("================================================================")
  println(s"\nGenerated files in: $outputDir/")
  println("Use these files for:")
  println("  - Yosys synthesis (area analysis)")
  println("  - Design Compiler (PPA analysis)")
  println("  - Verilator simulation")
  println("================================================================\n")
}
