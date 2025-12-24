// Custom instructions for LLM acceleration (Softmax, RMSNorm)
package riscv.core

import chisel3._
import riscv.Parameters

/**
 * Custom instruction definitions for Transformer non-linear operations
 *
 * Opcode: 0101011 (custom-1 in RISC-V spec)
 * Format: R-type variant
 *
 * Encoding: [func7(7) | rs2(5) | rs1(5) | func3(3) | rd(5) | opcode(7)]
 */
object CustomInstructions {
  // Custom opcode in RISC-V custom-1 space
  val CUSTOM1_OPCODE = "b0101011".U(7.W)

  // Function codes (func7) for different operations
  object Func7 {
    val VEXP     = "b0000001".U(7.W)  // Vector exponential approximation
    val VRSQRT   = "b0000010".U(7.W)  // Fast inverse square root
    val VREDSUM  = "b0000011".U(7.W)  // Vector reduction sum
    val SOFTMAX  = "b0000100".U(7.W)  // Complete softmax operation
    val RMSNORM  = "b0000101".U(7.W)  // RMSNorm operation
    val VDOT     = "b0000110".U(7.W)  // Vector dot product (helper)
    val VSCALE   = "b0000111".U(7.W)  // Vector scaling (helper)
  }

  // Function3 codes (currently all use 000)
  object Func3 {
    val DEFAULT = "b000".U(3.W)
  }

  /**
   * Check if an instruction is a custom instruction
   */
  def isCustomInstruction(instruction: UInt): Bool = {
    instruction(6, 0) === CUSTOM1_OPCODE
  }

  /**
   * Extract func7 from instruction
   */
  def getFunc7(instruction: UInt): UInt = {
    instruction(31, 25)
  }

  /**
   * Check specific custom instruction types
   */
  def isVEXP(instruction: UInt): Bool = {
    isCustomInstruction(instruction) && getFunc7(instruction) === Func7.VEXP
  }

  def isVRSQRT(instruction: UInt): Bool = {
    isCustomInstruction(instruction) && getFunc7(instruction) === Func7.VRSQRT
  }

  def isVREDSUM(instruction: UInt): Bool = {
    isCustomInstruction(instruction) && getFunc7(instruction) === Func7.VREDSUM
  }

  def isSOFTMAX(instruction: UInt): Bool = {
    isCustomInstruction(instruction) && getFunc7(instruction) === Func7.SOFTMAX
  }

  def isRMSNORM(instruction: UInt): Bool = {
    isCustomInstruction(instruction) && getFunc7(instruction) === Func7.RMSNORM
  }

  def isVDOT(instruction: UInt): Bool = {
    isCustomInstruction(instruction) && getFunc7(instruction) === Func7.VDOT
  }

  def isVSCALE(instruction: UInt): Bool = {
    isCustomInstruction(instruction) && getFunc7(instruction) === Func7.VSCALE
  }
}

/**
 * Custom instruction types for control logic
 */
object CustomInstructionType {
  val SCALAR = 0.U(2.W)  // Scalar operation (VRSQRT on single value)
  val VECTOR = 1.U(2.W)  // Vector operation (multi-cycle)
  val REDUCE = 2.U(2.W)  // Reduction operation (accumulate)
}

/**
 * SFU operation codes (internal to SFU module)
 */
object SFUOp {
  val NOP     = 0.U(4.W)
  val EXP     = 1.U(4.W)
  val RSQRT   = 2.U(4.W)
  val SUM     = 3.U(4.W)
  val SOFTMAX = 4.U(4.W)
  val RMSNORM = 5.U(4.W)
  val DOT     = 6.U(4.W)
  val SCALE   = 7.U(4.W)
}

/**
 * Register write source extension for custom instructions
 */
object CustomRegWriteSource {
  val SFU = 3.U(2.W)  // Write back from SFU (extends RegWriteSource)
}
