// Single VEXP Instruction Test - Simplified for debugging

package riscv

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.math.{exp, abs}

/**
 * Simplified Single Instruction Test
 *
 * This test focuses on a SINGLE VEXP instruction to isolate and debug
 * the pipeline timing and result capture issues.
 */
class SingleVexpTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Single VEXP Instruction")

  // Convert IEEE 754 bits to Float

  def ieee754ToFloat(bits: BigInt): Float = {
    java.lang.Float.intBitsToFloat(bits.toInt)
  }

  // Calculate relative error

  def relativeError(actual: Float, expected: Float): Float = {
    if (expected != 0.0f) {
      abs((actual - expected) / expected)
    } else {
      abs(actual - expected)
    }
  }

  it should "execute single VEXP instruction correctly" in {
    test(new TestTopModule("single_vexp_test.asmbin"))
      .withAnnotations(TestAnnotations.annos) { dut =>

      dut.clock.setTimeout(0)

      println("\n" + "="*80)
      println("Single VEXP Instruction Test")
      println("="*80)

      // Wait for ROM loading and execution
      println("\n[Step 1] Loading and executing program...")
      dut.clock.step(100)  // ROM loading

      println("\n[Step 2] Running VEXP instruction...")
      dut.clock.step(10000)  // Execute VEXP and store (increased for SFU latency)

      // Debug: Check registers
      println("\n[Debug] Register values:")
      val regNames = Seq(
        (10, "a0 (input)"),
        (11, "a1 (result)"),
        (5, "t0"),
        (6, "t1")
      )
      regNames.foreach { case (addr, name) =>
        dut.io.regs_debug_read_address.poke(addr.U)
        dut.clock.step(1)
        val value = dut.io.regs_debug_read_data.peekInt()
        println(f"  Register $name%-15s = 0x${value}%08X")
      }

      // Debug: Check memory
      println("\n[Debug] Memory values:")
      Seq(0x2000, 0x2004).foreach { addr =>
        dut.io.mem_debug_read_address.poke(addr.U)
        dut.clock.step(1)
        val value = dut.io.mem_debug_read_data.peekInt()
        println(f"  Memory[0x${addr}%04X] = 0x${value}%08X")
      }

      // Verify VEXP(1.0) result
      println("\n[Test] VEXP(1.0) → exp(1.0) ≈ 2.718")

      dut.io.mem_debug_read_address.poke(0x2000.U)
      dut.clock.step(1)
      val result_bits = dut.io.mem_debug_read_data.peekInt()
      val result = ieee754ToFloat(result_bits)
      val expected = exp(1.0).toFloat
      val error = relativeError(result, expected)

      println(f"  Memory[0x2000] = 0x${result_bits}%08X")
      println(f"  Result:        ${result}%.6f")
      println(f"  Expected:      ${expected}%.6f")
      println(f"  Error:         ${error * 100}%.4f%%")

      // Check completion marker
      dut.io.mem_debug_read_address.poke(0x2004.U)
      dut.clock.step(1)
      val marker = dut.io.mem_debug_read_data.peekInt()
      println(f"\n  Completion marker: 0x${marker}%08X")

      // Verify result (allow up to 25% error for exponential approximation)
      assert(error < 0.25,
        f"VEXP(1.0) error ${error * 100}%.4f%% exceeds 25%% threshold")

      println("\n" + "="*80)
      println("SINGLE VEXP TEST PASSED")
      println("="*80 + "\n")
    }
  }
}
