// Two Instructions Test: VEXP + VRSQRT

package riscv

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.math.{exp, sqrt, abs}

/**
 * Two Sequential Instructions Test
 *
 * Tests: VEXP + VRSQRT
 * This isolates whether the problem occurs when executing two custom instructions sequentially.
 */
class TwoInstructionsTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Two Sequential Custom Instructions")

  def ieee754ToFloat(bits: BigInt): Float = {
    java.lang.Float.intBitsToFloat(bits.toInt)
  }

  def relativeError(actual: Float, expected: Float): Float = {
    if (expected != 0.0f) {
      abs((actual - expected) / expected)
    } else {
      abs(actual - expected)
    }
  }

  it should "execute VEXP and VRSQRT sequentially" in {
    test(new TestTopModule("two_inst_test.asmbin"))
      .withAnnotations(TestAnnotations.annos) { dut =>

      dut.clock.setTimeout(0)

      println("\n" + "="*80)
      println("Two Instructions Test: VEXP + VRSQRT")
      println("="*80)

      println("\n[Step 1] Loading program...")
      dut.clock.step(100)

      println("\n[Step 2] Executing two custom instructions...")
      dut.clock.step(3000)  // Enough cycles for both instructions

      // Debug: Register values
      println("\n[Debug] Register values:")
      val regNames = Seq(
        (10, "a0 (VEXP input)"),
        (11, "a1 (VEXP result)"),
        (12, "a2 (VRSQRT input)"),
        (13, "a3 (VRSQRT result)"),
        (5, "t0"),
        (6, "t1")
      )
      regNames.foreach { case (addr, name) =>
        dut.io.regs_debug_read_address.poke(addr.U)
        dut.clock.step(1)
        val value = dut.io.regs_debug_read_data.peekInt()
        println(f"  $name%-20s = 0x${value}%08X")
      }

      println("\n[Debug] Memory values:")
      Seq(0x2000, 0x2004, 0x2008).foreach { addr =>
        dut.io.mem_debug_read_address.poke(addr.U)
        dut.clock.step(1)
        val value = dut.io.mem_debug_read_data.peekInt()
        println(f"  Memory[0x${addr}%04X] = 0x${value}%08X")
      }

      // Test 1: VEXP(1.0)
      println("\n[Test 1] VEXP(1.0) → exp(1.0) ≈ 2.718")
      dut.io.mem_debug_read_address.poke(0x2000.U)
      dut.clock.step(1)
      val vexp_bits = dut.io.mem_debug_read_data.peekInt()
      val vexp_result = ieee754ToFloat(vexp_bits)
      val vexp_expected = exp(1.0).toFloat
      val vexp_error = relativeError(vexp_result, vexp_expected)

      println(f"  Result:   ${vexp_result}%.6f")
      println(f"  Expected: ${vexp_expected}%.6f")
      println(f"  Error:    ${vexp_error * 100}%.4f%%")

      // Test 2: VRSQRT(4.0)
      println("\n[Test 2] VRSQRT(4.0) → 1/sqrt(4.0) = 0.5")
      dut.io.mem_debug_read_address.poke(0x2004.U)
      dut.clock.step(1)
      val vrsqrt_bits = dut.io.mem_debug_read_data.peekInt()
      val vrsqrt_result = ieee754ToFloat(vrsqrt_bits)
      val vrsqrt_expected = (1.0 / sqrt(4.0)).toFloat
      val vrsqrt_error = relativeError(vrsqrt_result, vrsqrt_expected)

      println(f"  Result:   ${vrsqrt_result}%.6f")
      println(f"  Expected: ${vrsqrt_expected}%.6f")
      println(f"  Error:    ${vrsqrt_error * 100}%.4f%%")

      // Verify both results
      println("\n" + "="*80)

      if (vexp_error < 0.25 && vrsqrt_error < 0.01) {
        println("  BOTH TESTS PASSED")
      } else {
        println("  TEST FAILED")
        if (vexp_error >= 0.25) println(f"  VEXP error ${vexp_error*100}%.4f%% too high")
        if (vrsqrt_error >= 0.01) println(f"  VRSQRT error ${vrsqrt_error*100}%.4f%% too high")
      }
      println("="*80 + "\n")

      assert(vexp_error < 0.25,
        f"VEXP error ${vexp_error * 100}%.4f%% exceeds threshold")
      assert(vrsqrt_error < 0.01,
        f"VRSQRT error ${vrsqrt_error * 100}%.4f%% exceeds threshold")
    }
  }
}
