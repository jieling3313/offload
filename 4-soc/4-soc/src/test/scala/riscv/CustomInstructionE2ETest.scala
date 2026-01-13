// End-to-End Test for Custom SFU Instructions

package riscv

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.math.{exp, sqrt, abs}

/**
 * End-to-End CPU Test for Custom Instructions
 *
 * This test verifies that custom SFU instructions work correctly
 * in the complete CPU pipeline, including:
 *
 * 1. Instruction decoding
 * 2. SFU operation execution
 * 3. Result writeback to registers
 * 4. Memory store operations
 * 5. Pipeline control and hazard handling
 */
class CustomInstructionE2ETest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Custom SFU Instructions E2E")

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

  it should "execute custom SFU instructions end-to-end" in {
    test(new TestTopModule("custom_inst_test.asmbin"))
      .withAnnotations(TestAnnotations.annos) { dut =>

      dut.clock.setTimeout(0) // Disable timeout for long-running test

      println("\n" + "="*80)
      println("Custom SFU Instructions End-to-End Test")
      println("="*80)


      /* Step 1: Wait for ROM loading and program execution */
      println("\n[Step 1] Loading ROM and running test program...")

      // Wait for ROM loading
      // TestTopModule has 4:1 clock divider, so need 4x cycles
      // ROM loading: ~19 instructions = ~76 test bench cycles
      println("\n[Debug] Waiting for ROM loading...")
      dut.clock.step(100)

      // Debug: Check if instructions are loaded at 0x1000
      println("\n[Debug] Checking ROM content at 0x1000...")
      dut.io.mem_debug_read_address.poke(0x1000.U)
      dut.clock.step(1)
      val inst0 = dut.io.mem_debug_read_data.peekInt()
      println(f"  Memory[0x1000] = 0x${inst0}%08X (expected: 0x3F800537 - lui a0, 0x3F800)")

      dut.io.mem_debug_read_address.poke(0x1004.U)
      dut.clock.step(1)
      val inst1 = dut.io.mem_debug_read_data.peekInt()
      println(f"  Memory[0x1004] = 0x${inst1}%08X (expected: 0x020505DB - VEXP a1, a0)")

      // Run for enough cycles to complete all operations
      // With 4:1 clock divider:
      // - Pipeline fill: 5 CPU cycles = 20 test cycles
      // - ~16 instructions with SFU operations
      // - VEXP: 5 cycles latency each = 20 test cycles
      // - VRSQRT: 11 cycles latency each = 44 test cycles
      // - Plus pipeline stalls when SFU is busy
      // Total estimate: ~500 CPU cycles = 2000 test bench cycles
      // But with stalls, may need much more. Try 20000 cycles.
      println("\n[Debug] Running program...")
      dut.clock.step(20000)  // Significantly increased to handle stalls

      // Debug: Check ALL relevant registers
      println("\n[Debug] Checking all relevant register values...")
      val regNames = Seq(
        (10, "a0 (x10)"), (11, "a1 (x11)"), (12, "a2 (x12)"), (13, "a3 (x13)"),
        (14, "a4 (x14)"), (15, "a5 (x15)"), (16, "a6 (x16)"), (17, "a7 (x17)"),
        (5, "t0 (x5)"), (6, "t1 (x6)")
      )
      regNames.foreach { case (addr, name) =>
        dut.io.regs_debug_read_address.poke(addr.U)
        dut.clock.step(1)
        val value = dut.io.regs_debug_read_data.peekInt()
        println(f"  Register $name%-12s = 0x${value}%08X")
      }

      // Debug: Check memory at instruction addresses
      println("\n[Debug] Checking memory at program addresses...")
      Seq(0x2000, 0x2004, 0x2008, 0x200C, 0x2010).foreach { addr =>
        dut.io.mem_debug_read_address.poke(addr.U)
        dut.clock.step(1)
        val value = dut.io.mem_debug_read_data.peekInt()
        println(f"  Memory[0x${addr}%04X] = 0x${value}%08X")
      }

      println("Program execution phase completed")

      /* Step 2: Verify Test 1 - VEXP(1.0) */
      println("\n[Test 1] VEXP(1.0) → exp(1.0) ≈ 2.718")

      dut.io.mem_debug_read_address.poke(0x2000.U)
      dut.clock.step(1)
      val exp1_bits = dut.io.mem_debug_read_data.peekInt()
      val exp1_result = ieee754ToFloat(exp1_bits)
      val exp1_expected = exp(1.0).toFloat
      val exp1_error = relativeError(exp1_result, exp1_expected)

      println(f"  Memory[0x2000] = 0x${exp1_bits}%08X")
      println(f"  Result:   ${exp1_result}%.6f")
      println(f"  Expected: ${exp1_expected}%.6f")
      println(f"  Error:    ${exp1_error * 100}%.4f%%")

      // ExponentialApproximator ~23% max error
      assert(exp1_error < 0.25,
        f"VEXP(1.0) error ${exp1_error * 100}%.4f%% exceeds 25%% threshold")
      println("  PASSED")

      /* Step 3: Verify Test 2 - VRSQRT(4.0) */
      println("\n[Test 2] VRSQRT(4.0) → 1/sqrt(4.0) = 0.5")

      dut.io.mem_debug_read_address.poke(0x2004.U)
      dut.clock.step(1)
      val rsqrt1_bits = dut.io.mem_debug_read_data.peekInt()
      val rsqrt1_result = ieee754ToFloat(rsqrt1_bits)
      val rsqrt1_expected = (1.0 / sqrt(4.0)).toFloat
      val rsqrt1_error = relativeError(rsqrt1_result, rsqrt1_expected)

      println(f"  Memory[0x2004] = 0x${rsqrt1_bits}%08X")
      println(f"  Result:   ${rsqrt1_result}%.6f")
      println(f"  Expected: ${rsqrt1_expected}%.6f")
      println(f"  Error:    ${rsqrt1_error * 100}%.6f%%")

      assert(rsqrt1_error < 0.01,
        f"VRSQRT(4.0) error ${rsqrt1_error * 100}%.6f%% exceeds 1%% threshold")
      println("   PASSED")

      /* Step 4: Verify Test 3 - VEXP(2.0) */
      println("\n[Test 3] VEXP(2.0) → exp(2.0) ≈ 7.389")

      dut.io.mem_debug_read_address.poke(0x2008.U)
      dut.clock.step(1)
      val exp2_bits = dut.io.mem_debug_read_data.peekInt()
      val exp2_result = ieee754ToFloat(exp2_bits)
      val exp2_expected = exp(2.0).toFloat
      val exp2_error = relativeError(exp2_result, exp2_expected)

      println(f"  Memory[0x2008] = 0x${exp2_bits}%08X")
      println(f"  Result:   ${exp2_result}%.6f")
      println(f"  Expected: ${exp2_expected}%.6f")
      println(f"  Error:    ${exp2_error * 100}%.4f%%")

      assert(exp2_error < 0.25,
        f"VEXP(2.0) error ${exp2_error * 100}%.4f%% exceeds 25%% threshold")
      println("  PASSED")

      /* Step 5: Verify Test 4 - VRSQRT(9.0) */
      println("\n[Test 4] VRSQRT(9.0) → 1/sqrt(9.0) ≈ 0.333")

      dut.io.mem_debug_read_address.poke(0x200C.U)
      dut.clock.step(1)
      val rsqrt2_bits = dut.io.mem_debug_read_data.peekInt()
      val rsqrt2_result = ieee754ToFloat(rsqrt2_bits)
      val rsqrt2_expected = (1.0 / sqrt(9.0)).toFloat
      val rsqrt2_error = relativeError(rsqrt2_result, rsqrt2_expected)

      println(f"  Memory[0x200C] = 0x${rsqrt2_bits}%08X")
      println(f"  Result:   ${rsqrt2_result}%.6f")
      println(f"  Expected: ${rsqrt2_expected}%.6f")
      println(f"  Error:    ${rsqrt2_error * 100}%.6f%%")

      assert(rsqrt2_error < 0.01,
        f"VRSQRT(9.0) error ${rsqrt2_error * 100}%.6f%% exceeds 1%% threshold")
      println("  PASSED")

      /* Step 6: Verify completion marker */
      println("\n[Step 6] Verifying completion marker...")

      dut.io.mem_debug_read_address.poke(0x2010.U)
      dut.clock.step(1)
      val marker = dut.io.mem_debug_read_data.peekInt()

      println(f"  Memory[0x2010] = 0x${marker}%08X")
      assert(marker == 0x00000001,
        s"Completion marker should be 0x00000001, got 0x${marker}%08X")
      println("  Completion marker verified")

      println("\n" + "="*80)
      println("  ALL TESTS PASSED - Custom SFU Instructions Working!")
      println("="*80)
      println(f"  VEXP(1.0):   ${exp1_error * 100}%6.4f%% error")
      println(f"  VRSQRT(4.0): ${rsqrt1_error * 100}%6.4f%% error")
      println(f"  VEXP(2.0):   ${exp2_error * 100}%6.4f%% error")
      println(f"  VRSQRT(9.0): ${rsqrt2_error * 100}%6.4f%% error")
      println("="*80 + "\n")
    }
  }
}
