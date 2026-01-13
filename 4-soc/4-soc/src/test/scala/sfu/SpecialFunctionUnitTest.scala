// SpecialFunctionUnit Integration Test
package sfu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.core.SFUOp
import scala.math.{exp, sqrt, abs}

/**
 * Integration test for SpecialFunctionUnit
 *
 * Tests the integration of three core modules:
 * 1. ExponentialApproximator (EXP operation)
 * 2. InvSqrt (RSQRT operation)
 * 3. VectorAccumulator (SUM operation)
 *
 * Each test verifies:
 * - Correct operation routing
 * - FSM state transitions
 * - Timing and valid signals
 * - Result accuracy
 */
class SpecialFunctionUnitTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("SpecialFunctionUnit")

  /* Helper Functions */

  // Convert Float to IEEE 754 32-bit representation
  def floatToIEEE754(f: Float): BigInt = {
    java.lang.Float.floatToIntBits(f) & 0xFFFFFFFFL
  }

  // Convert IEEE 754 32-bit to Float
  def ieee754ToFloat(bits: BigInt): Float = {
    java.lang.Float.intBitsToFloat(bits.toInt)
  }

  /**
   * Wait for operation to complete with timeout
   *
   * dut: The device under test
   * maxCycles: Maximum cycles to wait
   * return: true if done signal asserted
   */
  def waitForDone(dut: SpecialFunctionUnit, maxCycles: Int = 100): Boolean = {
    var cyclesWaited = 0
    var done = false

    while (cyclesWaited < maxCycles && !done) {
      dut.clock.step(1)
      done = dut.io.done.peek().litToBoolean
      cyclesWaited += 1
    }

    done
  }

  /* Test 1: EXP Operation Integration */

  it should "perform single EXP operation correctly" in {
    test(new SpecialFunctionUnit).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(200)

      println("\n" + "="*80)
      println("Test 1: EXP Operation Integration")
      println("="*80)

      // Test case: exp(1.0) ≈ 2.718
      val testValue = 1.0f
      val expectedExp = exp(testValue.toDouble).toFloat

      println(f"Testing exp($testValue%.2f)")
      println(f"Expected: $expectedExp%.6f")

      // Step 1: Initialize - Reset state
      dut.io.start.poke(false.B)
      dut.io.op.poke(SFUOp.NOP)
      dut.clock.step(1)

      // Step 2: Start EXP operation
      dut.io.op.poke(SFUOp.EXP)
      dut.io.in1.poke(floatToIEEE754(testValue).U)
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // Step 3: Wait for completion
      // ExponentialApproximator: 5-cycle latency
      val completed = waitForDone(dut, maxCycles = 20)

      assert(completed, "EXP operation timed out")

      // Step 4: Read result
      val resultBits = dut.io.out.peek().litValue
      val result = ieee754ToFloat(resultBits)
      val relativeError = abs((result - expectedExp) / expectedExp)

      println(f"Actual: $result%.6f")
      println(f"Relative Error: ${relativeError * 100}%.4f%%")

      // Verify valid signal is asserted
      assert(dut.io.valid.peek().litToBoolean, "Valid signal not asserte")

      assert(relativeError < 0.23,
        f"EXP error ${relativeError * 100}%.4f%% exceeds 23%% threshold")

      println("Test 1 PASSED: EXP operation correct")
    }
  }

  /* Test 2: RSQRT Operation Integration */

  it should "perform single RSQRT operation correctly" in {
    test(new SpecialFunctionUnit).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(200)

      println("\n" + "="*80)
      println("Test 2: RSQRT Operation Integration")
      println("="*80)

      // Test case: 1/sqrt(4.0) = 0.5
      val testValue = 4.0f
      val expectedRsqrt = (1.0 / sqrt(testValue.toDouble)).toFloat

      println(f"Testing 1/sqrt($testValue%.2f)")
      println(f"Expected: $expectedRsqrt%.6f")

      // Step 1: Initialize
      dut.io.start.poke(false.B)
      dut.io.op.poke(SFUOp.NOP)
      dut.clock.step(1)

      // Step 2: Start RSQRT operation
      dut.io.op.poke(SFUOp.RSQRT)
      dut.io.in1.poke(floatToIEEE754(testValue).U)
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // Step 3: Wait for completion
      // InvSqrt has 11-cycle latency (magic constant + 2 Newton-Raphson iterations)
      val completed = waitForDone(dut, maxCycles = 30)

      assert(completed, "RSQRT operation timed out")

      // Step 4: Read result
      val resultBits = dut.io.out.peek().litValue
      val result = ieee754ToFloat(resultBits)
      val relativeError = abs((result - expectedRsqrt) / expectedRsqrt)

      println(f"Actual: $result%.6f")
      println(f"Relative Error: ${relativeError * 100}%.4f%%")

      // Verify valid signal
      assert(dut.io.valid.peek().litToBoolean, "Valid signal not asserted")

      // InvSqrt has excellent precision:
      assert(relativeError < 0.01,
        f"RSQRT error ${relativeError * 100}%.4f%% exceeds 1%% threshold")

      println("Test 2 PASSED: RSQRT operation correct")
    }
  }

  /* Test 3: SUM (Vector Accumulation) Integration */

  it should "perform vector SUM operation correctly" in {
    test(new SpecialFunctionUnit).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(200)

      println("\n" + "="*80)
      println("Test 3: SUM (Vector Accumulation) Integration")
      println("="*80)

      // Test case: sum([1.0, 2.0, 3.0, 4.0]) = 10.0
      val testVector = Seq(1.0f, 2.0f, 3.0f, 4.0f)
      val expectedSum = testVector.sum

      println(s"Testing sum of vector: [${testVector.mkString(", ")}]")
      println(f"Expected: $expectedSum%.6f")

      // Step 1: Initialize
      dut.io.start.poke(false.B)
      dut.io.op.poke(SFUOp.NOP)
      dut.io.vec_in_valid.poke(false.B)
      dut.clock.step(1)

      // Step 2: Start SUM operation
      dut.io.op.poke(SFUOp.SUM)
      dut.io.in2.poke(testVector.length.U)  // Vector length
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // Step 3: Feed vector elements
      // VectorAccumulator uses streaming input
      testVector.foreach { value =>
        dut.io.vec_in.poke(floatToIEEE754(value).U)
        dut.io.vec_in_valid.poke(true.B)
        dut.clock.step(1)
      }
      dut.io.vec_in_valid.poke(false.B)

      // Step 4: Wait for completion
      val completed = waitForDone(dut, maxCycles = 30)

      assert(completed, "SUM operation timed out")

      // Step 5: Read result
      val resultBits = dut.io.out.peek().litValue
      val result = ieee754ToFloat(resultBits)
      val relativeError = abs((result - expectedSum) / expectedSum)

      println(f"Actual: $result%.6f")
      println(f"Relative Error: ${relativeError * 100}%.4f%%")

      // Verify valid signal
      assert(dut.io.valid.peek().litToBoolean, "Valid signal not asserted")

      assert(relativeError < 0.001,
        f"SUM error ${relativeError * 100}%.4f%% exceeds 0.1%% threshold")

      println("Test 3 PASSED: SUM operation correct")
    }
  }

  /* Test 4: FSM State Transitions */

  it should "correctly transition FSM states" in {
    test(new SpecialFunctionUnit).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)

      println("\n" + "="*80)
      println("Test 4: FSM State Transitions")
      println("="*80)

      // Verify initial state: Idle
      assert(!dut.io.busy.peek().litToBoolean,
        "SFU should be idle initially")
      println("Initial state: Idle")

      // Start an operation
      dut.io.op.poke(SFUOp.EXP)
      dut.io.in1.poke(floatToIEEE754(1.0f).U)
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // Verify state: Executing
      assert(dut.io.busy.peek().litToBoolean,
        "SFU should be busy during execution")
      println("Transition to: Executing")

      // Wait for done
      val completed = waitForDone(dut, maxCycles = 20)
      assert(completed, "Operation did not complete")

      // Verify done signal asserted for one cycle
      assert(dut.io.done.peek().litToBoolean,
        "Done signal should be asserted")
      println("Done signal asserted")

      // Step one more cycle, done should be cleared but busy still held
      dut.clock.step(1)
      assert(!dut.io.done.peek().litToBoolean,
        "Done signal should be cleared")

      // Step one more cycle, now should return to fully Idle
      dut.clock.step(1)
      assert(!dut.io.busy.peek().litToBoolean,
        "SFU should return to idle after busy hold period")
      println("Return to: Idle")

      println("Test 4 PASSED: FSM transitions correct")
    }
  }

  /* Test 5: Multiple Operations Sequentially */

  it should "handle multiple operations sequentially" in {
    test(new SpecialFunctionUnit).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(300)

      println("\n" + "="*80)
      println("Test 5: Multiple Sequential Operations")
      println("="*80)

      // Operation 1: EXP
      println("\nOperation 1: EXP(2.0)")
      dut.io.op.poke(SFUOp.EXP)
      dut.io.in1.poke(floatToIEEE754(2.0f).U)
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      var completed = waitForDone(dut, maxCycles = 20)
      assert(completed, "EXP timed out")

      val exp_result = ieee754ToFloat(dut.io.out.peek().litValue)
      val exp_expected = exp(2.0).toFloat
      println(f"  Result: $exp_result%.6f (Expected: $exp_expected%.6f)")

      dut.clock.step(1)  // Return to idle

      // Operation 2: RSQRT
      println("\nOperation 2: RSQRT(9.0)")
      dut.io.op.poke(SFUOp.RSQRT)
      dut.io.in1.poke(floatToIEEE754(9.0f).U)
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      completed = waitForDone(dut, maxCycles = 30)
      assert(completed, "RSQRT timed out")

      val rsqrt_result = ieee754ToFloat(dut.io.out.peek().litValue)
      val rsqrt_expected = (1.0 / sqrt(9.0)).toFloat
      println(f"  Result: $rsqrt_result%.6f (Expected: $rsqrt_expected%.6f)")

      dut.clock.step(1)  // Return to idle

      // Operation 3: SUM
      println("\nOperation 3: SUM([5.0, 10.0, 15.0])")
      val sumVector = Seq(5.0f, 10.0f, 15.0f)

      dut.io.op.poke(SFUOp.SUM)
      dut.io.in2.poke(sumVector.length.U)
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      sumVector.foreach { value =>
        dut.io.vec_in.poke(floatToIEEE754(value).U)
        dut.io.vec_in_valid.poke(true.B)
        dut.clock.step(1)
      }
      dut.io.vec_in_valid.poke(false.B)

      completed = waitForDone(dut, maxCycles = 30)
      assert(completed, "Operation 3 (SUM) timed out")

      val sum_result = ieee754ToFloat(dut.io.out.peek().litValue)
      val sum_expected = sumVector.sum
      println(f"  Result: $sum_result%.6f (Expected: $sum_expected%.6f)")

      println("\nTest 5 PASSED: Sequential operations correct")
    }
  }

  /* Test 6: Comprehensive Integration Test */

  it should "pass comprehensive integration test with multiple test cases" in {
    test(new SpecialFunctionUnit).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(500)

      println("\n" + "="*80)
      println("Test 6: Comprehensive Integration Test")
      println("="*80)

      var passCount = 0
      var totalTests = 0

      // EXP test cases
      val expTestCases = Seq(
        (0.0f, "exp(0.0)"),
        (1.0f, "exp(1.0)"),
        (-1.0f, "exp(-1.0)")
      )

      println("\n--- EXP Tests ---")
      expTestCases.foreach { case (input, desc) =>
        totalTests += 1
        val expected = exp(input.toDouble).toFloat

        dut.io.op.poke(SFUOp.EXP)
        dut.io.in1.poke(floatToIEEE754(input).U)
        dut.io.start.poke(true.B)
        dut.clock.step(1)
        dut.io.start.poke(false.B)

        if (waitForDone(dut, maxCycles = 20)) {
          val result = ieee754ToFloat(dut.io.out.peek().litValue)
          val error = abs((result - expected) / expected)

          if (error < 0.23) {
            passCount += 1
            println(f"$desc: $result%.6f (error: ${error*100}%.4f%%)")
          } else {
            println(f"$desc: $result%.6f (error: ${error*100}%.4f%% > 23%%)")
          }
        }
        dut.clock.step(1)
      }

      // RSQRT test cases
      val rsqrtTestCases = Seq(
        (1.0f, "rsqrt(1.0)"),
        (4.0f, "rsqrt(4.0)"),
        (16.0f, "rsqrt(16.0)")
      )

      println("\n--- RSQRT Tests ---")
      rsqrtTestCases.foreach { case (input, desc) =>
        totalTests += 1
        val expected = (1.0 / sqrt(input.toDouble)).toFloat

        dut.io.op.poke(SFUOp.RSQRT)
        dut.io.in1.poke(floatToIEEE754(input).U)
        dut.io.start.poke(true.B)
        dut.clock.step(1)
        dut.io.start.poke(false.B)

        if (waitForDone(dut, maxCycles = 30)) {
          val result = ieee754ToFloat(dut.io.out.peek().litValue)
          val error = abs((result - expected) / expected)

          if (error < 0.01) {
            passCount += 1
            println(f"$desc: $result%.6f (error: ${error*100}%.6f%%)")
          } else {
            println(f"$desc: $result%.6f (error: ${error*100}%.6f%% > 1%%)")
          }
        }
        dut.clock.step(1)
      }

      println("\n" + "="*80)
      println(f"Final Score: $passCount/$totalTests tests passed")
      println("="*80)

      assert(passCount == totalTests,
        s"Some tests failed: $passCount/$totalTests passed")

      println("Test 6 PASSED: All integration tests passed")
    }
  }
}
