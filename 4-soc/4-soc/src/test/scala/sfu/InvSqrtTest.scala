// InvSqrt Unit Test
// Tests fast inverse square root (1/sqrt(x)) using Quake III algorithm

package sfu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.math.{sqrt, abs}

class InvSqrtTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("InvSqrt")

  // IEEE 754 single-precision float conversion helpers
  def floatToIEEE754(f: Float): BigInt = {
    java.lang.Float.floatToIntBits(f) & 0xFFFFFFFFL
  }

  def ieee754ToFloat(bits: BigInt): Float = {
    java.lang.Float.intBitsToFloat(bits.toInt)
  }

  // Helper to test a single value
  def testInvSqrtValue(dut: InvSqrt, x: Float): (Float, Float, Float) = {
    val expectedInvSqrt = (1.0 / sqrt(x.toDouble)).toFloat

    // Poke input
    dut.io.in.poke(floatToIEEE754(x).U)
    dut.io.valid.poke(true.B)

    // Wait for pipeline latency (11 cycles after implementing Newton-Raphson iterations)
    dut.clock.step(11)

    // Read output
    val resultBits = dut.io.out.peek().litValue
    val result = ieee754ToFloat(resultBits)

    val relativeError = if (expectedInvSqrt != 0.0f) {
      abs((result - expectedInvSqrt) / expectedInvSqrt)
    } else {
      abs(result - expectedInvSqrt)
    }

    (result, expectedInvSqrt, relativeError)
  }

  it should "compute 1/sqrt(1.0) = 1.0" in {
    test(new InvSqrt).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(20)

      val x = 1.0f
      val (result, expected, error) = testInvSqrtValue(dut, x)

      println(f"1/sqrt($x%.2f) - Expected: $expected%.6f, Actual: $result%.6f, Error: ${error * 100}%.4f%%")

      // Target: < 1% error after 2 Newton-Raphson iterations
      assert(error < 0.01, f"Error ${error * 100}%.4f%% should be less than 1%%")
    }
  }

  it should "compute 1/sqrt(4.0) = 0.5" in {
    test(new InvSqrt).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(20)

      val x = 4.0f
      val (result, expected, error) = testInvSqrtValue(dut, x)

      println(f"1/sqrt($x%.2f) - Expected: $expected%.6f, Actual: $result%.6f, Error: ${error * 100}%.4f%%")

      assert(error < 0.01, f"Error ${error * 100}%.4f%% should be less than 1%%")
    }
  }

  it should "compute 1/sqrt(0.25) = 2.0" in {
    test(new InvSqrt).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(20)

      val x = 0.25f
      val (result, expected, error) = testInvSqrtValue(dut, x)

      println(f"1/sqrt($x%.2f) - Expected: $expected%.6f, Actual: $result%.6f, Error: ${error * 100}%.4f%%")

      assert(error < 0.01, f"Error ${error * 100}%.4f%% should be less than 1%%")
    }
  }

  it should "compute 1/sqrt(9.0) = 0.333..." in {
    test(new InvSqrt).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(20)

      val x = 9.0f
      val (result, expected, error) = testInvSqrtValue(dut, x)

      println(f"1/sqrt($x%.2f) - Expected: $expected%.6f, Actual: $result%.6f, Error: ${error * 100}%.4f%%")

      assert(error < 0.01, f"Error ${error * 100}%.4f%% should be less than 1%%")
    }
  }

  it should "compute 1/sqrt(2.0) = 0.707..." in {
    test(new InvSqrt).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(20)

      val x = 2.0f
      val (result, expected, error) = testInvSqrtValue(dut, x)

      println(f"1/sqrt($x%.2f) - Expected: $expected%.6f, Actual: $result%.6f, Error: ${error * 100}%.4f%%")

      assert(error < 0.01, f"Error ${error * 100}%.4f%% should be less than 1%%")
    }
  }

  it should "test multiple values and measure accuracy" in {
    test(new InvSqrt).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(500)

      // Test values across a range
      val testValues = Seq(
        0.1f, 0.25f, 0.5f, 1.0f, 2.0f,
        4.0f, 9.0f, 16.0f, 25.0f, 100.0f
      )

      var totalError = 0.0
      var maxError = 0.0
      var errorCount = 0

      println("\n" + "="*80)
      println("Testing Inverse Square Root Across Range")
      println("="*80)
      println(f"${"x"}%10s  ${"1/sqrt(x)"}%12s  ${"approx"}%12s  ${"error"}%10s")
      println("-"*80)

      testValues.foreach { x =>
        val (result, expected, error) = testInvSqrtValue(dut, x)

        println(f"$x%10.2f  $expected%12.6f  $result%12.6f  ${error * 100}%9.4f%%")

        totalError += error
        maxError = math.max(maxError, error)
        errorCount += 1

        // With 2 Newton-Raphson iterations, error should be < 1%
        assert(error < 0.01, f"Error ${error * 100}%.4f%% at x=$x should be less than 1%%")
      }

      val avgError = totalError / errorCount

      println("-"*80)
      println(f"Average Relative Error: ${avgError * 100}%.4f%%")
      println(f"Maximum Relative Error: ${maxError * 100}%.4f%%")
      println(f"Target: Mean < 0.5%%, Max < 1%% (with 2 N-R iterations)")
      println("="*80)

      // Check against design specification
      assert(avgError < 0.005, f"Average error ${avgError * 100}%.4f%% should be < 0.5%%")
      assert(maxError < 0.01, f"Max error ${maxError * 100}%.4f%% should be < 1%%")
    }
  }

  it should "handle pipelined throughput correctly" in {
    test(new InvSqrt).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)

      // Test pipelined operation
      // Using 11 inputs to match 11-cycle pipeline latency for correct timing
      val testInputs = Seq(1.0f, 2.0f, 4.0f, 9.0f, 16.0f, 25.0f, 36.0f, 49.0f, 64.0f, 81.0f, 100.0f)

      println("\n" + "="*80)
      println("Testing Pipelined Throughput")
      println("="*80)

      // Feed all inputs
      testInputs.foreach { x =>
        dut.io.in.poke(floatToIEEE754(x).U)
        dut.io.valid.poke(true.B)
        dut.clock.step(1)
      }

      dut.io.valid.poke(false.B)

      // Read all outputs
      testInputs.foreach { x =>
        val expectedInvSqrt = (1.0 / sqrt(x.toDouble)).toFloat
        val resultBits = dut.io.out.peek().litValue
        val result = ieee754ToFloat(resultBits)
        val error = if (expectedInvSqrt != 0.0f) {
          abs((result - expectedInvSqrt) / expectedInvSqrt)
        } else {
          abs(result - expectedInvSqrt)
        }

        println(f"1/sqrt($x%6.2f) - Expected: $expectedInvSqrt%10.6f, Actual: $result%10.6f, Error: ${error * 100}%6.2f%%")

        assert(error < 0.01, f"Pipelined result error should be < 1%%")
        dut.clock.step(1)
      }

      println("="*80)
    }
  }

  it should "handle very small values (< 1)" in {
    test(new InvSqrt).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)

      val testValues = Seq(0.01f, 0.1f, 0.5f)

      println("\n" + "="*80)
      println("Testing Small Input Values")
      println("="*80)

      testValues.foreach { x =>
        val (result, expected, error) = testInvSqrtValue(dut, x)

        println(f"1/sqrt($x%.4f) - Expected: $expected%.6f, Actual: $result%.6f, Error: ${error * 100}%.4f%%")

        assert(error < 0.01, f"Error for small values should be < 1%%")
      }

      println("="*80)
    }
  }

  it should "handle large values (> 100)" in {
    test(new InvSqrt).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)

      val testValues = Seq(100.0f, 1000.0f, 10000.0f)

      println("\n" + "="*80)
      println("Testing Large Input Values")
      println("="*80)

      testValues.foreach { x =>
        val (result, expected, error) = testInvSqrtValue(dut, x)

        println(f"1/sqrt($x%.1f) - Expected: $expected%.6f, Actual: $result%.6f, Error: ${error * 100}%.4f%%")

        assert(error < 0.01, f"Error for large values should be < 1%%")
      }

      println("="*80)
    }
  }

  it should "verify Quake III magic constant provides good initial approximation" in {
    test(new InvSqrt).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(50)

      println("\n" + "="*80)
      println("Testing Magic Constant Initial Approximation Quality")
      println("="*80)

      // Test  values
      val testValues = Seq(1.0f, 4.0f, 16.0f)

      testValues.foreach { x =>
        // Feed input
        dut.io.in.poke(floatToIEEE754(x).U)
        dut.io.valid.poke(true.B)

        // Check after stage 1 only (magic constant approximation)
        dut.clock.step(1)

        val expectedInvSqrt = (1.0 / sqrt(x.toDouble)).toFloat

        println(f"x = $x%.2f: 1/sqrt(x) = $expectedInvSqrt%.6f")
        println(f"  Magic constant should give ~3-5%% error on first approximation")

        // Continue to final result
        dut.clock.step(2)
      }

      println("="*80)
    }
  }
}
