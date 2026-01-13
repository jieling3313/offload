// FPDivider Unit Tests
// Tests the Newton-Raphson based floating-point divider

package sfu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/**
 * Test suite for FPDivider module
 *
 * Tests the Newton-Raphson reciprocal division implementation across various input ranges and edge cases.
 *
 * Test Categories:
 * 1. Basic division operations
 * 2. Precision tests (comparing to software floating-point)
 * 3. Edge cases (small/large values, fractions)
 * 4. Special values (zero handling)
 * 5. Accuracy validation (error thresholds)
 */
class FPDividerTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FPDivider"

  /* Helper Functions for IEEE 754 Conversion */

  // Convert Float to IEEE 754 32-bit representation
  def floatToIEEE754(f: Float): BigInt = {
    BigInt(java.lang.Float.floatToIntBits(f) & 0xFFFFFFFFL)
  }

  // Convert IEEE 754 32-bit representation to Float
  def ieee754ToFloat(bits: BigInt): Float = {
    java.lang.Float.intBitsToFloat(bits.toInt)
  }

  /**
   * Helper function to test a single division operation
   *
   * a: Dividend
   * b: Divisor
   * tolerance: Maximum acceptable relative error (default 1%)
   */
  def testDivision(a: Float, b: Float, tolerance: Double = 0.01): Unit = {
    it should f"compute $a / $b correctly (within ${tolerance * 100}%% error)" in {
      test(new FPDivider) { dut =>
        val a_bits = floatToIEEE754(a)
        val b_bits = floatToIEEE754(b)
        val expected = a / b

        // Poke inputs
        dut.io.a.poke(a_bits.U)
        dut.io.b.poke(b_bits.U)

        // Wait for 8-cycle pipeline latency (Newton-Raphson: 2 iterations)
        // Stage 0: Initial guess (combinational)
        // Stage 1-3: First iteration
        // Stage 4-6: Second iteration
        // Stage 7: Final multiply
        dut.clock.step(8)

        // Read output
        val result_bits = dut.io.result.peek().litValue
        val result = ieee754ToFloat(result_bits)

        // Calculate error
        val error = if (expected != 0.0f) {
          Math.abs((result - expected) / expected)
        } else {
          if (result == 0.0f) 0.0 else 1.0
        }

        // Print results
        println(f"Division Test: $a%.6f / $b%.6f")
        println(f"  Expected: $expected%.6f")
        println(f"  Result:   $result%.6f")
        println(f"  Error:    ${error * 100}%.4f%%")

        // Assert within tolerance
        assert(error < tolerance,
          s"Error ${error * 100}% exceeds tolerance ${tolerance * 100}%\n" +
          s"  Input: $a / $b\n" +
          s"  Expected: $expected\n" +
          s"  Got: $result")
      }
    }
  }

  /* Test Category 1: Basic Division Operations */

  testDivision(1.0f, 1.0f, tolerance = 0.001)   //   1 /  1 = 1.0
  testDivision(10.0f, 2.0f, tolerance = 0.01)   //  10 /  2 = 5.0
  testDivision(100.0f, 10.0f, tolerance = 0.01) // 100 / 10 = 10.0
  testDivision(7.0f, 1.0f, tolerance = 0.01)    //   7 /  1 = 7.0

  /* Test Category 2: Fractional Results */

  testDivision(1.0f, 2.0f, tolerance = 0.01)    //  1 / 2 = 0.5
  testDivision(1.0f, 3.0f, tolerance = 0.01)    //  1 / 3 = 0.333...
  testDivision(1.0f, 4.0f, tolerance = 0.01)    //  1 / 4 = 0.25
  testDivision(7.0f, 3.0f, tolerance = 0.01)    //  7 / 3 = 2.333...
  testDivision(22.0f, 7.0f, tolerance = 0.01)   // 22 / 7 ≈ π

  // ============================================================
  // Test Category 3: Small and Large Values
  // ============================================================

  testDivision(0.5f, 0.25f, tolerance = 0.01)    //  0.5 / 0.25 = 2.0
  testDivision(0.1f, 0.2f, tolerance = 0.01)     //  0.1 /  0.2 = 0.5
  testDivision(1000.0f, 10.0f, tolerance = 0.01) // 1000 /   10 = 100
  testDivision(1.0f, 1000.0f, tolerance = 0.01)  //    1 / 1000 = 0.001
  testDivision(100.0f, 7.0f, tolerance = 0.01)   //  100 /    7 = 14.285...

  /* Test Category 4: Precision Validation */

  it should "achieve better than 1% accuracy for typical Softmax values" in {
    test(new FPDivider) { dut =>
      // Simulate Softmax scenario: exp(x_i) / sum(exp)
      // Example: exp(1.0) / (exp(1.0) + exp(2.0) + exp(3.0))
      val exp1 = math.exp(1.0).toFloat  // ≈ 2.718
      val exp2 = math.exp(2.0).toFloat  // ≈ 7.389
      val exp3 = math.exp(3.0).toFloat  // ≈ 20.086
      val sum = exp1 + exp2 + exp3      // ≈ 30.193

      val test_cases = Seq(
        (exp1, sum),  // First element
        (exp2, sum),  // Second element
        (exp3, sum)   // Third element
      )

      println("========================================")
      println("Softmax Precision Test")
      println("========================================")

      var max_error = 0.0

      test_cases.foreach { case (numerator, denominator) =>
        val a_bits = floatToIEEE754(numerator)
        val b_bits = floatToIEEE754(denominator)
        val expected = numerator / denominator

        dut.io.a.poke(a_bits.U)
        dut.io.b.poke(b_bits.U)
        dut.clock.step(8)

        val result_bits = dut.io.result.peek().litValue
        val result = ieee754ToFloat(result_bits)
        val error = Math.abs((result - expected) / expected)

        println(f"  $numerator%.6f / $denominator%.6f = $expected%.6f")
        println(f"    Hardware result: $result%.6f, Error: ${error * 100}%.4f%%")

        max_error = Math.max(max_error, error)
      }

      println(f"  Maximum error: ${max_error * 100}%.4f%%")
      println("========================================")

      assert(max_error < 0.01, s"Max error ${max_error * 100}% exceeds 1%")
    }
  }

  /* Test Category 5: RMSNorm Mean Division */

  it should "accurately compute mean for RMSNorm (sum/N)" in {
    test(new FPDivider) { dut =>
      // Test: sum of squares / vector length
      // Example: [1.0, 2.0, 3.0, 4.0] → sum(x^2) = 30.0, N = 4, mean = 7.5
      val sum_of_squares = 30.0f
      val vector_length = 4.0f
      val expected_mean = 7.5f

      val sum_bits = floatToIEEE754(sum_of_squares)
      val len_bits = floatToIEEE754(vector_length)

      dut.io.a.poke(sum_bits.U)
      dut.io.b.poke(len_bits.U)
      dut.clock.step(8)  // 8-cycle pipeline latency

      val result_bits = dut.io.result.peek().litValue
      val result = ieee754ToFloat(result_bits)
      val error = Math.abs((result - expected_mean) / expected_mean)

      println("========================================")
      println("RMSNorm Mean Division Test")
      println("========================================")
      println(f"  Sum of squares: $sum_of_squares%.6f")
      println(f"  Vector length:  $vector_length%.6f")
      println(f"  Expected mean:  $expected_mean%.6f")
      println(f"  Hardware result: $result%.6f")
      println(f"  Error: ${error * 100}%.4f%%")
      println("========================================")

      assert(error < 0.01, s"Error ${error * 100}% exceeds 1%")
    }
  }

  /* Test Category 6: Zero Handling */

  it should "handle zero dividend (0/b = 0)" in {
    test(new FPDivider) { dut =>
      val zero_bits = floatToIEEE754(0.0f)
      val b_bits = floatToIEEE754(5.0f)

      dut.io.a.poke(zero_bits.U)
      dut.io.b.poke(b_bits.U)
      dut.clock.step(5)

      val result_bits = dut.io.result.peek().litValue
      val result = ieee754ToFloat(result_bits)

      println(s"Zero dividend test: 0.0 / 5.0 = $result (expected: 0.0)")
      assert(result == 0.0f, s"Expected 0.0, got $result")
    }
  }

  it should "handle zero divisor safely (a/0 -> 0)" in {
    test(new FPDivider) { dut =>
      val a_bits = floatToIEEE754(5.0f)
      val zero_bits = floatToIEEE754(0.0f)

      dut.io.a.poke(a_bits.U)
      dut.io.b.poke(zero_bits.U)
      dut.clock.step(5)

      val result_bits = dut.io.result.peek().litValue
      val result = ieee754ToFloat(result_bits)

      println(s"Zero divisor test: 5.0 / 0.0 = $result (returns 0 for safety)")
      // Ideal: return Infinity, For safty: return 0
      assert(result == 0.0f, s"Expected 0.0 (safety value), got $result")
    }
  }

  /* Test Category 7: Pipelined Throughput */

  it should "handle pipelined throughput correctly" in {
    test(new FPDivider) { dut =>
      println("========================================")
      println("Pipelined Throughput Test")
      println("========================================")

      // Test inputs: feed consecutive divisions
      val test_inputs = Seq(
        (10.0f, 2.0f),   // = 5.0
        (9.0f, 3.0f),    // = 3.0
        (16.0f, 4.0f),   // = 4.0
        (25.0f, 5.0f),   // = 5.0
        (36.0f, 6.0f)    // = 6.0
      )

      val expected_results = test_inputs.map { case (a, b) => a / b }

      // Feed all inputs (pipelined operation)
      test_inputs.zipWithIndex.foreach { case ((a, b), idx) =>
        println(f"  Feeding input $idx: $a%.1f / $b%.1f")
        dut.io.a.poke(floatToIEEE754(a).U)
        dut.io.b.poke(floatToIEEE754(b).U)
        dut.clock.step(1)  // Feed next input every cycle
      }

      // Wait for first result to emerge
      // After feeding 5 inputs with step(1) each @ cycle 5
      // !!! Need 2 more cycles for first result
      println(s"  Waiting for pipeline to fill (2 more cycles)...")
      dut.clock.step(2)

      // Read outputs (first result is ready now, then step for each subsequent)
      val results = test_inputs.indices.map { idx =>
        val result_bits = dut.io.result.peek().litValue
        val result = ieee754ToFloat(result_bits)
        println(f"  Reading output $idx: $result%.6f")
        if (idx < test_inputs.length - 1) dut.clock.step(1)  // Step to next result
        result
      }

      // Validate results
      results.zip(expected_results).zipWithIndex.foreach {
        case ((result, expected), idx) =>
          val error = Math.abs((result - expected) / expected)
          println(f"  Result $idx: $result%.6f (expected: $expected%.6f, error: ${error * 100}%.4f%%)")
          assert(error < 0.01, s"Result $idx error exceeds 1%")
      }

      println("All pipelined results within tolerance  ")
      println("========================================")
    }
  }

  /* Test Category 8: Comprehensive Range Test */

  it should "maintain accuracy across wide range of values" in {
    test(new FPDivider) { dut =>
      println("========================================")
      println("Comprehensive Range Test")
      println("========================================")

      val test_cases = Seq(
        (0.01f, 0.1f),    // Small values
        (0.1f, 1.0f),     // Fractional < 1
        (1.0f, 1.0f),     // Unity
        (10.0f, 3.0f),    // Medium values
        (100.0f, 7.0f),   // Larger values
        (1000.0f, 13.0f), // Large values
        (Math.PI.toFloat, Math.E.toFloat) // Irrational numbers
      )

      var max_error = 0.0

      test_cases.foreach { case (a, b) =>
        val a_bits = floatToIEEE754(a)
        val b_bits = floatToIEEE754(b)
        val expected = a / b

        dut.io.a.poke(a_bits.U)
        dut.io.b.poke(b_bits.U)
        dut.clock.step(8)

        val result_bits = dut.io.result.peek().litValue
        val result = ieee754ToFloat(result_bits)
        val error = Math.abs((result - expected) / expected)

        println(f"  $a%.6f / $b%.6f = $expected%.6f → $result%.6f (error: ${error * 100}%.4f%%)")

        max_error = Math.max(max_error, error)
      }

      println(f"Maximum error across all tests: ${max_error * 100}%.4f%%")
      println("========================================")

      assert(max_error < 0.02, s"Max error ${max_error * 100}% exceeds 2%")
    }
  }
}
