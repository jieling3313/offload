// Softmax Test Suite
// Tests the Softmax accelerator implementation

package sfu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SoftmaxTest extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "SoftmaxAccelerator"

  // Helper function to convert Float to IEEE 754 UInt
  def floatToUInt(f: Float): UInt = {
    val bits = java.lang.Float.floatToIntBits(f)
    (bits & 0xFFFFFFFFL).U(32.W)
  }

  // Helper function to convert IEEE 754 UInt to Float
  def uintToFloat(u: UInt): Float = {
    java.lang.Float.intBitsToFloat(u.litValue.toInt)
  }

  // Helper function to compute relative error
  def relativeError(expected: Float, actual: Float): Double = {
    if (expected == 0.0f) {
      if (actual == 0.0f) 0.0 else 100.0
    } else {
      math.abs((expected - actual) / expected) * 100.0
    }
  }

  /**
   * Implementation of Softmax
   * Formula: softmax(x_i) = exp(x_i - max(x)) / sum(exp(x_j - max(x)))
   * !!! Using max subtraction for numerical stability to prevent overflow
   */
  def softwareSoftmax(input: Seq[Float]): Seq[Float] = {
    // Step 1: Find max value (for numerical stability)
    val max_value = input.max
    println(s"  Max value: $max_value")

    // Step 2: Compute exp(x - max) for each element
    val exp_values = input.map(x => math.exp(x - max_value).toFloat)
    println(s"  Exp values: ${exp_values.map(x => f"$x%.6f").mkString(", ")}")

    // Step 3: Compute sum of exp values
    val sum_exp = exp_values.sum
    println(s"  Sum of exp: $sum_exp")

    // Step 4: Normalize by dividing each exp value by sum
    val softmax_values = exp_values.map(x => x / sum_exp)
    println(s"  Softmax values: ${softmax_values.map(x => f"$x%.6f").mkString(", ")}")

    // Verify sum is approximately 1.0
    val sum_check = softmax_values.sum
    println(s"  Sum of softmax (should be 1.0): $sum_check")

    softmax_values
  }

  it should "compute Softmax for simple vector [1.0, 2.0, 3.0]" in {
    test(new SoftmaxAccelerator).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val input = Seq(1.0f, 2.0f, 3.0f)

      println(s"\n[Software Reference]")
      val expected = softwareSoftmax(input)

      // Initialize
      dut.io.start.poke(false.B)
      dut.io.in_valid.poke(false.B)
      dut.clock.step(1)

      // Start Softmax
      dut.io.start.poke(true.B)
      dut.io.length.poke(input.length.U)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      println(s"\n[Test] Starting Softmax with ${input.length} elements")

      // Feed input elements
      for ((value, idx) <- input.zipWithIndex) {
        dut.io.in.poke(floatToUInt(value))
        dut.io.in_valid.poke(true.B)
        println(f"[Test] Feeding element $idx: $value%.2f (0x${floatToUInt(value).litValue.toInt}%08x)")
        dut.clock.step(1)
      }
      dut.io.in_valid.poke(false.B)

      // Wait for computation
      var timeout = 2000
      var outputs_collected = 0
      val actual_outputs = scala.collection.mutable.ArrayBuffer[Float]()

      while (timeout > 0 && outputs_collected < input.length) {
        if (dut.io.out_valid.peek().litToBoolean) {
          val output_bits = dut.io.out.peek()
          val output_value = uintToFloat(output_bits)
          actual_outputs += output_value
          println(f"[Test] Output[$outputs_collected]: $output_value%.6f (0x${output_bits.litValue.toInt}%08x)")
          outputs_collected += 1
        }

        if (dut.io.done.peek().litToBoolean) {
          println("[Test] Done signal asserted")
        }

        dut.clock.step(1)
        timeout -= 1
      }

      assert(timeout > 0, "Timeout waiting for Softmax to complete")
      assert(outputs_collected == input.length, s"Expected ${input.length} outputs, got $outputs_collected")

      // Verify outputs
      println("\n[Verification]")
      var max_error = 0.0
      var total_error = 0.0

      for (i <- 0 until input.length) {
        val error = relativeError(expected(i), actual_outputs(i))
        total_error += error
        max_error = math.max(max_error, error)

        println(f"  Element $i: expected=${expected(i)}%.6f, actual=${actual_outputs(i)}%.6f, error=$error%.4f%%")

        // Assert with 25% error tolerance (due to exp approximation)
        assert(error < 25.0, f"Element $i error $error%.4f%% exceeds 25%% threshold")
      }

      val avg_error = total_error / input.length
      println(f"\n[Summary]")
      println(f"  Average error: $avg_error%.4f%%")
      println(f"  Maximum error: $max_error%.4f%%")
      println(f"  All tests passed!")
    }
  }

  it should "compute Softmax for uniform vector [1.0, 1.0, 1.0]" in {
    test(new SoftmaxAccelerator) { dut =>
      val input = Seq(1.0f, 1.0f, 1.0f)

      println(s"\n[Software Reference]")
      val expected = softwareSoftmax(input)

      dut.io.start.poke(false.B)
      dut.io.in_valid.poke(false.B)
      dut.clock.step(1)

      dut.io.start.poke(true.B)
      dut.io.length.poke(input.length.U)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      println(s"\n[Test] Uniform vector test (each output should be ~0.333)")

      for ((value, idx) <- input.zipWithIndex) {
        dut.io.in.poke(floatToUInt(value))
        dut.io.in_valid.poke(true.B)
        dut.clock.step(1)
      }
      dut.io.in_valid.poke(false.B)

      var timeout = 2000
      var outputs_collected = 0
      val actual_outputs = scala.collection.mutable.ArrayBuffer[Float]()

      while (timeout > 0 && outputs_collected < input.length) {
        if (dut.io.out_valid.peek().litToBoolean) {
          val output_value = uintToFloat(dut.io.out.peek())
          actual_outputs += output_value
          outputs_collected += 1
        }
        dut.clock.step(1)
        timeout -= 1
      }

      assert(timeout > 0, "Timeout waiting for Softmax to complete")

      println("\n[Verification]")
      for (i <- 0 until input.length) {
        val error = relativeError(expected(i), actual_outputs(i))
        println(f"  Element $i: expected=${expected(i)}%.6f, actual=${actual_outputs(i)}%.6f, error=$error%.4f%%")
        assert(error < 25.0, f"Element $i error $error%.4f%% exceeds 25%% threshold")
      }
    }
  }

  it should "compute Softmax for vector [0.0, 5.0, 10.0] (numerical stability test)" in {
    test(new SoftmaxAccelerator) { dut =>
      val input = Seq(0.0f, 5.0f, 10.0f)

      println(s"\n[Software Reference]")
      val expected = softwareSoftmax(input)

      dut.io.start.poke(false.B)
      dut.io.in_valid.poke(false.B)
      dut.clock.step(1)

      dut.io.start.poke(true.B)
      dut.io.length.poke(input.length.U)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      println(s"\n[Test] Numerical stability test with large values")

      for ((value, idx) <- input.zipWithIndex) {
        dut.io.in.poke(floatToUInt(value))
        dut.io.in_valid.poke(true.B)
        dut.clock.step(1)
      }
      dut.io.in_valid.poke(false.B)

      var timeout = 2000
      var outputs_collected = 0
      val actual_outputs = scala.collection.mutable.ArrayBuffer[Float]()

      while (timeout > 0 && outputs_collected < input.length) {
        if (dut.io.out_valid.peek().litToBoolean) {
          val output_value = uintToFloat(dut.io.out.peek())
          actual_outputs += output_value
          outputs_collected += 1
        }
        dut.clock.step(1)
        timeout -= 1
      }

      assert(timeout > 0, "Timeout waiting for Softmax to complete")

      println("\n[Verification]")
      for (i <- 0 until input.length) {
        val error = relativeError(expected(i), actual_outputs(i))
        println(f"  Element $i: expected=${expected(i)}%.6f, actual=${actual_outputs(i)}%.6f, error=$error%.4f%%")
        assert(error < 25.0, f"Element $i error $error%.4f%% exceeds 25%% threshold")
      }
    }
  }

  it should "handle single element vector" in {
    test(new SoftmaxAccelerator) { dut =>
      val input = Seq(2.0f)

      println(s"\n[Software Reference]")
      val expected = softwareSoftmax(input)

      dut.io.start.poke(false.B)
      dut.io.in_valid.poke(false.B)
      dut.clock.step(1)

      dut.io.start.poke(true.B)
      dut.io.length.poke(input.length.U)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      println(s"\n[Test] Single element test (output should be 1.0)")

      dut.io.in.poke(floatToUInt(input(0)))
      dut.io.in_valid.poke(true.B)
      dut.clock.step(1)
      dut.io.in_valid.poke(false.B)

      var timeout = 2000
      var got_output = false
      var actual_output = 0.0f

      while (timeout > 0 && !got_output) {
        if (dut.io.out_valid.peek().litToBoolean) {
          actual_output = uintToFloat(dut.io.out.peek())
          got_output = true
        }
        dut.clock.step(1)
        timeout -= 1
      }

      assert(timeout > 0, "Timeout waiting for Softmax to complete")
      val error = relativeError(expected(0), actual_output)
      println(f"Single element: expected=${expected(0)}%.6f, actual=$actual_output%.6f, error=$error%.4f%%")
      assert(error < 25.0, f"Error $error%.4f%% exceeds 25%% threshold")
    }
  }
}
