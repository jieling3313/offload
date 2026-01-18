// RMSNorm Test Suite
// Tests the RMSNorm accelerator implementation

package sfu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class RMSNormTest extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "RMSNormAccelerator"

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

  // Implementation of RMSNorm
  def softwareRMSNorm(input: Seq[Float], gain: Float): Seq[Float] = {
    // Step 1: Compute mean of squares
    val squares = input.map(x => x * x)
    val sum_squares = squares.sum
    val mean_squares = sum_squares / input.length

    // Step 2: Compute normalization factor (1 / sqrt(mean))
    val norm_factor = 1.0f / math.sqrt(mean_squares).toFloat

    // Step 3: Normalize each element
    val normalized = input.map(x => x * norm_factor * gain)

    println(s"[Software RMSNorm]")
    println(s"  Input: ${input.mkString(", ")}")
    println(s"  Squares: ${squares.map(x => f"$x%.4f").mkString(", ")}")
    println(s"  Sum of squares: $sum_squares")
    println(s"  Mean of squares: $mean_squares")
    println(s"  Norm factor (1/√mean): $norm_factor")
    println(s"  Gain: $gain")
    println(s"  Output: ${normalized.mkString(", ")}")

    normalized
  }

  it should "compute RMSNorm for simple vector [1.0, 2.0, 3.0, 4.0] with gain=1.0" in {
    test(new RMSNormAccelerator).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val input = Seq(1.0f, 2.0f, 3.0f, 4.0f)
      val gain = 1.0f
      val expected = softwareRMSNorm(input, gain)

      // Initialize
      dut.io.start.poke(false.B)
      dut.io.in_valid.poke(false.B)
      dut.clock.step(1)

      // Start RMSNorm
      dut.io.start.poke(true.B)
      dut.io.length.poke(input.length.U)
      dut.io.gain.poke(floatToUInt(gain))
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      println(s"\n[Test] Starting RMSNorm with ${input.length} elements, gain=$gain")

      // Feed input elements
      for ((value, idx) <- input.zipWithIndex) {
        dut.io.in.poke(floatToUInt(value))
        dut.io.in_valid.poke(true.B)
        println(f"[Test] Feeding element $idx: $value%.2f (0x${floatToUInt(value).litValue.toInt}%08x)")
        dut.clock.step(1)
      }
      dut.io.in_valid.poke(false.B)

      // Wait for computation
      var timeout = 1000
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

      assert(timeout > 0, "Timeout waiting for RMSNorm to complete")
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

        // Assert with 1% error tolerance
        assert(error < 1.0, f"Element $i error $error%.4f%% exceeds 1%% threshold")
      }

      val avg_error = total_error / input.length
      println(f"\n[Summary]")
      println(f"  Average error: $avg_error%.4f%%")
      println(f"  Maximum error: $max_error%.4f%%")
      println(f"  All tests passed!")
    }
  }

  it should "compute RMSNorm with gain=2.0" in {
    test(new RMSNormAccelerator) { dut =>
      val input = Seq(1.0f, 2.0f, 3.0f, 4.0f)
      val gain = 2.0f
      val expected = softwareRMSNorm(input, gain)

      dut.io.start.poke(false.B)
      dut.io.in_valid.poke(false.B)
      dut.clock.step(1)

      dut.io.start.poke(true.B)
      dut.io.length.poke(input.length.U)
      dut.io.gain.poke(floatToUInt(gain))
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      for (value <- input) {
        dut.io.in.poke(floatToUInt(value))
        dut.io.in_valid.poke(true.B)
        dut.clock.step(1)
      }
      dut.io.in_valid.poke(false.B)

      var timeout = 1000
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

      assert(timeout > 0, "Timeout waiting for RMSNorm to complete")

      for (i <- 0 until input.length) {
        val error = relativeError(expected(i), actual_outputs(i))
        assert(error < 1.0, f"Element $i error $error%.4f%% exceeds 1%% threshold")
      }
    }
  }

  it should "compute RMSNorm for vector [0.5, 1.5, 2.5]" in {
    test(new RMSNormAccelerator) { dut =>
      val input = Seq(0.5f, 1.5f, 2.5f)
      val gain = 1.0f
      val expected = softwareRMSNorm(input, gain)

      dut.io.start.poke(false.B)
      dut.io.in_valid.poke(false.B)
      dut.clock.step(1)

      dut.io.start.poke(true.B)
      dut.io.length.poke(input.length.U)
      dut.io.gain.poke(floatToUInt(gain))
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      for (value <- input) {
        dut.io.in.poke(floatToUInt(value))
        dut.io.in_valid.poke(true.B)
        dut.clock.step(1)
      }
      dut.io.in_valid.poke(false.B)

      var timeout = 1000
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

      assert(timeout > 0, "Timeout waiting for RMSNorm to complete")

      for (i <- 0 until input.length) {
        val error = relativeError(expected(i), actual_outputs(i))
        println(f"Element $i: expected=${expected(i)}%.6f, actual=${actual_outputs(i)}%.6f, error=$error%.4f%%")
        assert(error < 1.0, f"Element $i error $error%.4f%% exceeds 1%% threshold")
      }
    }
  }

  it should "handle single element vector" in {
    test(new RMSNormAccelerator) { dut =>
      val input = Seq(5.0f)
      val gain = 1.0f
      val expected = softwareRMSNorm(input, gain)

      dut.io.start.poke(false.B)
      dut.io.in_valid.poke(false.B)
      dut.clock.step(1)

      dut.io.start.poke(true.B)
      dut.io.length.poke(input.length.U)
      dut.io.gain.poke(floatToUInt(gain))
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      dut.io.in.poke(floatToUInt(input(0)))
      dut.io.in_valid.poke(true.B)
      dut.clock.step(1)
      dut.io.in_valid.poke(false.B)

      var timeout = 1000
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

      assert(timeout > 0, "Timeout waiting for RMSNorm to complete")
      val error = relativeError(expected(0), actual_output)
      println(f"Single element: expected=${expected(0)}%.6f, actual=$actual_output%.6f, error=$error%.4f%%")
      assert(error < 1.0, f"Error $error%.4f%% exceeds 1%% threshold")
    }
  }
}
