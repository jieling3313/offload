// VectorAccumulator Unit Test
// Tests floating-point vector accumulation (sum reduction)

package sfu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class VectorAccumulatorTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("VectorAccumulator")

  // IEEE 754 single-precision float conversion helpers
  // Convert Float to IEEE 754 UInt representation
  def floatToIEEE754(f: Float): BigInt = {
    java.lang.Float.floatToIntBits(f) & 0xFFFFFFFFL
  }

  // Convert IEEE 754 UInt representation to Float
  def ieee754ToFloat(bits: BigInt): Float = {
    java.lang.Float.intBitsToFloat(bits.toInt)
  }

  it should "accumulate a simple vector [1.0, 2.0, 3.0, 4.0]" in {
    test(new VectorAccumulator).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)

      // Test vector: [1.0, 2.0, 3.0, 4.0]
      val testVector = Seq(1.0f, 2.0f, 3.0f, 4.0f)
      val expectedSum = 10.0f

      // Convert to IEEE 754
      val testVectorBits = testVector.map(floatToIEEE754)

      // Initialize
      dut.io.start.poke(false.B)
      dut.io.in_valid.poke(false.B)
      dut.io.in.poke(0.U)
      dut.clock.step(1)

      // Start accumulation
      dut.io.length.poke(testVector.length.U)
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // Feed vector elements
      for (i <- testVectorBits.indices) {
        dut.io.in.poke(testVectorBits(i).U)
        dut.io.in_valid.poke(true.B)
        dut.clock.step(1)
      }
      dut.io.in_valid.poke(false.B)

      // Wait for completion
      var timeout = 10
      while (!dut.io.done.peek().litToBoolean && timeout > 0) {
        dut.clock.step(1)
        timeout -= 1
      }

      // Check result
      val result = ieee754ToFloat(dut.io.out.peek().litValue)
      val error = Math.abs(result - expectedSum) / expectedSum

      println(f"Test Vector: ${testVector.mkString(", ")}")
      println(f"Expected Sum: $expectedSum%.6f")
      println(f"Actual Sum: $result%.6f")
      println(f"Relative Error: ${error * 100}%.6f%%")

      assert(dut.io.done.peek().litToBoolean, "Accumulation should complete")
      assert(error < 0.01, f"Result $result should be close to $expectedSum (error: ${error * 100}%.6f%%)")
    }
  }

  it should "handle zero vector [0.0, 0.0, 0.0]" in {
    test(new VectorAccumulator).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(50)

      val testVector = Seq(0.0f, 0.0f, 0.0f)
      val expectedSum = 0.0f
      val testVectorBits = testVector.map(floatToIEEE754)

      dut.io.start.poke(false.B)
      dut.io.in_valid.poke(false.B)
      dut.clock.step(1)

      dut.io.length.poke(testVector.length.U)
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      for (i <- testVectorBits.indices) {
        dut.io.in.poke(testVectorBits(i).U)
        dut.io.in_valid.poke(true.B)
        dut.clock.step(1)
      }
      dut.io.in_valid.poke(false.B)

      var timeout = 10
      while (!dut.io.done.peek().litToBoolean && timeout > 0) {
        dut.clock.step(1)
        timeout -= 1
      }

      val result = ieee754ToFloat(dut.io.out.peek().litValue)
      println(f"Zero vector test - Result: $result%.6f (Expected: $expectedSum%.6f)")

      assert(dut.io.done.peek().litToBoolean)
      assert(result == expectedSum, f"Result should be exactly 0.0")
    }
  }

  it should "accumulate positive and negative values [5.0, -3.0, 2.0, -1.0]" in {
    test(new VectorAccumulator).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)

      val testVector = Seq(5.0f, -3.0f, 2.0f, -1.0f)
      val expectedSum = 3.0f
      val testVectorBits = testVector.map(floatToIEEE754)

      dut.io.start.poke(false.B)
      dut.io.in_valid.poke(false.B)
      dut.clock.step(1)

      dut.io.length.poke(testVector.length.U)
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      for (i <- testVectorBits.indices) {
        dut.io.in.poke(testVectorBits(i).U)
        dut.io.in_valid.poke(true.B)
        dut.clock.step(1)
      }
      dut.io.in_valid.poke(false.B)

      var timeout = 10
      while (!dut.io.done.peek().litToBoolean && timeout > 0) {
        dut.clock.step(1)
        timeout -= 1
      }

      val result = ieee754ToFloat(dut.io.out.peek().litValue)
      val error = Math.abs(result - expectedSum) / Math.abs(expectedSum)

      println(f"Mixed sign test")
      println(f"Test Vector: ${testVector.mkString(", ")}")
      println(f"Expected: $expectedSum%.6f, Actual: $result%.6f")
      println(f"Relative Error: ${error * 100}%.6f%%")

      assert(dut.io.done.peek().litToBoolean)
      assert(error < 0.01, f"Result should be close to expected (error: ${error * 100}%.6f%%)")
    }
  }

  it should "accumulate fractional values [0.1, 0.2, 0.3, 0.4]" in {
    test(new VectorAccumulator).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)

      val testVector = Seq(0.1f, 0.2f, 0.3f, 0.4f)
      val expectedSum = 1.0f
      val testVectorBits = testVector.map(floatToIEEE754)

      dut.io.start.poke(false.B)
      dut.io.in_valid.poke(false.B)
      dut.clock.step(1)

      dut.io.length.poke(testVector.length.U)
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      for (i <- testVectorBits.indices) {
        dut.io.in.poke(testVectorBits(i).U)
        dut.io.in_valid.poke(true.B)
        dut.clock.step(1)
      }
      dut.io.in_valid.poke(false.B)

      var timeout = 10
      while (!dut.io.done.peek().litToBoolean && timeout > 0) {
        dut.clock.step(1)
        timeout -= 1
      }

      val result = ieee754ToFloat(dut.io.out.peek().litValue)
      val error = Math.abs(result - expectedSum) / expectedSum

      println(f"Fractional values test")
      println(f"Test Vector: ${testVector.mkString(", ")}")
      println(f"Expected: $expectedSum%.6f, Actual: $result%.6f")
      println(f"Relative Error: ${error * 100}%.6f%%")

      assert(dut.io.done.peek().litToBoolean)
      assert(error < 0.01, f"Result should be close to expected (error: ${error * 100}%.6f%%)")
    }
  }

  it should "handle single element vector [42.0]" in {
    test(new VectorAccumulator).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(50)

      val testVector = Seq(42.0f)
      val expectedSum = 42.0f
      val testVectorBits = testVector.map(floatToIEEE754)

      dut.io.start.poke(false.B)
      dut.io.in_valid.poke(false.B)
      dut.clock.step(1)

      dut.io.length.poke(testVector.length.U)
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      for (i <- testVectorBits.indices) {
        dut.io.in.poke(testVectorBits(i).U)
        dut.io.in_valid.poke(true.B)
        dut.clock.step(1)
      }
      dut.io.in_valid.poke(false.B)

      var timeout = 10
      while (!dut.io.done.peek().litToBoolean && timeout > 0) {
        dut.clock.step(1)
        timeout -= 1
      }

      val result = ieee754ToFloat(dut.io.out.peek().litValue)
      val error = Math.abs(result - expectedSum) / expectedSum

      println(f"Single element test")
      println(f"Expected: $expectedSum%.6f, Actual: $result%.6f")
      println(f"Relative Error: ${error * 100}%.6f%%")

      assert(dut.io.done.peek().litToBoolean)
      assert(error < 0.001, f"Result should exactly match input (error: ${error * 100}%.6f%%)")
    }
  }

  it should "handle multiple accumulations sequentially" in {
    test(new VectorAccumulator).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(200)

      // First accumulation
      val testVector1 = Seq(1.0f, 2.0f)
      val expectedSum1 = 3.0f
      val testVectorBits1 = testVector1.map(floatToIEEE754)

      dut.io.start.poke(false.B)
      dut.io.in_valid.poke(false.B)
      dut.clock.step(1)

      dut.io.length.poke(testVector1.length.U)
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      for (i <- testVectorBits1.indices) {
        dut.io.in.poke(testVectorBits1(i).U)
        dut.io.in_valid.poke(true.B)
        dut.clock.step(1)
      }
      dut.io.in_valid.poke(false.B)

      var timeout = 10
      while (!dut.io.done.peek().litToBoolean && timeout > 0) {
        dut.clock.step(1)
        timeout -= 1
      }

      val result1 = ieee754ToFloat(dut.io.out.peek().litValue)
      println(f"First accumulation - Expected: $expectedSum1%.6f, Actual: $result1%.6f")
      assert(Math.abs(result1 - expectedSum1) / expectedSum1 < 0.01)

      // Wait a bit
      dut.clock.step(5)

      // Second accumulation
      val testVector2 = Seq(5.0f, 6.0f, 7.0f)
      val expectedSum2 = 18.0f
      val testVectorBits2 = testVector2.map(floatToIEEE754)

      dut.io.length.poke(testVector2.length.U)
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      for (i <- testVectorBits2.indices) {
        dut.io.in.poke(testVectorBits2(i).U)
        dut.io.in_valid.poke(true.B)
        dut.clock.step(1)
      }
      dut.io.in_valid.poke(false.B)

      timeout = 10
      while (!dut.io.done.peek().litToBoolean && timeout > 0) {
        dut.clock.step(1)
        timeout -= 1
      }

      val result2 = ieee754ToFloat(dut.io.out.peek().litValue)
      println(f"Second accumulation - Expected: $expectedSum2%.6f, Actual: $result2%.6f")
      assert(Math.abs(result2 - expectedSum2) / expectedSum2 < 0.01)
    }
  }
}
