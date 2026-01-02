// Vector Accumulator for reduction operations (sum, mean, etc.)

package sfu

import chisel3._
import chisel3.util._

/**
 * Vector Accumulator for Reduction Operations
 *
 * Performs streaming accumulation of vector elements
 * Used for operations like:
 * - Sum reduction (for Softmax normalization)
 * - Mean calculation (for RMSNorm)
 * - Dot product
 *
 * Supports IEEE 754 single-precision floating-point
 *
 * Latency: N cycles for N elements (streaming)
 */
class VectorAccumulator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())        // Start accumulation
    val in = Input(UInt(32.W))       // Input element (IEEE 754 float)
    val in_valid = Input(Bool())     // Input valid
    val length = Input(UInt(16.W))   // Vector length
    val out = Output(UInt(32.W))     // Accumulated result
    val done = Output(Bool())        // Accumulation complete
  })

  // States
  val sIdle :: sAccumulating :: sDone :: Nil = Enum(3)
  val state = RegInit(sIdle)

  // Accumulator register
  val accumulator = RegInit(0.U(32.W))

  // Element counter
  val count = RegInit(0.U(16.W))
  val target_length = RegInit(0.U(16.W))

  // Floating-point adder instance
  val fp_adder = Module(new FPAdder)

  // Default values for FP adder
  fp_adder.io.a := 0.U
  fp_adder.io.b := 0.U

  // Default outputs
  io.out := accumulator
  io.done := false.B

  // State machine
  switch(state) {
    is(sIdle) {
      when(io.start) {
        accumulator := 0.U  // Reset accumulator (0.0 in IEEE 754 is 0x00000000)
        count := 0.U
        target_length := io.length
        state := sAccumulating
      }
    }

    is(sAccumulating) {
      when(io.in_valid) {
        // Add current input to accumulator
        fp_adder.io.a := accumulator
        fp_adder.io.b := io.in
        accumulator := fp_adder.io.result

        count := count + 1.U

        when(count === target_length - 1.U) {
          state := sDone
        }
      }
    }

    is(sDone) {
      io.done := true.B
      state := sIdle
    }
  }
}

/**
 * Vector Mean Calculator
 * Computes mean of a vector by accumulating sum and dividing by length
 * Used in RMSNorm calculation
 */

class VectorMean extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val in = Input(UInt(32.W))       // Input element
    val in_valid = Input(Bool())
    val length = Input(UInt(16.W))
    val out = Output(UInt(32.W))     // Mean value
    val done = Output(Bool())
  })

  // Use vector accumulator for sum
  val accumulator = Module(new VectorAccumulator)

  accumulator.io.start := io.start
  accumulator.io.in := io.in
  accumulator.io.in_valid := io.in_valid
  accumulator.io.length := io.length

  // States
  val sIdle :: sAccumulating :: sDividing :: sDone :: Nil = Enum(4)
  val state = RegInit(sIdle)

  val sum = RegInit(0.U(32.W))
  val count = RegInit(0.U(16.W))

  io.done := false.B
  io.out := 0.U

  switch(state) {
    is(sIdle) {
      when(io.start) {
        count := io.length
        state := sAccumulating
      }
    }

    is(sAccumulating) {
      when(accumulator.io.done) {
        sum := accumulator.io.out
        state := sDividing
      }
    }

    is(sDividing) {
      // Divide sum by count
      // TODO: Implement FP division or use reciprocal approximation
      io.out := sum
      state := sDone
    }

    is(sDone) {
      io.done := true.B
      state := sIdle
    }
  }
}


