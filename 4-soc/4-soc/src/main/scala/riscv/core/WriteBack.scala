// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package riscv.core

import chisel3._
import chisel3.util._
import riscv.Parameters

class WriteBack extends Module {
  val io = IO(new Bundle() {
    val instruction_address = Input(UInt(Parameters.AddrWidth))
    val alu_result          = Input(UInt(Parameters.DataWidth))
    val memory_read_data    = Input(UInt(Parameters.DataWidth))
    val regs_write_source   = Input(UInt(2.W))
    val csr_read_data       = Input(UInt(Parameters.DataWidth))

    val regs_write_data = Output(UInt(Parameters.DataWidth))
  })
  // Debug: Track writeback source and data
  when(io.regs_write_source === CustomRegWriteSource.SFU) {
    printf("[WriteBack] SFU writeback: alu_result=0x%x, regs_write_data=0x%x\n",
      io.alu_result, io.regs_write_data)
  }

  io.regs_write_data := MuxLookup(
    io.regs_write_source,
    io.alu_result
  )(
    IndexedSeq(
      RegWriteSource.Memory                 -> io.memory_read_data,
      RegWriteSource.CSR                    -> io.csr_read_data,
      RegWriteSource.NextInstructionAddress -> (io.instruction_address + 4.U),
      CustomRegWriteSource.SFU              -> io.alu_result  // SFU result comes through alu_result
    )
  )
}
