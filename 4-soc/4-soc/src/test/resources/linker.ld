/* RISC-V Linker Script for Custom Instruction Test */

OUTPUT_ARCH("riscv")
ENTRY(_start)

MEMORY
{
  /* Instruction memory starts at 0x1000 (matching MyCPU Parameters.EntryAddress) */
  ROM (rx)  : ORIGIN = 0x1000, LENGTH = 4K

  /* Data memory starts at 0x2000 */
  RAM (rw)  : ORIGIN = 0x2000, LENGTH = 4K
}

SECTIONS
{
  /* Code section */
  .text : {
    *(.text)
    *(.text.*)
  } > ROM

  /* Data section */
  .data : {
    *(.data)
    *(.data.*)
  } > RAM

  /* BSS section */
  .bss : {
    *(.bss)
    *(.bss.*)
  } > RAM

  /* Discard unnecessary sections */
  /DISCARD/ : {
    *(.comment)
    *(.riscv.attributes)
    *(.note.*)
  }
}
