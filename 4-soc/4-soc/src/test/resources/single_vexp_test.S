# Single VEXP Instruction Test
# Tests: VEXP(1.0) -> exp(1.0) ≈ 2.718

.section .text
.globl _start

_start:
    # Test: VEXP(1.0) -> exp(1.0) ≈ 2.718
    # Load 1.0 (0x3F800000) into a0
    lui a0, 0x3F800

    # Execute VEXP: a1 = exp(a0)
    # Custom-1 opcode: 0x2B, funct7=1 (VEXP)
    .insn r 0x2B, 0, 1, a1, a0, x0

    # Store result to memory at 0x2000
    lui t0, 0x2
    sw a1, 0(t0)

    # Write completion marker (1) to 0x2004
    li t1, 1
    sw t1, 4(t0)

halt:
    j halt
