# Two Instructions Test: VEXP + VRSQRT
# Tests sequential execution of two custom Instructions

.section .text
.globl _start

_start:
    # Test 1: VEXP(1.0) -> exp(1.0) ≈ 2.718
    lui a0, 0x3F800
    .insn r 0x2B, 0, 1, a1, a0, x0

    # Store first result
    lui t0, 0x2
    sw a1, 0(t0)

    # Test 2: VRSQRT(4.0) -> 1/sqrt(4.0) = 0.5
    lui a2, 0x40800
    .insn r 0x2B, 0, 2, a3, a2, x0

    # Store second result
    sw a3, 4(t0)

    # Completion marker
    li t1, 1
    sw t1, 8(t0)

halt:
    j halt
