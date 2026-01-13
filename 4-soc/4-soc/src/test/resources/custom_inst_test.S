# RISC-V Assembly Test for Custom SFU Instructions

.section .text
.globl _start

_start:
    # Test 1: VEXP Instruction
    # Calculate exp(1.0) ≈ 2.718

    # Load 1.0 (IEEE 754: 0x3F800000) into register a0
    lui  a0, 0x3F800      # Load upper 20 bits

    # Execute VEXP a1, a0 (a1 = exp(a0))
    # Custom instruction encoding:
    # .insn r opcode, func3, func7, rd, rs1, rs2
    # opcode=0x2B (custom-1), func3=0, func7=1 (VEXP)
    .insn r 0x2B, 0, 1, a1, a0, x0

    # Store result to memory address 0x2000
    lui  t0, 0x2          # t0 = 0x2000
    sw   a1, 0(t0)        # mem[0x2000] = a1 (exp result)

    # Test 2: VRSQRT Instruction
    # Calculate 1/sqrt(4.0) = 0.5

    # Load 4.0 (IEEE 754: 0x40800000) into register a2
    lui  a2, 0x40800      # Load upper 20 bits

    # Execute VRSQRT a3, a2 (a3 = 1/sqrt(a2))
    # opcode=0x2B, func3=0, func7=2 (VRSQRT)
    .insn r 0x2B, 0, 2, a3, a2, x0

    # Store result to memory address 0x2004
    sw   a3, 4(t0)        # mem[0x2004] = a3 (rsqrt result)

    # Test 3: VEXP with different value
    # Calculate exp(2.0) ≈ 7.389

    # Load 2.0 (IEEE 754: 0x40000000) into register a4
    lui  a4, 0x40000      # Load upper 20 bits

    # Execute VEXP a5, a4 (a5 = exp(a4))
    .insn r 0x2B, 0, 1, a5, a4, x0

    # Store result to memory address 0x2008
    sw   a5, 8(t0)        # mem[0x2008] = a5 (exp(2.0))

    # Test 4: VRSQRT with different value
    # Calculate 1/sqrt(9.0) ≈ 0.333
    # 計算 1/sqrt(9.0) ≈ 0.333

    # Load 9.0 (IEEE 754: 0x41100000) into register a6
    lui  a6, 0x41100      # Load upper 20 bits

    # Execute VRSQRT a7, a6 (a7 = 1/sqrt(a6))
    .insn r 0x2B, 0, 2, a7, a6, x0

    # Store result to memory address 0x200C
    sw   a7, 12(t0)       # mem[0x200C] = a7 (rsqrt(9.0))

    # Test completion marker
    # Store 0x00000001 to 0x2010 to indicate test completion
    addi t1, x0, 1
    sw   t1, 16(t0)       # mem[0x2010] = 0x00000001 (completion marker)

    # Infinite loop (halt simulation)
halt:
    j halt
