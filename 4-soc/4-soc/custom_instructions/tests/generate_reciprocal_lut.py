#!/usr/bin/env python3
"""
Generate Reciprocal Lookup Table for FPDivider Initial Guess

This script generates a LUT for improving the initial guess in Newton-Raphson
division. The LUT maps mantissa values to their reciprocals.

Strategy:
- Use mantissa high 4 bits (16 entries)
- For each mantissa m in [1.0, 2.0), compute 1/m
- Store as IEEE 754 FP32 values
"""

import struct

def float_to_chisel_hex(f):
    """Convert float to Chisel hex string format: "hXXXXXXXX".U(32.W)"""
    packed = struct.pack('>f', f)
    uint_value = struct.unpack('>I', packed)[0]
    return f'"h{uint_value:08x}".U(32.W)'

def generate_scala_code(bits=4):
    """Generate Scala code for the reciprocal LUT matching FPArithmetic.scala format"""
    num_entries = 2 ** bits

    scala_code = ""
    scala_code += "  val reciprocal_lut = VecInit(Seq(\n"

    for i in range(num_entries):
        mantissa_shifted = i << (23 - bits)
        mantissa_frac = mantissa_shifted / (2.0 ** 23)
        mantissa_value = 1.0 + mantissa_frac
        reciprocal = 1.0 / mantissa_value
        hex_value = float_to_chisel_hex(reciprocal)

        comma = "," if i < num_entries - 1 else ""
        scala_code += f'    {hex_value}{comma}  // [{i:2d}] 1/{mantissa_value:.6f} = {reciprocal:.6f}\n'

    scala_code += "  ))\n\n"

    scala_code += "  /** \n"
    scala_code += "   * Stage 0: Initial Reciprocal Guess (Combinational) \n"
    scala_code += "   * Method: LUT-based mantissa + exponent flip\n"
    scala_code += "   *\n"
    scala_code += "   * For b with exponent exp_b, reciprocal 1/b has exponent (254 - exp_b)\n"
    scala_code += "   * Mantissa is looked up from LUT using high 4 bits of b's mantissa\n"
    scala_code += "   *\n"
    scala_code += "   */\n\n"

    scala_code += "  val exp_b = io.b(30, 23)\n\n"

    scala_code += "  // Extract mantissa index (high 4 bits of mantissa)\n"
    scala_code += f"  val mant_idx = io.b(22, {23-bits})  // Bits [22:{23-bits}]\n\n"

    scala_code += "  // Lookup reciprocal mantissa from LUT (full IEEE 754 value)\n"
    scala_code += "  val recip_mant_lut = reciprocal_lut(mant_idx)\n\n"

    scala_code += "  // Extract exponent from LUT to check if reciprocal < 1.0\n"
    scala_code += "  val lut_exp = recip_mant_lut(30, 23)\n\n"

    scala_code += "  // Reciprocal exponent calculation:\n"
    scala_code += "  // If LUT value has exp=126 (reciprocal < 1.0), adjust by -1\n"
    scala_code += "  // If LUT value has exp=127 (reciprocal >= 1.0), use standard formula\n"
    scala_code += "  val exp_adjust = Mux(lut_exp === 126.U, 1.U, 0.U)\n"
    scala_code += "  val recip_exp = 254.U - exp_b - exp_adjust\n\n"

    scala_code += "  // Extract mantissa component from LUT result\n"
    scala_code += "  val lut_mant = recip_mant_lut(22, 0)\n\n"

    scala_code += "  // Initial guess: sign=0, adjusted exponent, LUT mantissa\n"
    scala_code += "  val x0 = Cat(0.U(1.W), recip_exp, lut_mant)\n"

    return scala_code

def main():
    """Main function to generate reciprocal LUT"""
    print("Generating Reciprocal LUT for FPDivider")
    print(f"Configuration: 4-bit mantissa index (16 entries)")
    print()

    # Generate Scala code
    scala_code = generate_scala_code(bits=4)

    # Save to file
    output_file = "reciprocal_lut_scala.txt"
    with open(output_file, 'w') as f:
        f.write(scala_code)

    print(f"Scala code saved to: {output_file}")
    print()
    print("Copy the generated code into FPArithmetic.scala")
    print("-" * 70)
    print(scala_code)
    print("-" * 70)

if __name__ == "__main__":
    main()
