#!/usr/bin/env python3
"""
Compute Optimal LUT Coefficients for Exponential Approximation
- This script computes the optimal coefficients (a, b) of the piecewise linear approximation exp(x) in the range [-10, 10].
- Method: Use Least squares fitting for each segment
- Output: Scala code that can be copied directly into ExponentialApproximator.scala
"""

import numpy as np
import struct
from typing import Tuple, List

# Configuration
NUM_SEGMENTS = 16
INPUT_MIN = -10.0
INPUT_MAX = 10.0
SEGMENT_WIDTH = (INPUT_MAX - INPUT_MIN) / NUM_SEGMENTS  # 1.25

# IEEE 754 conversion functions
def float_to_ieee754_hex(value: float) -> str:
    """Convert float to IEEE 754 hex string for Scala"""
    # Pack as float, unpack as uint32
    packed = struct.pack('>f', value)  # Big-endian
    uint_value = struct.unpack('>I', packed)[0]
    return f'"h{uint_value:08X}".U'

def compute_segment_coefficients(segment_idx: int) -> Tuple[float, float]:
    """
    Compute the optimal (a, b) coefficients of the segment using the least squares method
    For i, approximate: exp(x) ≈ a*x + b
    Args:
        segment_idx: Segment index (0-15)
    Returns:
        (a, b): Linear coefficients (slope, intercept)
    """
    # Segment boundaries
    x_min = INPUT_MIN + segment_idx * SEGMENT_WIDTH
    x_max = x_min + SEGMENT_WIDTH

    # Sample points within segment (dense sampling for better fit)
    num_samples = 100
    x_samples = np.linspace(x_min, x_max, num_samples)

    # Compute actual exp values
    y_samples = np.exp(x_samples)

    # Least squares fitting: y = a*x + b
    # [x 1] * [a; b] = y
    A = np.column_stack([x_samples, np.ones(num_samples)])
    coeffs, residuals, rank, s = np.linalg.lstsq(A, y_samples, rcond=None)

    a, b = coeffs

    # 計算error metrics
    y_approx = a * x_samples + b
    abs_error = np.abs(y_samples - y_approx)
    rel_error = abs_error / np.abs(y_samples)

    max_abs_error = np.max(abs_error)
    max_rel_error = np.max(rel_error)
    mean_abs_error = np.mean(abs_error)

    print(f"  Segment {segment_idx:2d} [{x_min:6.2f}, {x_max:6.2f}]:")
    print(f"    a = {a:12.6e}, b = {b:12.6e}")
    print(f"    Max abs error: {max_abs_error:8.4e}, Max rel error: {max_rel_error:6.2%}")
    print(f"    Mean abs error: {mean_abs_error:8.4e}")

    return a, b

def generate_scala_lut_code(coefficients: List[Tuple[float, float]]) -> str:
    """
    Scala code to generate LUT initial
    """
    # Generate lut_a (slopes)
    scala_code = "  val lut_a = VecInit(Seq(\n"
    scala_code += "    // Segments for negative values (exp approaches 0)\n"

    for i in range(8):
        a, b = coefficients[i]
        x_min = INPUT_MIN + i * SEGMENT_WIDTH
        x_max = x_min + SEGMENT_WIDTH
        hex_a = float_to_ieee754_hex(a)
        comment = f"// Segment {i:2d}: [{x_min:6.2f}, {x_max:6.2f}), a ≈ {a:.4e}"
        scala_code += f"    {hex_a},  {comment}\n"

    scala_code += "\n    // Segments for positive values (exp grows)\n"

    for i in range(8, NUM_SEGMENTS):
        a, b = coefficients[i]
        x_min = INPUT_MIN + i * SEGMENT_WIDTH
        x_max = x_min + SEGMENT_WIDTH
        hex_a = float_to_ieee754_hex(a)
        comment = f"// Segment {i:2d}: [{x_min:6.2f}, {x_max:6.2f}), a ≈ {a:.4e}"
        if i < NUM_SEGMENTS - 1:
            scala_code += f"    {hex_a},  {comment}\n"
        else:
            scala_code += f"    {hex_a}   {comment}\n"

    scala_code += "  ))\n\n"

    # Generate lut_b (intercepts)
    scala_code += "  val lut_b = VecInit(Seq(\n"
    scala_code += "    // intercept of each segment\n"

    for i in range(NUM_SEGMENTS):
        a, b = coefficients[i]
        x_min = INPUT_MIN + i * SEGMENT_WIDTH
        x_max = x_min + SEGMENT_WIDTH
        hex_b = float_to_ieee754_hex(b)
        comment = f"// Segment {i:2d}: [{x_min:6.2f}, {x_max:6.2f}), b ≈ {b:.4e}"
        if i < NUM_SEGMENTS - 1:
            scala_code += f"    {hex_b},  {comment}\n"
        else:
            scala_code += f"    {hex_b}   {comment}\n"

    scala_code += "  ))\n"

    return scala_code

def compute_overall_accuracy(coefficients: List[Tuple[float, float]]):
    """
    Calculate the overall approximation accuracy of all segments
    """
    print("\n" + "="*70)
    print("Overall Accuracy Analysis")
    print("="*70)

    # Test across entire range with dense sampling
    x_test = np.linspace(INPUT_MIN, INPUT_MAX, 10000)
    y_true = np.exp(x_test)
    # Compute approximation for each test point
    y_approx = np.zeros_like(x_test)

    for i, x in enumerate(x_test):
        # Determine segment
        segment_idx = int((x - INPUT_MIN) / SEGMENT_WIDTH)
        segment_idx = max(0, min(NUM_SEGMENTS - 1, segment_idx))

        # linear approximation
        a, b = coefficients[segment_idx]
        y_approx[i] = a * x + b

    # 誤差
    abs_error = np.abs(y_true - y_approx)
    rel_error = abs_error / np.abs(y_true)

    print(f"\nAbsolute Error:")
    print(f"  Max:  {np.max(abs_error):.6e}")
    print(f"  Mean: {np.mean(abs_error):.6e}")
    print(f"  Std:  {np.std(abs_error):.6e}")

    print(f"\nRelative Error:")
    print(f"  Max:  {np.max(rel_error):.6%}")
    print(f"  Mean: {np.mean(rel_error):.6%}")
    print(f"  Std:  {np.std(rel_error):.6%}")

    # 找最壞
    worst_idx = np.argmax(rel_error)
    print(f"\nWorst approximation at x = {x_test[worst_idx]:.4f}:")
    print(f"  True:   {y_true[worst_idx]:.6e}")
    print(f"  Approx: {y_approx[worst_idx]:.6e}")
    print(f"  Error:  {rel_error[worst_idx]:.6%}")

def main():
    """Main function to compute and generate LUT coefficients"""
    print("Computing Optimal LUT Coefficients for Exponential Approximation")
    print(f"\nConfiguration:")
    print(f"  Input range: [{INPUT_MIN}, {INPUT_MAX}]")
    print(f"  Number of segments: {NUM_SEGMENTS}")
    print(f"  Segment width: {SEGMENT_WIDTH}")
    print(f"\nMethod: Least squares linear fitting")

    # Compute coefficients for each segment
    coefficients = []
    for i in range(NUM_SEGMENTS):
        a, b = compute_segment_coefficients(i)
        coefficients.append((a, b))

    # Compute overall accuracy
    compute_overall_accuracy(coefficients)

    # Scala code
    print("Generated Scala Code")
    print("\nCopy the following code into ExponentialApproximator.scala")
    print("-"*70)

    scala_code = generate_scala_lut_code(coefficients)
    print(scala_code)

    print("-"*70)

    # Save to file
    output_file = "lut_coefficients_scala.txt"
    with open(output_file, 'w') as f:
        f.write(scala_code)

    print(f"\nScala code saved to: {output_file}")

    # Save coefficients as CSV for reference
    csv_file = "lut_coefficients.csv"
    with open(csv_file, 'w') as f:
        f.write("segment,x_min,x_max,a,b\n")
        for i, (a, b) in enumerate(coefficients):
            x_min = INPUT_MIN + i * SEGMENT_WIDTH
            x_max = x_min + SEGMENT_WIDTH
            f.write(f"{i},{x_min:.4f},{x_max:.4f},{a:.12e},{b:.12e}\n")

    print(f"Coefficients saved to: {csv_file}")

    print("\n" + "-"*70)
    print("Computation Complete!")


if __name__ == "__main__":
    main()
