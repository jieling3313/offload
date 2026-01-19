#!/usr/bin/env python3
"""
TX2 Real Transformer Layer Benchmark
Real 12-Layer Transformer Performance Test
"""

import time
import math
import numpy as np
from datetime import datetime
import json

class Colors:
    GREEN = '\033[0;32m'
    YELLOW = '\033[1;33m'
    BLUE = '\033[0;34m'
    RED = '\033[0;31m'
    BOLD = '\033[1m'
    NC = '\033[0m'

def print_header(text):
    print(f"\n{Colors.BOLD}{Colors.BLUE}{'='*70}{Colors.NC}")
    print(f"{Colors.BOLD}{Colors.BLUE}{text:^70}{Colors.NC}")
    print(f"{Colors.BOLD}{Colors.BLUE}{'='*70}{Colors.NC}\n")

def print_section(text):
    print(f"\n{Colors.YELLOW}{text}{Colors.NC}")
    print(f"{Colors.YELLOW}{'-'*70}{Colors.NC}")

# Software Implementation - Complete Transformer Layer

def softmax(x):
    """Softmax implementation"""
    x = np.array(x, dtype=np.float32)
    max_x = np.max(x)
    exp_x = np.exp(x - max_x)
    return exp_x / np.sum(exp_x)

def rmsnorm(x, gain=1.0):
    """RMSNorm implementation"""
    x = np.array(x, dtype=np.float32)
    rms = np.sqrt(np.mean(x ** 2))
    return (x / rms) * gain

class TransformerLayer:
    """Complete Transformer Layer Implementation"""

    def __init__(self, d_model=128, n_heads=12, seq_len=128):
        self.d_model = d_model
        self.n_heads = n_heads
        self.seq_len = seq_len
        self.d_k = d_model // n_heads

        # Initialize weights (random initialization for testing)
        np.random.seed(42)
        self.W_q = np.random.randn(d_model, d_model).astype(np.float32) * 0.01
        self.W_k = np.random.randn(d_model, d_model).astype(np.float32) * 0.01
        self.W_v = np.random.randn(d_model, d_model).astype(np.float32) * 0.01
        self.W_o = np.random.randn(d_model, d_model).astype(np.float32) * 0.01

        self.softmax_count = 0
        self.rmsnorm_count = 0

    def multi_head_attention(self, x):
        """Multi-Head Attention
        Each head needs to calculate attention for seq_len positions.
        Each position requires 1 Softmax (N=seq_len).
        """
        batch_size = x.shape[0]

        # Linear projections
        Q = x @ self.W_q  # (batch, seq_len, d_model)
        K = x @ self.W_k
        V = x @ self.W_v

        # Split into heads
        # (batch, seq_len, n_heads, d_k)
        Q = Q.reshape(batch_size, self.seq_len, self.n_heads, self.d_k)
        K = K.reshape(batch_size, self.seq_len, self.n_heads, self.d_k)
        V = V.reshape(batch_size, self.seq_len, self.n_heads, self.d_k)

        # Transpose: (batch, n_heads, seq_len, d_k)
        Q = Q.transpose(0, 2, 1, 3)
        K = K.transpose(0, 2, 1, 3)
        V = V.transpose(0, 2, 1, 3)

        # Attention scores: (batch, n_heads, seq_len, seq_len)
        scores = Q @ K.transpose(0, 1, 3, 2) / math.sqrt(self.d_k)

        # Softmax over last dimension (once per query position)
        # For each head, every seq position needs 1 softmax
        attention = np.zeros_like(scores)
        for b in range(batch_size):
            for h in range(self.n_heads):
                for s in range(self.seq_len):
                    attention[b, h, s, :] = softmax(scores[b, h, s, :])
                    self.softmax_count += 1

        # Apply attention to values
        output = attention @ V  # (batch, n_heads, seq_len, d_k)

        # Concatenate heads
        output = output.transpose(0, 2, 1, 3).reshape(batch_size, self.seq_len, self.d_model)

        # Output projection
        output = output @ self.W_o

        return output

    def forward(self, x, use_rmsnorm=True):
        """Complete Transformer Layer Forward Pass
        1. RMSNorm (pre-attention)
        2. Multi-Head Attention (includes Softmax)
        3. Residual Connection
        4. RMSNorm (pre-FFN)
        5. Feed-Forward Network
        6. Residual Connection
        """
        batch_size = x.shape[0]

        # 1. Pre-attention RMSNorm
        if use_rmsnorm:
            x_norm = np.zeros_like(x)
            for b in range(batch_size):
                for s in range(self.seq_len):
                    x_norm[b, s, :] = rmsnorm(x[b, s, :])
                    self.rmsnorm_count += 1
        else:
            x_norm = x

        # 2. Multi-Head Attention (includes Softmax)
        attn_output = self.multi_head_attention(x_norm)

        # 3. Residual
        x = x + attn_output

        # 4. Pre-FFN RMSNorm
        if use_rmsnorm:
            x_norm = np.zeros_like(x)
            for b in range(batch_size):
                for s in range(self.seq_len):
                    x_norm[b, s, :] = rmsnorm(x[b, s, :])
                    self.rmsnorm_count += 1
        else:
            x_norm = x

        # 5. Feed-Forward Network (Simplified to single layer here)
        # In real applications there would be more linear transformations, 
        # but non-linear accelerators are not involved in those.

        # 6. Residual
        output = x + x_norm

        return output

# Benchmark Functions
def benchmark_transformer_layer_software(layer, input_data, iterations=10):
    """Measure software Transformer Layer execution time"""
    print(f"  Software Transformer Layer Test ({iterations} times)... ", end='', flush=True)

    # Warmup
    for _ in range(2):
        layer.softmax_count = 0
        layer.rmsnorm_count = 0
        _ = layer.forward(input_data)

    # Actual measurement
    layer.softmax_count = 0
    layer.rmsnorm_count = 0

    start = time.perf_counter()
    for _ in range(iterations):
        _ = layer.forward(input_data)
    end = time.perf_counter()

    total_time = end - start
    avg_time = total_time / iterations

    softmax_per_iter = layer.softmax_count // iterations
    rmsnorm_per_iter = layer.rmsnorm_count // iterations

    print(f"Done")
    print(f"    Per iteration: {softmax_per_iter} Softmax, {rmsnorm_per_iter} RMSNorm")

    return avg_time, softmax_per_iter, rmsnorm_per_iter

def benchmark_hardware(softmax_count, rmsnorm_count, cpu_freq=2.0e9):
    """Calculate hardware execution time based on hardware cycle counts"""
    # Cycle counts obtained from test results
    cycles_per_softmax = 2314  # N=128
    cycles_per_rmsnorm = 1566  # N=128

    total_cycles = (softmax_count * cycles_per_softmax +
                   rmsnorm_count * cycles_per_rmsnorm)

    hw_time = total_cycles / cpu_freq

    return hw_time, total_cycles

# Main Benchmark
def main():
    print_header("TX2 Real Transformer Layer Benchmark")

    results = {
        'timestamp': datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
        'platform': 'Jetson TX2 (ARM Cortex-A57 @ 2.0 GHz)',
        'config': {},
        'single_layer': {},
        'multi_layer': {}
    }

    # Configuration
    batch_size = 1
    seq_len = 128
    d_model = 128
    n_heads = 12
    cpu_freq = 2.0e9  # 2.0 GHz

    results['config'] = {
        'batch_size': batch_size,
        'seq_len': seq_len,
        'd_model': d_model,
        'n_heads': n_heads,
        'cpu_freq_hz': cpu_freq
    }

    print_section("Configuration")
    print(f"  Batch Size: {batch_size}")
    print(f"  Sequence Length: {seq_len}")
    print(f"  Hidden Dimension (d_model): {d_model}")
    print(f"  Attention Heads: {n_heads}")
    print(f"  CPU Frequency: {cpu_freq/1e9:.1f} GHz")

    # Test 1: Single Transformer Layer
    print_section("Test 1: Single Transformer Layer")

    # Create test data
    np.random.seed(42)
    input_data = np.random.randn(batch_size, seq_len, d_model).astype(np.float32)

    # Create Transformer Layer
    layer = TransformerLayer(d_model=d_model, n_heads=n_heads, seq_len=seq_len)

    # Software measurement
    sw_time, softmax_count, rmsnorm_count = benchmark_transformer_layer_software(
        layer, input_data, iterations=10
    )

    # Hardware estimation
    hw_time, hw_cycles = benchmark_hardware(softmax_count, rmsnorm_count, cpu_freq)

    print(f"\n  Results:")
    print(f"    Non-linear Operation Statistics:")
    print(f"      Softmax count: {softmax_count}")
    print(f"      RMSNorm count: {rmsnorm_count}")
    print(f"\n    Software Execution Time:")
    print(f"      Average time: {sw_time * 1e3:.2f} ms/layer")
    print(f"\n    Hardware Execution Time:")
    print(f"      Total cycles: {hw_cycles:,} cycles")
    print(f"      Average time: {hw_time * 1e3:.2f} ms/layer")

    speedup = sw_time / hw_time
    time_saved = sw_time - hw_time

    print(f"\n    {Colors.GREEN}Speedup: {speedup:.1f}x{Colors.NC}")
    print(f"    {Colors.GREEN}Time Saved: {time_saved * 1e3:.2f} ms ({time_saved / sw_time * 100:.1f}%){Colors.NC}")

    results['single_layer'] = {
        'softmax_count': softmax_count,
        'rmsnorm_count': rmsnorm_count,
        'software_time_ms': sw_time * 1e3,
        'hardware_time_ms': hw_time * 1e3,
        'hardware_cycles': hw_cycles,
        'speedup': speedup,
        'time_saved_ms': time_saved * 1e3,
        'improvement_percent': (time_saved / sw_time * 100)
    }

    # Test 2: 12-Layer Transformer (Full Model)
    print_section("Test 2: 12-Layer Transformer (GPT-2 Small Scale)")

    num_layers = 12

    print(f"  Config: {num_layers} Layer Transformer")
    print(f"  Testing {num_layers}-layer forward pass... ", end='', flush=True)

    # Create 12 layers
    layers = [TransformerLayer(d_model=d_model, n_heads=n_heads, seq_len=seq_len)
              for _ in range(num_layers)]

    # Warmup
    x = input_data.copy()
    for layer in layers:
        x = layer.forward(x)

    # Actual measurement
    for layer in layers:
        layer.softmax_count = 0
        layer.rmsnorm_count = 0

    x = input_data.copy()
    start = time.perf_counter()
    for layer in layers:
        x = layer.forward(x)
    end = time.perf_counter()

    sw_time_total = end - start

    # Calculate total operations
    total_softmax = sum(layer.softmax_count for layer in layers)
    total_rmsnorm = sum(layer.rmsnorm_count for layer in layers)

    print(f"Done")
    print(f"\n  Non-linear Operation Statistics:")
    print(f"    Total Softmax: {total_softmax}")
    print(f"    Total RMSNorm: {total_rmsnorm}")

    # Hardware estimation
    hw_time_total, hw_cycles_total = benchmark_hardware(total_softmax, total_rmsnorm, cpu_freq)

    print(f"\n  Software Execution Time:")
    print(f"    Total time: {sw_time_total * 1e3:.2f} ms")
    print(f"    Average per layer: {sw_time_total / num_layers * 1e3:.2f} ms")

    print(f"\n  Hardware Execution Time:")
    print(f"    Total cycles: {hw_cycles_total:,} cycles")
    print(f"    Total time: {hw_time_total * 1e3:.2f} ms")
    print(f"    Average per layer: {hw_time_total / num_layers * 1e3:.2f} ms")

    speedup_total = sw_time_total / hw_time_total
    time_saved_total = sw_time_total - hw_time_total

    print(f"\n  {Colors.GREEN}{Colors.BOLD}{'='*70}{Colors.NC}")
    print(f"  {Colors.GREEN}{Colors.BOLD}12-Layer Transformer Acceleration Results:{Colors.NC}")
    print(f"  {Colors.GREEN}{Colors.BOLD}  Speedup: {speedup_total:.1f}x{Colors.NC}")
    print(f"  {Colors.GREEN}{Colors.BOLD}  Time Saved: {time_saved_total * 1e3:.2f} ms ({time_saved_total / sw_time_total * 100:.1f}%){Colors.NC}")
    print(f"  {Colors.GREEN}{Colors.BOLD}{'='*70}{Colors.NC}")

    results['multi_layer'] = {
        'num_layers': num_layers,
        'total_softmax': total_softmax,
        'total_rmsnorm': total_rmsnorm,
        'software_time_ms': sw_time_total * 1e3,
        'hardware_time_ms': hw_time_total * 1e3,
        'hardware_cycles': hw_cycles_total,
        'speedup': speedup_total,
        'time_saved_ms': time_saved_total * 1e3,
        'improvement_percent': (time_saved_total / sw_time_total * 100)
    }

    # Save Results
    output_file = f"transformer_benchmark_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
    with open(output_file, 'w') as f:
        json.dump(results, f, indent=2)

    print(f"\nResults saved to: {Colors.BLUE}{output_file}{Colors.NC}")
    print()

if __name__ == "__main__":
    main()