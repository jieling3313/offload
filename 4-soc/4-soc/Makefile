# SPDX-License-Identifier: MIT
# MyCPU is freely redistributable under the MIT License. See the file
# "LICENSE" for information on usage and redistribution of this file.

# Include common build utilities
include build.mk

# RISC-V Toolchain Configuration

# RISC-V Toolchain Path
RISCV_PREFIX = /home/jieling/riscv-none-elf-gcc/bin/riscv-none-elf-

# Toolchain Binaries
AS      = $(RISCV_PREFIX)as
LD      = $(RISCV_PREFIX)ld
OBJCOPY = $(RISCV_PREFIX)objcopy
OBJDUMP = $(RISCV_PREFIX)objdump
SIZE    = $(RISCV_PREFIX)size

# Directories
TEST_RES_DIR    = src/test/resources
MAIN_RES_DIR    = src/main/resources
BUILD_DIR       = build

# Assembly Source Files
CUSTOM_INST_ASM_SRC     = $(TEST_RES_DIR)/custom_inst_test.S
SINGLE_VEXP_ASM_SRC     = $(TEST_RES_DIR)/single_vexp_test.S
TWO_INST_ASM_SRC        = $(TEST_RES_DIR)/two_inst_test.S
LINKER_SCRIPT           = $(TEST_RES_DIR)/linker.ld

# Output Files
CUSTOM_INST_OBJ     = $(BUILD_DIR)/custom_inst_test.o
CUSTOM_INST_ELF     = $(BUILD_DIR)/custom_inst_test.elf
CUSTOM_INST_BIN     = $(BUILD_DIR)/custom_inst_test.bin
CUSTOM_INST_ASMBIN  = $(MAIN_RES_DIR)/custom_inst_test.asmbin
CUSTOM_INST_DUMP    = $(BUILD_DIR)/custom_inst_test.dump

SINGLE_VEXP_OBJ     = $(BUILD_DIR)/single_vexp_test.o
SINGLE_VEXP_ELF     = $(BUILD_DIR)/single_vexp_test.elf
SINGLE_VEXP_BIN     = $(BUILD_DIR)/single_vexp_test.bin
SINGLE_VEXP_ASMBIN  = $(MAIN_RES_DIR)/single_vexp_test.asmbin

TWO_INST_OBJ        = $(BUILD_DIR)/two_inst_test.o
TWO_INST_ELF        = $(BUILD_DIR)/two_inst_test.elf
TWO_INST_BIN        = $(BUILD_DIR)/two_inst_test.bin
TWO_INST_ASMBIN     = $(MAIN_RES_DIR)/two_inst_test.asmbin

# Compiler/Linker Flags
ASFLAGS  = -march=rv32i -mabi=ilp32
LDFLAGS  = -T $(LINKER_SCRIPT)

# Test Commands
test:
	sbt test

# SFU Unit Tests
# Run all SFU unit tests
test-sfu:
	@echo "================================================================"
	@echo "                 Running SFU Unit Tests"
	@echo "================================================================"
	sbt "testOnly sfu.*"

# Individual SFU module tests
test-sfu-exp:
	@echo "Testing ExponentialApproximator (VEXP)..."
	sbt "testOnly sfu.ExponentialApproximatorTest"
 
	@echo "Testing InvSqrt (VRSQRT)..."
	sbt "testOnly sfu.InvSqrtTest"

test-sfu-accumulator:
	@echo "Testing VectorAccumulator (VREDSUM)..."
	sbt "testOnly sfu.VectorAccumulatorTest"

test-sfu-divider:
	@echo "Testing FPDivider..."
	sbt "testOnly sfu.FPDividerTest"

test-sfu-rmsnorm:
	@echo "Testing RMSNorm Accelerator..."
	sbt "testOnly sfu.RMSNormTest"

test-sfu-softmax:
	@echo "Testing Softmax Accelerator..."
	sbt "testOnly sfu.SoftmaxTest"

test-sfu-integration:
	@echo "Testing SpecialFunctionUnit Integration..."
	sbt "testOnly sfu.SpecialFunctionUnitTest"

# CPU Integration Tests

# Run all custom instruction tests
test-custom:
	@echo "═================================================================"
	@echo "                 Running Custom Instruction Tests"
	@echo "================================================================"
	sbt "testOnly riscv.SingleVexpTest riscv.TwoInstructionsTest riscv.CustomInstructionE2ETest"

# Individual integration tests
test-single-vexp:
	@echo "Testing Single VEXP Instruction..."
	sbt "testOnly riscv.SingleVexpTest"

test-two-instructions:
	@echo "Testing Two Sequential Instructions (VEXP + VRSQRT)..."
	sbt "testOnly riscv.TwoInstructionsTest"

test-e2e:
	@echo "Testing End-to-End (4 instructions)..."
	sbt "testOnly riscv.CustomInstructionE2ETest"

# Comprehensive Test Suites

# Run all SFU-related tests (unit + integration)
test-sfu-all: test-sfu test-custom
	@echo ""
	@echo "================================================================"
	@echo "                 All SFU Tests Completed"
	@echo "================================================================"

# Quick  test
test-quick:
	@echo " Running quick smoke tests..."
	sbt "testOnly sfu.ExponentialApproximatorTest sfu.InvSqrtTest riscv.SingleVexpTest"

# Assembly Build Commands


# Create build directory
$(BUILD_DIR):
	@echo " Creating build directory..."
	@mkdir -p $(BUILD_DIR)

# Build all assembly test programs
build-asm: $(CUSTOM_INST_ASMBIN) $(SINGLE_VEXP_ASMBIN) $(TWO_INST_ASMBIN)
	@echo ""
	@echo "================================================================"
	@echo "  All Assembly Test Programs Built Successfully!"
	@echo "================================================================"
	@echo ""
	@echo "Generated files:"
	@echo "  $(CUSTOM_INST_ASMBIN)"
	@echo "  $(SINGLE_VEXP_ASMBIN)"
	@echo "  $(TWO_INST_ASMBIN)"
	@echo ""

# Custom Instruction Test (4 instructions)

$(CUSTOM_INST_OBJ): $(CUSTOM_INST_ASM_SRC) | $(BUILD_DIR)
	@echo "[1/4] Assembling $(CUSTOM_INST_ASM_SRC)..."
	$(AS) $(ASFLAGS) -o $@ $<
	@echo "  Assembly successful"

$(CUSTOM_INST_ELF): $(CUSTOM_INST_OBJ) $(LINKER_SCRIPT)
	@echo "[2/4] Linking $(CUSTOM_INST_OBJ)..."
	$(LD) $(LDFLAGS) -o $@ $(CUSTOM_INST_OBJ)
	@echo "  Linking successful"

$(CUSTOM_INST_BIN): $(CUSTOM_INST_ELF)
	@echo "[3/4] Converting ELF to binary..."
	$(OBJCOPY) -O binary $< $@
	@echo "  Binary conversion successful (size: $$(stat -c%s $@) bytes)"

$(CUSTOM_INST_ASMBIN): $(CUSTOM_INST_BIN)
	@echo "[4/4] Installing to resources..."
	@mkdir -p $(MAIN_RES_DIR)
	@cp $< $@
	@echo "  Installed to $(CUSTOM_INST_ASMBIN)"

# Single VEXP Test

$(SINGLE_VEXP_OBJ): $(SINGLE_VEXP_ASM_SRC) | $(BUILD_DIR)
	@echo "Assembling $(SINGLE_VEXP_ASM_SRC)..."
	$(AS) $(ASFLAGS) -o $@ $<

$(SINGLE_VEXP_ELF): $(SINGLE_VEXP_OBJ) $(LINKER_SCRIPT)
	@echo "Linking $(SINGLE_VEXP_OBJ)..."
	$(LD) $(LDFLAGS) -o $@ $(SINGLE_VEXP_OBJ)

$(SINGLE_VEXP_BIN): $(SINGLE_VEXP_ELF)
	@echo "Converting to binary..."
	$(OBJCOPY) -O binary $< $@

$(SINGLE_VEXP_ASMBIN): $(SINGLE_VEXP_BIN)
	@echo "Installing $(SINGLE_VEXP_ASMBIN)..."
	@mkdir -p $(MAIN_RES_DIR)
	@cp $< $@

# Two Instructions Test

$(TWO_INST_OBJ): $(TWO_INST_ASM_SRC) | $(BUILD_DIR)
	@echo "Assembling $(TWO_INST_ASM_SRC)..."
	$(AS) $(ASFLAGS) -o $@ $<

$(TWO_INST_ELF): $(TWO_INST_OBJ) $(LINKER_SCRIPT)
	@echo "Linking $(TWO_INST_OBJ)..."
	$(LD) $(LDFLAGS) -o $@ $(TWO_INST_OBJ)

$(TWO_INST_BIN): $(TWO_INST_ELF)
	@echo "Converting to binary..."
	$(OBJCOPY) -O binary $< $@

$(TWO_INST_ASMBIN): $(TWO_INST_BIN)
	@echo "Installing $(TWO_INST_ASMBIN)..."
	@mkdir -p $(MAIN_RES_DIR)
	@cp $< $@

# Individual Build Steps

build-custom-inst: $(CUSTOM_INST_ASMBIN)
	@echo "Custom instruction test binary ready"

build-single-vexp: $(SINGLE_VEXP_ASMBIN)
	@echo "Single VEXP test binary ready"

build-two-inst: $(TWO_INST_ASMBIN)
	@echo "Two instructions test binary ready"

# Analysis Tools

# Disassemble ELF files
disasm: $(CUSTOM_INST_ELF)
	@echo "Disassembling $(CUSTOM_INST_ELF)..."
	@echo "================================================================"
	$(OBJDUMP) -d $< | tee $(CUSTOM_INST_DUMP)
	@echo "================================================================"
	@echo "Disassembly saved to $(CUSTOM_INST_DUMP)"

disasm-single: $(SINGLE_VEXP_ELF)
	@echo "Disassembling $(SINGLE_VEXP_ELF)..."
	$(OBJDUMP) -d $<

disasm-two: $(TWO_INST_ELF)
	@echo "Disassembling $(TWO_INST_ELF)..."
	$(OBJDUMP) -d $<

# Show binary size and sections
size: $(CUSTOM_INST_ELF)
	@echo "Binary Size Information:"
	@echo "================================================================"
	$(SIZE) $<
	@echo "================================================================"

# Dump hex
hexdump: $(CUSTOM_INST_BIN)
	@echo "Hex Dump of $(CUSTOM_INST_BIN):"
	@echo "================================================================"
	@hexdump -C $< | head -20
	@echo "================================================================"

# Show toolchain info
toolchain-info:
	@echo "Toolchain Information:"
	@echo "================================================================"
	@echo "Assembler:  $(AS)"
	@$(AS) --version | head -1 || echo "  (not found)"
	@echo ""
	@echo "Linker:     $(LD)"
	@$(LD) --version | head -1 || echo "  (not found)"
	@echo ""
	@echo "Objcopy:    $(OBJCOPY)"
	@$(OBJCOPY) --version | head -1 || echo "  (not found)"
	@echo "================================================================"

verilator:
	@if java -version >/dev/null 2>&1; then \
		PATH=$$HOME/.local/bin:$$PATH sbt "runMain board.verilator.VerilogGenerator"; \
	else \
		echo "⚠️  Java runtime not found; using existing generated Verilog in verilog/verilator"; \
		if [ ! -f verilog/verilator/Top.v ]; then \
			echo "❌ Top.v missing; install Java (set JAVA_HOME) to regenerate Verilog"; \
			exit 1; \
		fi; \
	fi
	cd verilog/verilator && verilator --exe --cc sim.cpp Top.v \
		-CFLAGS "$$(sdl2-config --cflags)" \
		-LDFLAGS "$$(sdl2-config --libs)" && \
		make -C obj_dir -f VTop.mk

demo: verilator
	@echo "🔄 Building nyancat demo binary..."
	@$(MAKE) -C csrc nyancat.asmbin >/dev/null
	@echo "🎮 Running nyancat demo with SDL2 display..."
	@echo "   - Press ESC or close window to exit"
	@echo "   - VGA output: 640×480 @ 72Hz"
	@echo ""
	cd verilog/verilator/obj_dir && ./VTop -i ../../../csrc/nyancat.asmbin
	@echo ""
	@echo "✅ Demo complete!"

indent:
	find . -name '*.scala' | xargs scalafmt
	clang-format -i verilog/verilator/*.cpp verilog/verilator/*.h
	clang-format -i csrc/*.[ch]

compliance: check-riscof
	@echo "Running RISCOF compliance tests for 4-soc (RV32I + Zicsr)..."
	@cd ../tests && RISCOF_WORK=riscof_work_4soc ./run-compliance.sh 4-soc
	@echo ""
	@echo "Copying results to results/ directory..."
	@$(RM) -r results
	@mkdir -p results
	@cp -r ../tests/riscof_work_4soc/* results/
	@echo "Cleaning up auto-generated RISCOF test files..."
	@$(RM) -f src/test/scala/riscv/compliance/ComplianceTest.scala
	@$(RM) -f src/main/resources/test.asmbin
	@echo "✅ Compliance tests complete. Results in results/"
	@echo "📊 View report: results/report.html"

clean:
	sbt clean
	$(RM) -r test_run_dir
	$(RM) -r verilog/verilator/obj_dir
	$(RM) verilog/verilator/*.v
	$(RM) verilog/verilator/*.fir
	$(RM) verilog/verilator/*.anno.json
	$(RM) -r $(BUILD_DIR)
	$(RM) -f $(MAIN_RES_DIR)/custom_inst_test.asmbin
	$(RM) -f $(MAIN_RES_DIR)/single_vexp_test.asmbin
	$(RM) -f $(MAIN_RES_DIR)/two_inst_test.asmbin

distclean: clean
	$(RM) -r results


# Help
help:
	@echo ""
	@echo "================================================================"
	@echo "                 MyCPU with SFU - Makefile Help"
	@echo "================================================================"
	@echo ""
	@echo "  Test Commands:"
	@echo "    make test                  - Run all tests"
	@echo "    make test-sfu              - Run all SFU unit tests"
	@echo "    make test-sfu-all          - Run all SFU tests (unit + integration)"
	@echo "    make test-custom           - Run all custom instruction tests"
	@echo "    make test-quick            - Quick smoke tests"
	@echo ""
	@echo "  SFU Unit Tests:"
	@echo "    make test-sfu-exp          - Test VEXP (ExponentialApproximator)"
	@echo "    make test-sfu-rsqrt        - Test VRSQRT (InvSqrt)"
	@echo "    make test-sfu-accumulator  - Test VREDSUM (VectorAccumulator)"
	@echo "    make test-sfu-divider      - Test FPDivider"
	@echo "    make test-sfu-rmsnorm      - Test RMSNorm Accelerator"
	@echo "    make test-sfu-softmax      - Test Softmax Accelerator"
	@echo "    make test-sfu-integration  - Test SpecialFunctionUnit"
	@echo ""
	@echo "  CPU Integration Tests:"
	@echo "    make test-single-vexp      - Single VEXP instruction test"
	@echo "    make test-two-instructions - Two instructions test (VEXP + VRSQRT)"
	@echo "    make test-e2e              - End-to-end test (4 instructions)"
	@echo ""
	@echo "  Assembly Build Commands:"
	@echo "    make build-asm             - Build all assembly test programs"
	@echo "    make build-custom-inst     - Build custom_inst_test.asmbin"
	@echo "    make build-single-vexp     - Build single_vexp_test.asmbin"
	@echo "    make build-two-inst        - Build two_inst_test.asmbin"
	@echo ""
	@echo "  Analysis Tools:"
	@echo "    make disasm                - Disassemble custom_inst_test.elf"
	@echo "    make disasm-single         - Disassemble single_vexp_test.elf"
	@echo "    make disasm-two            - Disassemble two_inst_test.elf"
	@echo "    make size                  - Show binary size information"
	@echo "    make hexdump               - Show hex dump of binary"
	@echo "    make toolchain-info        - Show RISC-V toolchain information"
	@echo ""
	@echo "  Build Commands:"
	@echo "    make verilator             - Build Verilator simulation"
	@echo "    make demo                  - Run nyancat demo"
	@echo ""
	@echo "  Other Commands:"
	@echo "    make indent                - Format Scala and C code"
	@echo "    make compliance            - Run RISC-V compliance tests"
	@echo "    make clean                 - Clean build artifacts"
	@echo "    make distclean             - Clean everything including results"
	@echo "    make help                  - Show this help"
	@echo ""
	@echo "  Examples:"
	@echo "    make build-asm             # Build all assembly test programs"
	@echo "    make test-sfu-all          # Test all SFU functionality"
	@echo "    make test-quick            # Fast validation"
	@echo "    make test-e2e              # Full end-to-end test"
	@echo "    make disasm                # View disassembly"
	@echo ""
	@echo "================================================================"
	@echo ""

.PHONY: verilator test indent demo compliance clean distclean help \
        test-sfu test-sfu-exp test-sfu-rsqrt test-sfu-accumulator \
        test-sfu-divider test-sfu-rmsnorm test-sfu-softmax test-sfu-integration test-custom \
        test-single-vexp test-two-instructions test-e2e \
        test-sfu-all test-quick \
        build-asm build-custom-inst build-single-vexp build-two-inst \
        disasm disasm-single disasm-two size hexdump toolchain-info
