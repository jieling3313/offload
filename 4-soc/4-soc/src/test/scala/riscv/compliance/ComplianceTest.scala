// Auto-generated compliance test
package riscv.compliance

import riscv.TestAnnotations

class ComplianceTest extends ComplianceTestBase {
  behavior.of("MyCPU Compliance")

  it should "pass test /Users/jserv/playground/ca2025-lab3/tests/riscv-arch-test/riscv-test-suite/rv32i_m/I/src/add-01.S" in {
    runComplianceTest(
      "test.asmbin",
      "/Users/jserv/playground/ca2025-lab3/tests/riscof_work_4soc/rv32i_m/I/src/add-01.S/dut/dut.elf",
      "/Users/jserv/playground/ca2025-lab3/tests/riscof_work_4soc/rv32i_m/I/src/add-01.S/dut/DUT-mycpu.signature",
      TestAnnotations.annos
    )
  }
}
