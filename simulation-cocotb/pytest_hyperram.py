from cocotb_test.simulator import run
import pytest
import os
import sys

verilog_sources_dir = os.path.join(os.path.dirname(os.path.dirname(__file__)), 'src/hyperram')
mock_sources_dir = os.path.join(os.path.dirname(os.path.dirname(__file__)), 'HyperRAM_Simulation_Model')
test_sources_dir = os.path.join(os.path.dirname(os.path.dirname(__file__)), 'simulation-cocotb/test')

python_include_dir = os.path.join(os.path.dirname(os.path.dirname(__file__)), 'simulation-cocotb')

def test_hyperram_verilog():
    os.environ["SIM"] = "verilator"
    run(
        verilog_sources=[
            os.path.join(test_sources_dir, "clk.sv"),
            os.path.join(test_sources_dir, "delay.sv"),
            os.path.join(test_sources_dir, "hyperram_tb_top.sv"),

            os.path.join(verilog_sources_dir, "hyperram_config.sv"),
            os.path.join(verilog_sources_dir, "hyperram_ctrl.sv"),
            os.path.join(verilog_sources_dir, "hyperram_errata.sv"),
            os.path.join(verilog_sources_dir, "hyperram_io.sv"),
            os.path.join(verilog_sources_dir, "hyperram.sv"),

            os.path.join(mock_sources_dir, "hbram_ctrl.v"),
            os.path.join(mock_sources_dir, "HyperBusTestBench_simple.v"),
            os.path.join(mock_sources_dir, "s27kl0642.v"),
            os.path.join(mock_sources_dir, "tasks_simple.v")
            ], # list of sources
        python_search=[python_include_dir],
        #compile_args = ["--timing", "-Wno-lint", "--bbox-unsup", "-f", "../verilator.vlt"],
        compile_args = ["-Wno-lint", "--bbox-unsup", "-Wno-MULTIDRIVEN", "-Wno-UNOPTFLAT", "-Wno-COMBDLY", "-Wno-BLKANDNBLK", "--threads", "2", "--trace-threads", "2","+1800-2017ext+v","+1800-2017ext+sv"],
        toplevel="hyperram_tb_top",   # top level HDL
        module="hyperram_cocotb", # name of cocotb test module
        waves=True)

if __name__ == "__main__":
    def test_hyperram_verilog():
