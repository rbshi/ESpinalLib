

RTL_ROOT = $(PWD)/../../../../rtl

TOPLEVEL_LANG = verilog

SIM ?= icarus

COCOTB_HDL_TIMEUNIT = 1ns
COCOTB_HDL_TIMEPRECISION = 1ns

COCOTB_ANSI_OUTPUT ?= 1
COCOTB_ENABLE_PROFILING ?= 1
COCOTB_LOG_LEVEL = 'DEBUG'


DUT      = WrapGraph
TOPLEVEL = $(DUT)
MODULE   = TestGraphSSSP

VERILOG_SOURCES += $(RTL_ROOT)/GraphSSSP.v $(RTL_ROOT)/WrapGraph.v

include $(shell cocotb-config --makefiles)/Makefile.sim

clean::
	@rm -rf iverilog_dump.v
	@rm -rf dump.fst $(TOPLEVEL).fst
