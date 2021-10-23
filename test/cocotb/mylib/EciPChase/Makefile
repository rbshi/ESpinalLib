

RTL_ROOT = $(PWD)/../../../../rtl

TOPLEVEL_LANG = verilog

SIM ?= icarus

COCOTB_HDL_TIMEUNIT = 1ns
COCOTB_HDL_TIMEPRECISION = 1ns

COCOTB_ANSI_OUTPUT ?= 1
COCOTB_ENABLE_PROFILING ?= 1
COCOTB_LOG_LEVEL = 'DEBUG'


DUT      = WrapEciPChase
TOPLEVEL = $(DUT)
MODULE   = TestEciPChase

VERILOG_SOURCES += $(RTL_ROOT)/EciPChaseMT.v $(RTL_ROOT)/$(DUT).v

include $(shell cocotb-config --makefiles)/Makefile.sim

clean::
	@rm -rf iverilog_dump.v
	@rm -rf dump.fst $(TOPLEVEL).fst
