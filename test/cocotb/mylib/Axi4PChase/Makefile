

RTL_ROOT = $(PWD)/../../../../rtl

TOPLEVEL_LANG = verilog

SIM ?= icarus

COCOTB_HDL_TIMEUNIT = 1ns
COCOTB_HDL_TIMEPRECISION = 1ns

COCOTB_ANSI_OUTPUT ?= 1
COCOTB_ENABLE_PROFILING ?= 1
COCOTB_LOG_LEVEL = 'DEBUG'


DUT      = WrapAxi4PChase
TOPLEVEL = $(DUT)
MODULE   = TestAxi4PChase

VERILOG_SOURCES += $(RTL_ROOT)/Axi4PChase.v $(RTL_ROOT)/WrapAxi4PChase.v

include $(shell cocotb-config --makefiles)/Makefile.sim

clean::
	@rm -rf iverilog_dump.v
	@rm -rf dump.fst $(TOPLEVEL).fst
