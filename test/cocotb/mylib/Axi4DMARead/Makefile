

RTL_ROOT = $(PWD)/../../../../rtl

TOPLEVEL_LANG = verilog

SIM ?= icarus

COCOTB_HDL_TIMEUNIT = 1ns
COCOTB_HDL_TIMEPRECISION = 1ns

COCOTB_ANSI_OUTPUT ?= 1
COCOTB_ENABLE_PROFILING ?= 1


DUT      = WrapAxi4DMARead
TOPLEVEL = $(DUT)
MODULE   = TestAxi4DMARead

VERILOG_SOURCES += $(RTL_ROOT)/Axi4DMARead.v $(RTL_ROOT)/WrapAxi4DMARead.v

include $(shell cocotb-config --makefiles)/Makefile.sim

clean::
	@rm -rf iverilog_dump.v
	@rm -rf dump.fst $(TOPLEVEL).fst

