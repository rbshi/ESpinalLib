
import itertools
import logging
import os

import cocotb_test.simulator
import pytest

import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, ClockCycles
from cocotb.regression import TestFactory

from cocotbext.axi import AxiWriteBus, AxiRamWrite
from cocotbext.axi import AxiStreamBus, AxiStreamFrame, AxiStreamSource

from cocotbext.axi.stream import define_stream


class TB(object):
    def __init__(self, dut):
        self.dut = dut

        self.log = logging.getLogger("cocotb.tb")
        self.log.setLevel(logging.DEBUG)

        cocotb.fork(Clock(dut.clk, 10, units="ns").start())

        # AXI interface
        self.axi_ram = AxiRamWrite(AxiWriteBus.from_prefix(dut, "io_axi"), dut.clk, dut.reset, size=1024)





    async def cycle_reset(self):
        self.dut.reset.setimmediatevalue(0)
        await RisingEdge(self.dut.clk)
        await RisingEdge(self.dut.clk)
        self.dut.reset <= 1
        await RisingEdge(self.dut.clk)
        await RisingEdge(self.dut.clk)
        self.dut.reset <= 0
        await RisingEdge(self.dut.clk)
        await RisingEdge(self.dut.clk)


@cocotb.test()
async def run_test_write(dut, data_in=None, idle_inserter=None, backpressure_inserter=None):

    tb = TB(dut)

    await tb.cycle_reset()

    dut.io_start_addr.setimmediatevalue(0)
    dut.io_ap_start.setimmediatevalue(1)
    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)
    dut.io_ap_start.setimmediatevalue(0)


    await RisingEdge(dut.io_ap_done)

    # for ii in range(0, 64):
    #     tb.log.info("address %d, value %d", ii, int.from_bytes(tb.axi_ram.read(ii, 1), byteorder='little'))

    await ClockCycles(dut.clk, 200)

    tb.log.info("%s", tb.axi_ram.hexdump_str((0 & ~0xf)-16, (((0 & 0xf)+5120-1) & ~0xf)+48))
