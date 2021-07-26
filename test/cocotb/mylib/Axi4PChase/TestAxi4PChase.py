
import itertools
import logging
import os

import mmap

import cocotb_test.simulator
import pytest

import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, ClockCycles
from cocotb.regression import TestFactory

from cocotbext.axi import AxiWriteBus, AxiReadBus, AxiRamWrite, AxiRamRead
from cocotbext.axi import AxiStreamBus, AxiStreamFrame, AxiStreamSource

from cocotbext.axi.stream import define_stream


class TB(object):
    def __init__(self, dut):
        self.dut = dut

        self.log = logging.getLogger("cocotb.tb")
        self.log.setLevel(logging.DEBUG)

        cocotb.fork(Clock(dut.clk, 10, units="ns").start())

        mem_size = 2**16
        # init a mem
        self.mem_init = mmap.mmap(-1, mem_size)
        for i in range(0, 100):
            self.mem_init.seek(i << 6) # set the lowest byte of each 512-bit lane
            self.mem_init.write(((i+1)*64).to_bytes(4, 'little')) # sequential

        # AXI interface
        self.axi_ram = AxiRamRead(AxiReadBus.from_prefix(dut, "io_axi"), dut.clk, dut.reset, size=mem_size, mem=self.mem_init)

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
async def run_test_read(dut, data_in=None, idle_inserter=None, backpressure_inserter=None):

    tb = TB(dut)

    await tb.cycle_reset()

    dut.io_start_addr.setimmediatevalue(0)
    dut.io_num_burst.setimmediatevalue(10)

    dut.io_ap_start.setimmediatevalue(1)
    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)
    dut.io_ap_start.setimmediatevalue(0)


    await RisingEdge(dut.io_ap_done)

    # for ii in range(0, 64):
    #     tb.log.info("address %d, value %d", ii, int.from_bytes(tb.axi_ram.read(ii, 1), byteorder='little'))

    await ClockCycles(dut.clk, 200)

    # tb.log.info("%s", tb.axi_ram.hexdump_str((0 & ~0xf)-16, (((0 & 0xf)+5120-1) & ~0xf)+48))
