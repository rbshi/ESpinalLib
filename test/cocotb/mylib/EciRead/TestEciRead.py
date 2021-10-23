
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
        # init a mem (a link list in strided memory)
        self.mem_init = mmap.mmap(-1, mem_size)
        for i in range(0, 1024):
            self.mem_init.seek(i << 3)
            self.mem_init.write(i.to_bytes(8, 'little')) # key

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

    dut.io_req_valid <= 0
    await tb.cycle_reset()

    await RisingEdge(dut.clk)


    dut.io_dr_out_ready <= 1
    dut.io_md_out_ready <= 1

    await RisingEdge(dut.clk)

    dut.io_line_number <= 0
    dut.io_transaction_id <= 1
    dut.io_req_valid <= 1

    await RisingEdge(dut.clk)

    dut.io_line_number <= 2

    await RisingEdge(dut.clk)

    dut.io_line_number <= 6

    await RisingEdge(dut.clk)

    dut.io_req_valid <= 0


    await RisingEdge(dut.io_dr_out_valid)

    await ClockCycles(dut.clk, 200)
