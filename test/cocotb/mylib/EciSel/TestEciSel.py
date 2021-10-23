
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

        mem_size = 2**20
        # init a mem (a link list in strided memory)
        self.mem_init = mmap.mmap(-1, mem_size)

        # parameters config line: xxxx 1280 (end address) 2, 3
        aMax = 1
        bMax = 2
        endAddr = 12800
        self.mem_init.seek(0)
        self.mem_init.write(aMax.to_bytes(4, 'little'))
        self.mem_init.seek(4)
        self.mem_init.write(bMax.to_bytes(4, 'little'))
        self.mem_init.seek(8)
        self.mem_init.write(endAddr.to_bytes(8, 'little'))


        for i in range(0, 1000):
            self.mem_init.seek((i+1) << 7)
            aVal = i % 3
            bVal = i % 4
            self.mem_init.write(aVal.to_bytes(4, 'little'))
            self.mem_init.seek(((i+1) << 7) + 4)
            self.mem_init.write(bVal.to_bytes(4, 'little'))

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

    for i in range(0, 500):

        dut.io_line_number <= i
        dut.io_transaction_id <= 1

        dut.io_req_valid <= 1
        await RisingEdge(dut.clk)
        dut.io_req_valid <= 0

        for i in range(0, 30):
            await RisingEdge(dut.clk)


    for i in range(0, 300):
        await RisingEdge(dut.clk)


    for i in range(0, 300):

        dut.io_line_number <= i
        dut.io_transaction_id <= 1

        dut.io_req_valid <= 1
        await RisingEdge(dut.clk)
        dut.io_req_valid <= 0

        for i in range(0, 30):
            await RisingEdge(dut.clk)

    await ClockCycles(dut.clk, 200)
