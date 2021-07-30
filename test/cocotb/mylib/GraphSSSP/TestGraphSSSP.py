
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

from cocotbext.axi import AxiWriteBus, AxiReadBus, AxiRamWrite, AxiRamRead, AxiRam
from cocotbext.axi import AxiStreamBus, AxiStreamFrame, AxiStreamSource

from cocotbext.axi.stream import define_stream

import math

class AxiBus():
    def __init__(self, read, write):
        self.read = read
        self.write = write


class TB(object):
    def __init__(self, dut, mem_init, mem_size):
        self.dut = dut

        self.log = logging.getLogger("cocotb.tb")
        self.log.setLevel(logging.DEBUG)

        cocotb.fork(Clock(dut.clk, 10, units="ns").start())

        # AXI interface

        axi_bus = AxiBus(AxiReadBus.from_prefix(dut, "io_axi"), AxiWriteBus.from_prefix(dut, "io_axi"))
        self.axi_ram = AxiRam(axi_bus, dut.clk, dut.reset, size=mem_size, mem=mem_init)

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


    num_node = 6301
    start_node = 7
    addr_ptr = 0
    addr_data = math.ceil(float(num_node)/(512/64)) * 64
    addr_queue = addr_data + math.ceil(float(num_node)/(512/64)) * 64


    mem_size = os.path.getsize("graphmem.bin")
    with open("graphmem.bin", "r+b") as f:
        mem_init = mmap.mmap(f.fileno(), 0, access=mmap.ACCESS_COPY) # with default mmap.ACCESS_WRITE, the underlying file is mutable

    mem_init.seek(addr_data+8*start_node)
    mem_init.write((0).to_bytes(8, 'little'))

    mem_init.seek(addr_queue)
    mem_init.write((start_node).to_bytes(4, 'little'))

    tb = TB(dut, mem_init, mem_size)


    for ii in range(0, num_node):
        mem_init.seek(addr_data + ii * 8)
        dist = int.from_bytes(mem_init.read(8), byteorder='little')
        if dist < 0xffffffffffffffff:
            tb.log.info("distance to node [%d]= %d", ii, dist)


    await tb.cycle_reset()

    dut.io_addr_ptr.setimmediatevalue(addr_ptr)
    dut.io_addr_data.setimmediatevalue(addr_data)
    dut.io_addr_queue.setimmediatevalue(addr_queue)
    dut.io_start_node.setimmediatevalue(0)

    dut.io_ap_start.setimmediatevalue(1)
    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)
    dut.io_ap_start.setimmediatevalue(0)

    await RisingEdge(dut.io_ap_done)

    for ii in range(0, num_node):
        mem_init.seek(addr_data + ii * 8)
        dist = int.from_bytes(mem_init.read(8), byteorder='little')
        if dist < 0xffffffffffffffff:
            tb.log.info("distance to node [%d]= %d", ii, dist)



    await ClockCycles(dut.clk, 200)

    # tb.log.info("%s", tb.axi_ram.hexdump_str((0 & ~0xf)-16, (((0 & 0xf)+5120-1) & ~0xf)+48))
