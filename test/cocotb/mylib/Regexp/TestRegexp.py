
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

    async def cycle_reset(self):
        self.dut.rst.setimmediatevalue(0)
        await RisingEdge(self.dut.clk)
        await RisingEdge(self.dut.clk)
        self.dut.rst <= 1
        await RisingEdge(self.dut.clk)
        await RisingEdge(self.dut.clk)
        self.dut.rst <= 0
        await RisingEdge(self.dut.clk)
        await RisingEdge(self.dut.clk)


    def list2mem(self, list):
        val = 0
        if type(list) == str:
            val = len(list) # byte[1:0] is the length of string
            for ii in range(0, len(list)):
                val = val + (ord(list[ii]) << ((ii+2)*8))
        else:
            for ii in range(0, len(list)):
                val = val + (list[ii] << (ii*8))
            # set msb of config to 1 (config all)
            val = val + (1 << 511)
        return val

        # mem = mmap.mmap(-1, len(list))
        # for ii in range(0, len(list)):
        #     mem.seek(ii)
        #     if type(list) == str:
        #         mem.write(ord(list[ii]).to_bytes(1, 'little'))
        #     else:
        #         mem.write(list[ii].to_bytes(1, 'little'))
        # return mem

@cocotb.test()
async def run_test_read(dut, data_in=None, idle_inserter=None, backpressure_inserter=None):

    tb = TB(dut)

    config_list = (97, 97, 97, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 0) # "aaa"
    # config_list = (98, 99, 97, 101, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 2, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 1, 0, 0, 0, 0, 0, 2, 1, 0, 0, 0, 3)
    str_in = "abcdeeeaaabcdaaeaaad"
    # str_in = "cccccccc"

    mem_config = tb.list2mem(config_list)
    mem_str = tb.list2mem(str_in)


    dut.input_valid <= 0
    dut.config_valid <= 0
    dut.found_ready <= 1

    await tb.cycle_reset()


    dut.input_valid <= 0
    dut.config_valid <= 1
    dut.config_data <= mem_config
    await RisingEdge(dut.clk)

    dut.input_valid <= 1
    dut.input_last <= 1
    dut.config_valid <= 0
    dut.input_data <= mem_str
    await RisingEdge(dut.clk)

    dut.input_data <= tb.list2mem("abcdeeeaabcdaaeaad")
    await RisingEdge(dut.clk)

    for ii in range(0, 10):
        dut.input_data <= tb.list2mem("abcdeeeaaabcdaaeaad") # ok
        await RisingEdge(dut.clk)

        dut.input_data <= tb.list2mem("abcdeeeaabcdaaeaad") # nok
        await RisingEdge(dut.clk)

    dut.input_valid <= 0
    dut.config_valid <= 0

    await RisingEdge(dut.found_valid)

    await ClockCycles(dut.clk, 2000)
