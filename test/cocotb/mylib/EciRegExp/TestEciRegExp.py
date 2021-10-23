
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

        config_list = (97, 97, 97, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 0) # "aaa"
        str_match = "aaacddaaabccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbcaaa"
        str_not_match = "bbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd"


        list_len = 32
        endAddr = list_len * 128

        # init config
        for ii in range(0, len(config_list)):
            self.mem_init.seek(ii)
            self.mem_init.write(config_list[ii].to_bytes(1, 'little'))
        self.mem_init.seek(48) # byte[51:48] is endAddr
        self.mem_init.write(endAddr.to_bytes(8, 'little'))
        self.mem_init.seek(63)
        self.mem_init.write(int(128).to_bytes(4, 'little')) # set msb of config to 1 (config all)

        # write the list to be matched
        for ii in range(0, list_len):
            if ii%2==0:
                str_in = str_match
            else:
                str_in = str_match

            self.mem_init.seek((ii+1) * 128)
            self.mem_init.write(len(str_in).to_bytes(1, 'little'))
            for jj in range(0, len(str_in)):
                self.mem_init.seek((ii+1) * 128 + jj + 2)
                self.mem_init.write(ord(str_in[jj]).to_bytes(1, 'little'))

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

    for i in range(0, 32):

        dut.io_line_number <= i
        dut.io_transaction_id <= 1

        dut.io_req_valid <= 1
        await RisingEdge(dut.clk)
        dut.io_req_valid <= 0

        # for i in range(0, 30):
        #     await RisingEdge(dut.clk)

    await RisingEdge(dut.io_md_out_valid)

    #
    # for i in range(0, 300):
    #     await RisingEdge(dut.clk)
    #
    #
    # for i in range(0, 300):
    #
    #     dut.io_line_number <= i
    #     dut.io_transaction_id <= 1
    #
    #     dut.io_req_valid <= 1
    #     await RisingEdge(dut.clk)
    #     dut.io_req_valid <= 0
    #
    #     for i in range(0, 30):
    #         await RisingEdge(dut.clk)

    await ClockCycles(dut.clk, 200)
