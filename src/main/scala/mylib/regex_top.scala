package mylib

import spinal.core.{UInt, _}
import spinal.lib._

class regex_top(regex_count_bits: Int, max_regex_engines: Int) extends BlackBox {

  addGeneric("REGEX_COUNT_BITS", regex_count_bits)
  addGeneric("MAX_REGEX_ENGINES", max_regex_engines)

  val io = new Bundle{
    val clk = in Bool()
    val rst = in Bool()
    val input_data = in Bits(512 bits)
    val input_valid = in Bool()
    val input_last = in Bool()
    val input_ready = out Bool()

    val config_data = in Bits(512 bits)
    val config_valid = in Bool()
    val config_ready = out Bool()

    val found_loc = out Bool()
    val found_valid = out Bool()
    val found_ready = in Bool()
  }

  noIoPrefix()
  // Map the current clock domain to the io.clk pin
  mapClockDomain(clock=io.clk, reset = io.rst)
}
