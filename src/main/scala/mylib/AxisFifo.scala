/*
 *
 */

package mylib

import spinal.core.Component.push
import spinal.core.{UInt, _}
import spinal.lib._
import spinal.lib.bus.amba4.axi._

import scala.language.postfixOps

object AxisFifo {}

/**
 * Issue Axi4 write transactions in strode memory addresses with burst
 */
case class AxisFifo(width: Int, depth: Int) extends Component {

  val io = new Bundle {

    val s_axis = slave Stream(Bits(width bits))
    val m_axis = master Stream(Bits(width bits))
    val s_axis_almostfull = out Bool()
  }

  val fifo = StreamFifo(Bits(width bits), depth)

  io.s_axis >> fifo.io.push
  fifo.io.pop >> io.m_axis
  io.s_axis_almostfull := (fifo.io.occupancy === depth-1)

}

object AxisFifoMain {
  def main(args: Array[String]) {
    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC), targetDirectory = "rtl").generateVerilog(new AxisFifo(1, 512))
  }
}