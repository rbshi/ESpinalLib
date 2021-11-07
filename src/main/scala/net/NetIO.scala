package net

import spinal.core._
import spinal.core.Mem
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm.StateMachine
import scala.language.postfixOps
import util._

case class notif() extends Bundle{
  val sid = UInt(16 bits)
  val length = UInt(16 bits)
  val ipaddr = UInt(32 bits)
  val dstport = UInt(16 bits)
  val closed = Bool()
}

case class ipTuple() extends Bundle{
  val ipaddr = UInt(32 bits)
  val port = UInt(16 bits)
}

case class openStatus() extends Bundle{
  val sid = UInt(16 bits)
  val success = Bool()
}

case class readPkg() extends Bundle{
  val sid = UInt(16 bits)
  val length = UInt(16 bits)
}

case class netData(width: Int) extends Bundle{
  val data = Bits(width bits)
  val kep = Bits(width/8 bits)
  val last = Bool()
}

case class txMeta() extends Bundle{
  val sid = UInt(16 bits)
  val length = UInt(16 bits)
}

case class txStatus() extends Bundle{
  val sid = UInt(16 bits)
  val length = UInt(16 bits)
  val remaining_space = UInt(30 bits)
  val error = UInt(2 bits)
}


case class NetIO() extends Bundle with SetDefaultIO {
  val listen_port = master Stream UInt(16 bits)
  val port_status = slave Stream Bool()
  val notification = slave Stream notif().asBits
  val read_pkg = master Stream readPkg().asBits
  val rx_meta = slave Stream UInt(16 bits).asBits // length
  val rx_data = slave Stream netData(512).asBits

  val open_connection = master Stream ipTuple().asBits
  val open_status = slave Stream openStatus().asBits
  val close_connection = master Stream UInt(16 bits)

  val tx_meta = master Stream txMeta().asBits // tx data request
  val tx_data = master Stream netData(512).asBits
  val tx_status = slave Stream txStatus().asBits // tx data resp

  def setDefault(): Unit ={
    for (stream <- List(listen_port, port_status, rx_meta, open_connection, open_status, close_connection, tx_status))
      setDefStream(stream)
  }

}





















