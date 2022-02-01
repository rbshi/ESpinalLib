package coyote

import spinal.core._
import scala.language.postfixOps

// both BpssReq & RDMACmd
//case class ReqT() extends Bundle {
//  val vaddr = UInt(48 bits)
//  val len = UInt(28 bits)
//  val stream, sync, ctl, host = Bool()
//  val dest = UInt(4 bits)
//  val pid = UInt(6 bits)
//  val vfid = UInt(1 bits) // only 1 vFPGA
//  val rsrvd = UInt(96-48-28-4-4-6-1 bits) // total len = 96b
//}

case class ReqT() extends Bundle {
  val rsrvd = UInt(96-48-28-4-4-6-1 bits) // total len = 96b
  val vfid = UInt(1 bits) // only 1 vFPGA
  val pid = UInt(6 bits)
  val dest = UInt(4 bits)
  val host, ctl, sync, stream = Bool()
  val len = UInt(28 bits)
  val vaddr = UInt(48 bits)
}


case class BpssDone() extends Bundle {
  val pid = UInt(6 bits)
}

// cast structure to data stream
case class StreamData(width: Int) extends Bundle {
  val data = Bits(width bits)
}

case class BpssData(width: Int) extends Bundle {
  val tdata = Bits(width bits)
  val tdest = UInt(4 bits)
  val tkeep = Bits(width/8 bits) // will be renamed in RenameIO
  val tlast = Bool()
}

case class Axi4StreamData(width: Int) extends Bundle {
  val tdata = Bits(width bits)
  val tkeep = Bits(width/8 bits) // will be renamed in RenameIO
  val tlast = Bool()
}

// cast RdmaReqT.pkg -> RdmaBaseT
//case class RdmaBaseT() extends Bundle {
//  val params = UInt(64 bits)
//  val len = UInt( 32 bits)
//  val rvaddr = UInt(48 bits)
//  val lvaddr = UInt(48 bits)
//}

// rdma parser bit definition assumes LSB first in sv struct
case class RdmaBaseT() extends Bundle {
  val lvaddr = UInt(48 bits)
  val rvaddr = UInt(48 bits)
  val len = UInt( 32 bits)
  val params = UInt(64 bits)
}

//case class RdmaPkgT() extends Bundle {
//  val data = Bits(192 bits)
//}

case class RdmaReqT() extends Bundle {
  val rsrvd = UInt(256-5-24-1-1-1-192 bits) // total len = 96b
  val pkg = UInt(192 bits) // RdmaBaseT or RPC
  val mode = Bool()
  val host = Bool()
  val id = UInt(1 bits)
  val qpn = UInt(24 bits)
  val opcode = UInt(5 bits)
}

