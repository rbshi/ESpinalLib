package tmcs

import spinal.core.{UInt, _}
import spinal.lib._
import util._

class LtCh(sysConf: SysConfig) extends Component{
  val io = LockTableIO(sysConf)
  val ltAry = Array.fill(sysConf.nLtPart)(new LockTable(sysConf))

  // use the LSB bits for lkReq dispatching
  val lkReq2Lt = Stream(LkReq(sysConf, true))

  lkReq2Lt.arbitrationFrom(io.lkReq)
  for ((name, elem) <- lkReq2Lt.payload.elements){
    if (name != "tId") elem := io.lkReq.payload.find(name)
  }
  lkReq2Lt.tId := io.lkReq.payload.tId(lkReq2Lt.tId.high downto sysConf.wLtPart).resized

  // demux lock_req to multiple LTs
  val lkReq2LtDemux = StreamDemux(lkReq2Lt, io.lkReq.payload.tId(sysConf.wLtPart-1 downto 0), sysConf.nLtPart)
  (ltAry, lkReq2LtDemux).zipped.foreach(_.io.lkReq <-/< _) // pipelined and avoid the high fanout

  // arb the lkResp
  val lkRespArb = StreamArbiterFactory.roundRobin.build(LkResp(sysConf, true), sysConf.nLtPart)
  (lkRespArb.io.inputs, ltAry).zipped.foreach(_ <-/< _.io.lkResp)

  io.lkResp.arbitrationFrom(lkRespArb.io.output)
  for ((name, elem) <- io.lkResp.payload.elements){
    if (name != "tId") elem := lkRespArb.io.output.payload.find(name)
  }
  io.lkResp.tId := (lkRespArb.io.output.tId ## lkRespArb.io.chosen).asUInt.resized
}

class LtTop(sysConf: SysConfig) extends Component {
  // ltIO number: nTxnMan + (nNode -1)
  val io = new Bundle {
    val nodeId = in UInt(sysConf.wNId bits)
    val lt = Vec(LockTableIO(sysConf), sysConf.nTxnMan + sysConf.nNode -1)
  }

  // ltCh number: nCh
  val ltChAry = Array.fill(sysConf.nCh)(new LtCh(sysConf))
  // crossbar (lkReq/Resp bi-direction)
  StreamCrossbarFactory.on(io.lt.map(_.lkReq), ltChAry.map(_.io.lkReq),  io.lt.map(_.lkReq.cId))((o, i) => o.assignAllByName(i))
  StreamCrossbarFactory.on(ltChAry.map(_.io.lkResp), io.lt.map(_.lkResp), ltChAry.map(_.io.lkResp).map(fLkRespDemux(_)))((o, i) => o.assignAllByName(i))

  // demux the lkResp to nCh txnMan and (nNode - 1) txnAgent
  def fLkRespDemux(lkResp: LkResp): UInt = {
    val demuxSel = UInt(log2Up(sysConf.nCh + sysConf.nNode - 1) bits)
    when(io.nodeId < lkResp.nId) {
      demuxSel := sysConf.nCh + lkResp.nId - 1
    } otherwise {
      when(io.nodeId > lkResp.nId) {
        demuxSel := sysConf.nCh + lkResp.nId
      } otherwise {
        demuxSel := lkResp.cId
      }
    }
    demuxSel
  }

}

// with similar coding style as StreamArbiter
// bypass demuxSel for each StreamDemux
class StreamCrossbar[T1 <: Data, T2 <: Data](nIn: Int, nOut: Int, dInT: HardType[T1], dOutT: HardType[T2])(f: (T2, T1) => Unit) extends Component {
  val io = new Bundle {
    val inV = Vec(slave Stream(dInT), nIn)
    val outV = Vec(master Stream(dOutT), nOut)
    val inDemuxSel = in Vec(UInt(log2Up(nOut) bits))
  }

  // inV Demux
  val inDemuxAry = Array.fill(nIn)(new StreamDemux2(dInT, nOut))
  (io.inV, inDemuxAry).zipped.foreach(_ >> _.io.input)
  (io.inDemuxSel, inDemuxAry).zipped.foreach(_ >> _.io.select)

  // out Arbiter
  val outArbAry = Array.fill(nOut)(StreamArbiterFactory.roundRobin.build(dInT, nOut))
  for (i <- 0 until nOut)
    (inDemuxAry.map(_.io.outputs(i)) , outArbAry(i).io.inputs).zipped.foreach(_ >/-> _) // pipelined

  // apply f on arb out
  (io.outV, outArbAry.map(_.io.output)).zipped.foreach(_.translateFrom(_)(f))
}

object StreamCrossbarFactory {

  // instantiation
  def build[T1 <: Data, T2 <: Data](nIn: Int, nOut: Int, dInT: HardType[T1], dOutT: HardType[T2])(f: (T2, T1) => Unit): StreamCrossbar[T1, T2] = {
    new StreamCrossbar(nIn, nOut, dInT, dOutT)(f)
  }

  def on[T1 <: Data, T2 <: Data](inV: Seq[Stream[T1]], outV: Seq[Stream[T2]], inDemuxSel: Seq[UInt])(f: (T2, T1) => Unit) {
    val crossbar = build(inV.size, outV.size, inV.head.payloadType, outV.head.payloadType)(f)
    (crossbar.io.inV, inV).zipped.foreach(_ << _)
    (crossbar.io.outV, outV).zipped.foreach(_ >> _)
    (crossbar.io.inDemuxSel, inDemuxSel).zipped.foreach(_ <> _)
  }
}

// change dataType : T in StreamDemux -> dataType : HardType[T]
class StreamDemux2[T <: Data](dataType: HardType[T], portCount: Int) extends Component {
  val io = new Bundle {
    val select = in UInt (log2Up(portCount) bit)
    val input = slave Stream (dataType)
    val outputs = Vec(master Stream (dataType),portCount)
  }
  io.input.ready := False
  for (i <- 0 until portCount) {
    io.outputs(i).payload := io.input.payload
    when(i =/= io.select) {
      io.outputs(i).valid := False
    } otherwise {
      io.outputs(i).valid := io.input.valid
      io.input.ready := io.outputs(i).ready
    }
  }
}



















