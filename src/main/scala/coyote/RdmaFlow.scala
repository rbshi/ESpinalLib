package coyote

import spinal.core.Component.push
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4Config, AxiLite4SlaveFactory}
import spinal.lib.fsm.StateMachine
import util._

import scala.language.postfixOps

/*
* Bypass
* */
case class RdmaFlow(isMstr : Boolean) extends Component with SetDefaultIO with RenameIO {

  val io = new Bundle with SetDefaultIO {
    // interface RDMA
    val rdma_1_rd_req = slave Stream StreamData(96)
    val rdma_1_wr_req = slave Stream StreamData(96)
    // val rdma_1_rq = slave Stream StreamData(256)
    val rdma_1_sq = master Stream StreamData(256)
    val axis_rdma_1_sink = slave Stream Axi4StreamData(512)
    val axis_rdma_1_src =  master Stream Axi4StreamData(512)

    // interface user logic
    val q_sink = slave Stream(Bits(512 bits))
    val q_src = master Stream(Bits(512 bits))

    // input
    val en = in Bool()
    val len = in UInt(32 bits)
    val qpn = in UInt(24 bits)
    val nOnFly = in UInt(32 bits)

    // output
    val cntSent = out(Reg(UInt(32 bits))).init(0)
    val cntRecv = out(Reg(UInt(32 bits))).init(0)

    // dbg
    val dbg = out Bits(104 bits)
  }

  val rdma_base = RdmaBaseT()
  rdma_base.lvaddr := 0
  rdma_base.rvaddr := 0
  rdma_base.len := io.len
  rdma_base.params := 0

  val rdma_1_sq = RdmaReqT()
  rdma_1_sq.opcode := 1 // write
  rdma_1_sq.qpn := io.qpn
  rdma_1_sq.id := 0
  rdma_1_sq.host := False
  rdma_1_sq.mode := False
  rdma_1_sq.pkg.assignFromBits(rdma_base.asBits)
  rdma_1_sq.rsrvd := 0
  io.rdma_1_sq.data.assignFromBits(rdma_1_sq.asBits)

  // deft
  io.rdma_1_sq.valid.clear()
  io.axis_rdma_1_src.valid.clear()
  io.axis_rdma_1_src.tlast.clear()


  val cntAxiToSend = Reg(UInt(32 bits)).init(0)
  val cntOnFly = Reg(UInt(32 bits)).init(0)
  val cntWord = Reg(UInt(16 bits)).init(0)

  // ready of rd/wr req
  io.rdma_1_rd_req.ready.set()
  io.rdma_1_wr_req.ready.set()


  if(isMstr){
    // mstr hw & behavior

    val sendQ = StreamFifo(Bits(512 bits), 512)
    val recvQ = StreamFifo(Bits(512 bits), 512)

    when(~io.en){
      sendQ.io.flush.set()
      recvQ.io.flush.set()
    } otherwise {
      sendQ.io.flush.clear()
      recvQ.io.flush.clear()
    }

    // sendQ
    io.q_sink >> sendQ.io.push
    sendQ.io.pop.ready.clear()
    io.axis_rdma_1_src.tdata := sendQ.io.pop.payload

    // fire sq
    val fireSq = ((sendQ.io.occupancy - (cntAxiToSend<<4)).asSInt >= 16) && (recvQ.io.availability >= ((cntOnFly+1)<<4)) && (cntOnFly < io.nOnFly)
    when(fireSq)(io.rdma_1_sq.valid := True)

    // receive the rd_req
    // val incCntToSend = io.rdma_1_rd_req.fire
    val incCntToSend = io.rdma_1_sq.fire
    val decCntToSend = io.axis_rdma_1_src.fire && io.axis_rdma_1_src.tlast
    cntAxiToSend := cntAxiToSend - decCntToSend.asUInt(1 bit) + incCntToSend.asUInt(1 bit)
    io.cntSent := io.cntSent + decCntToSend.asUInt(1 bit)

    io.axis_rdma_1_src.tkeep.setAll()
    // have packet to send
    when(cntAxiToSend > 0) {
      // sendQ.pop >> rdma_src
      io.axis_rdma_1_src.valid <> sendQ.io.pop.valid
      io.axis_rdma_1_src.ready <> sendQ.io.pop.ready

      when(cntWord === ((io.len >> 6) - 1))(io.axis_rdma_1_src.tlast.set())
      when(io.axis_rdma_1_src.fire) {
        when(cntWord === ((io.len >> 6) - 1))(cntWord.clearAll()) otherwise (cntWord := cntWord + 1)
      }
    }

    // recvQ
    recvQ.io.pop >> io.q_src
    recvQ.io.push.payload := io.axis_rdma_1_sink.tdata
    recvQ.io.push.valid <> io.axis_rdma_1_sink.valid
    recvQ.io.push.ready <> io.axis_rdma_1_sink.ready

    // cntOnFly control
    val incOnFly = io.rdma_1_sq.fire
    val decOnFly = io.axis_rdma_1_sink.fire && io.axis_rdma_1_sink.tlast
    cntOnFly := cntOnFly - decOnFly.asUInt(1 bit) + incOnFly.asUInt(1 bit)
    io.cntRecv := io.cntRecv + decOnFly.asUInt(1 bit)


    // dbg
    io.dbg(0) := ((sendQ.io.occupancy - (cntAxiToSend<<4)) >= 16)
    io.dbg(1) := (recvQ.io.availability >= ((cntOnFly+1)<<4))
    io.dbg(2) := (cntOnFly < io.nOnFly)
    io.dbg(3) := incOnFly
    io.dbg(4) := decOnFly

    io.dbg(7 downto 5).clearAll()

    io.dbg(55 downto 8) := recvQ.io.availability.asBits.resized
    io.dbg(103 downto 56) := ((cntOnFly+1)<<4).asBits.resized

  } else {
    // slave hw & behavior (no onfly control)
    val reqsQ = StreamFifo(Bits(512 bits), 512)
    val respQ = StreamFifo(Bits(512 bits), 512)

    when(~io.en){
      reqsQ.io.flush.set()
      respQ.io.flush.set()
    } otherwise {
      reqsQ.io.flush.clear()
      respQ.io.flush.clear()
    }

    // reqsQ
    reqsQ.io.pop >> io.q_src
    reqsQ.io.push.payload := io.axis_rdma_1_sink.tdata
    reqsQ.io.push.valid <> io.axis_rdma_1_sink.valid
    reqsQ.io.push.ready <> io.axis_rdma_1_sink.ready

    val incRecv = io.axis_rdma_1_sink.fire && io.axis_rdma_1_sink.tlast
    io.cntRecv := io.cntRecv + incRecv.asUInt(1 bit)

    // respQ
    io.q_sink >> respQ.io.push
    respQ.io.pop.ready.clear()
    io.axis_rdma_1_src.tdata := respQ.io.pop.payload

    // fire sq
    val fireSq = (respQ.io.occupancy - (cntAxiToSend<<4)).asSInt >= 16 // cast to SInt for comparison
    when(fireSq)(io.rdma_1_sq.valid := True)

    // receive the rd_req
    // val incCntToSend = io.rdma_1_rd_req.fire // should NOT use rd_req to trigger the incCntToSend, it has a delay to the sq.fire, and will underflow to fireSq criteria to minus ??
    val incCntToSend = io.rdma_1_sq.fire
    val decCntToSend = io.axis_rdma_1_src.fire && io.axis_rdma_1_src.tlast
    cntAxiToSend := cntAxiToSend - decCntToSend.asUInt(1 bit) + incCntToSend.asUInt(1 bit)
    io.cntSent := io.cntSent + decCntToSend.asUInt(1 bit)

    io.axis_rdma_1_src.tkeep.setAll()
    // have packet to send
    when(cntAxiToSend > 0) {
      // sendQ.pop >> rdma_src
      io.axis_rdma_1_src.valid <> respQ.io.pop.valid
      io.axis_rdma_1_src.ready <> respQ.io.pop.ready

      when(cntWord === ((io.len >> 6) - 1))(io.axis_rdma_1_src.tlast.set())
      when(io.axis_rdma_1_src.fire) {
        when(cntWord === ((io.len >> 6) - 1))(cntWord.clearAll()) otherwise (cntWord := cntWord + 1)
      }
    }

    io.dbg.clearAll()
  }

  when(~io.en){
    for (e <- List(cntAxiToSend, cntOnFly, cntWord, io.cntSent, io.cntRecv))
      e.clearAll()
  }
}



object RdmaFlowMain {
  def main(args: Array[String]): Unit = {
    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = LOW), targetDirectory = "rtl").generateVerilog{
      val top = RdmaFlow(false)
      top.renameIO()
      top.setDefinitionName("design_user_wrapper_rdmaflowtmp") // wrapper name in `dynamic_wrapper.sv`
      top
    }
  }
}