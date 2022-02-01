package coyote

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
case class RdmaFlowTop() extends Component with SetDefaultIO with RenameIO {

  val io = new Bundle with SetDefaultIO {
    // ctrl
    val axi_ctrl = slave(AxiLite4(AxiLite4Config(64, 64)))

    // bpss h2c/c2h
    val bpss_rd_req = master Stream StreamData(96)
    val bpss_wr_req = master Stream StreamData(96)
    val bpss_rd_done = slave Stream StreamData(6)
    val bpss_wr_done = slave Stream StreamData(6)
    val axis_host_sink = slave Stream BpssData(512)
    val axis_host_src = master Stream BpssData(512)

    // rdma
    val rdma_1_rd_req = slave Stream StreamData(96)
    val rdma_1_wr_req = slave Stream StreamData(96)
    val rdma_1_rq = slave Stream StreamData(256)
    val rdma_1_sq = master Stream StreamData(256)
    val axis_rdma_1_sink = slave Stream Axi4StreamData(512)
    val axis_rdma_1_src =  master Stream Axi4StreamData(512)

    val dbg = out Bits(104 bits)

    // tie-off
    def setDefault(): Unit ={
      for(stream <- List(bpss_rd_req, bpss_wr_req, bpss_rd_done, bpss_wr_done, axis_host_sink, axis_host_src, rdma_1_rd_req, rdma_1_wr_req, rdma_1_sq, rdma_1_rq, axis_rdma_1_sink, axis_rdma_1_src))
        setDefStream(stream)
    }
  }

  io.setDefault()

  val q_sink = Stream(Bits(512 bits))
  val q_src = Stream(Bits(512 bits))
  q_sink.valid.clear()
  q_src.ready.clear()

  val rfMstr = RdmaFlow(true)
  val rfSlve = RdmaFlow(false)


  // axilite control registers
  val ctlReg = new AxiLite4SlaveFactory(io.axi_ctrl)
  val rfCtlOffs = 0

  val mode = ctlReg.createReadAndWrite(UInt(3 bits), rfCtlOffs+0, 0).init(0) // bit0 (IDLE:0 WORK:1) | bit1 (Slve:0, Mstr:1 ) | bit2 (1: Done)
  val lenpw = ctlReg.createReadAndWrite(UInt(4 bits), (rfCtlOffs+1)<<6, 0).init(0) // len = 2^lenpw
  val cnt = ctlReg.createReadAndWrite(UInt(32 bits), (rfCtlOffs+2)<<6, 0).init(0)
  val itvlPush = ctlReg.createReadAndWrite(UInt(16 bits), (rfCtlOffs+3)<<6, 0).init(0) // interval of rf
  val itvlPop = ctlReg.createReadAndWrite(UInt(16 bits), (rfCtlOffs+4)<<6, 0).init(0) // interval of rf
  val nOnFly = ctlReg.createReadAndWrite(UInt(32 bits), (rfCtlOffs+5)<<6, 0).init(32)
  val clkTimeOut = ctlReg.createReadAndWrite(UInt(64 bits), (rfCtlOffs+6)<<6, 0).init(0)

  val clk = ctlReg.createReadAndWrite(UInt(64 bits), (rfCtlOffs+7)<<6, 0).init(0)

  ctlReg.read(rfMstr.io.cntSent, (rfCtlOffs+8)<<6, 0)
  ctlReg.read(rfMstr.io.cntRecv, (rfCtlOffs+9)<<6, 0)
  ctlReg.read(rfSlve.io.cntSent, (rfCtlOffs+10)<<6, 0)
  ctlReg.read(rfSlve.io.cntRecv, (rfCtlOffs+11)<<6, 0)

  val isMstr = mode(1)
  val isEn = mode(0)

  io.dbg <> rfMstr.io.dbg

  def setDefRF(rf: RdmaFlow): Unit = {
    setDefStream(rf.io.rdma_1_rd_req, false)
    setDefStream(rf.io.rdma_1_wr_req, false)
    setDefStream(rf.io.rdma_1_sq, true)
    setDefStream(rf.io.axis_rdma_1_sink, false)
    setDefStream(rf.io.axis_rdma_1_src, true)
    setDefStream(rf.io.q_sink, false)
    setDefStream(rf.io.q_src, true)
  }

  setDefRF(rfMstr)
  setDefRF(rfSlve)

  when(isMstr){
    // connect io >> rfMstr
    io.rdma_1_rd_req <> rfMstr.io.rdma_1_rd_req
    io.rdma_1_wr_req <> rfMstr.io.rdma_1_wr_req
    io.rdma_1_sq <> rfMstr.io.rdma_1_sq
    io.axis_rdma_1_sink <> rfMstr.io.axis_rdma_1_sink
    io.axis_rdma_1_src <> rfMstr.io.axis_rdma_1_src
    q_sink >> rfMstr.io.q_sink
    q_src << rfMstr.io.q_src
  } otherwise{
    io.rdma_1_rd_req <> rfSlve.io.rdma_1_rd_req
    io.rdma_1_wr_req <> rfSlve.io.rdma_1_wr_req
    io.rdma_1_sq <> rfSlve.io.rdma_1_sq
    io.axis_rdma_1_sink <> rfSlve.io.axis_rdma_1_sink
    io.axis_rdma_1_src <> rfSlve.io.axis_rdma_1_src
    q_sink >> rfSlve.io.q_sink
    q_src << rfSlve.io.q_src
  }

  rfMstr.io.en := isMstr && isEn
  val one = UInt()
  one := 1
  rfMstr.io.len := (one<<lenpw).resized
  rfMstr.io.qpn := 0
  rfMstr.io.nOnFly := nOnFly

  rfSlve.io.en := ~isMstr && isEn
  rfSlve.io.len := (one<<lenpw).resized
  rfSlve.io.qpn := 0
  rfSlve.io.nOnFly := nOnFly


  val itvlPopCnt, itvlPushCnt = Reg(UInt(16 bits)).init(0)
  val cntPop, cntPush = Reg(UInt(32 bits)).init(0)
  val cntWord = UInt(32 bits)
  cntWord := (cnt << (lenpw-6)).resized

  when(isMstr){
    q_sink.payload := cntPush.asBits.resized
    when(itvlPushCnt >= itvlPush && cntPush < cntWord)(q_sink.valid.set())
    when(q_sink.fire)(itvlPushCnt.clearAll()) otherwise (itvlPushCnt := itvlPushCnt + 1)
    when(q_sink.fire)(cntPush := cntPush + 1)

    when(itvlPopCnt >= itvlPop && cntPop < cntWord)(q_src.ready.set())
    when(q_src.fire)(itvlPopCnt.clearAll()) otherwise (itvlPopCnt := itvlPopCnt + 1)
    when(q_src.fire)(cntPop := cntPop + 1)
  } otherwise{
    q_src.continueWhen(itvlPopCnt >= itvlPop && cntPop < cntWord) >> q_sink
    when(q_src.fire)(itvlPopCnt.clearAll()) otherwise (itvlPopCnt := itvlPopCnt + 1)
    when(q_src.fire)(cntPop := cntPop + 1)
  }

  when(isEn && ~mode(2))(clk := clk + 1)
  when(~isEn){
    for (e <- List(clk, cntPush, cntPop))
      e.clearAll()
  }

  val cntSent, cntRecv = UInt(32 bits)
  when(isMstr){
    cntSent := rfMstr.io.cntSent
    cntRecv := rfMstr.io.cntRecv
  } otherwise{
    cntSent := rfSlve.io.cntSent
    cntRecv := rfSlve.io.cntRecv
  }

  when((cntSent===cnt && cntRecv===cnt) || clk===clkTimeOut){
    mode(2) := True
  }

}


object RdmaFlowTopMain {
  def main(args: Array[String]): Unit = {
    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = LOW), targetDirectory = "rtl").generateVerilog{
      val top = RdmaFlowTop()
      top.renameIO()
      top.setDefinitionName("design_user_wrapper_rdmaflow") // wrapper name in `dynamic_wrapper.sv`
      top
    }
  }
}

