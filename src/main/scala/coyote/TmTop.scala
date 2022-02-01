package coyote

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4Config, AxiLite4SlaveFactory}
import spinal.lib.fsm.StateMachine
import util._

import scala.language.postfixOps

//
case class TmTop() extends Component with SetDefaultIO with RenameIO {

  // configs
  val hbmAxiConf = Axi4Config(
    addressWidth = 33,
    dataWidth = 512,
    idWidth = 6,
    useStrb = true,
    useBurst = true,
    useId = true,
    useLock = false,
    useRegion = false,
    useCache = false,
    useProt = false,
    useQos = false,
    useLen = true
  )

  val nTxnMan = 4
  val nLt = 16
  val nCh = 1
  val nPE = nCh * nTxnMan


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

    // hbm interface
    val axi_hbm = Vec(master(Axi4(hbmAxiConf)), 33)

    // rdma
    val rdma_1_rd_req = slave Stream StreamData(64)
    val rdma_1_wr_req = slave Stream StreamData(64)
    val rdma_1_rq = slave Stream StreamData(256)
    val rdma_1_sq = master Stream StreamData(256)
    val axis_rdma_1_sink = slave Stream Axi4StreamData(512)
    val axis_rdma_1_src = master Stream Axi4StreamData(512)

    // tie-off rdma and bpss_wr related
    def setDefault(): Unit = {
      for (stream <- List(rdma_1_rd_req, rdma_1_wr_req, rdma_1_rq, rdma_1_sq, axis_rdma_1_sink, axis_rdma_1_src))
        setDefStream(stream)

      for (ii <- (1+nCh*2) until 33) {
        for (stream <- List(axi_hbm(ii).aw, axi_hbm(ii).w, axi_hbm(ii).ar, axi_hbm(ii).r, axi_hbm(ii).b))
          setDefStream(stream)
      }

      // req_axi wr channels of tm
      for (ii <- (1+nCh) until (1+2*nCh)) {
        for (stream <- List(axi_hbm(ii).aw, axi_hbm(ii).w, axi_hbm(ii).b))
          setDefStream(stream)
      }

    }
  }

  io.setDefault()

  // instantiate hbm interface, TmMulti
  val hbmIntf = HbmIntf(hbmAxiConf)
  val tmMulti = TmMulti(nTxnMan, nLt, nCh, hbmAxiConf)
  io.bpss_rd_req <> hbmIntf.io.bpss_rd_req
  io.bpss_wr_req <> hbmIntf.io.bpss_wr_req
  io.bpss_rd_done <> hbmIntf.io.bpss_rd_done
  io.bpss_wr_done <> hbmIntf.io.bpss_wr_done
  io.axis_host_sink <> hbmIntf.io.axis_host_sink
  io.axis_host_src <> hbmIntf.io.axis_host_src

  io.axi_hbm(0) <> hbmIntf.io.axi_hbm

  for (i <- 0 until nCh) {
    io.axi_hbm(1+i) <> tmMulti.io.m_axi(i)
    // req_axi is read only
    io.axi_hbm(1+nCh+i).ar <> tmMulti.io.req_axi(i).ar
    io.axi_hbm(1+nCh+i).r <> tmMulti.io.req_axi(i).r
  }

  // ctrl reg
  val ctrlReg = new AxiLite4SlaveFactory(io.axi_ctrl)
  var ireg = 0
  // module ctrl offset
  val htmCtrlOff = 0
  val tmCtrlOff = 7

  val htmCtrlMode = ctrlReg.createReadAndWrite(hbmIntf.io.mode.clone(), (htmCtrlOff+ireg)<<6, 0)
  hbmIntf.io.mode <> htmCtrlMode
  // clear logic
  when(htmCtrlMode(0) || htmCtrlMode(1)){htmCtrlMode.clearAll()}
  ireg += 1

  for (e <- List(hbmIntf.io.hostAddr, hbmIntf.io.hbmAddr, hbmIntf.io.len, hbmIntf.io.cnt, hbmIntf.io.pid)) {
    val reg = ctrlReg.createReadAndWrite(e.clone(), (htmCtrlOff+ireg)<<6, 0)
    e <> reg
    ireg += 1
  }
  ctrlReg.read(hbmIntf.io.cntDone, (htmCtrlOff+ireg)<<6, 0)

  // tm module
  ireg = 0
  val tmCtrlStart = ctrlReg.createReadAndWrite(tmMulti.io.ap_start.clone(), (tmCtrlOff+ireg)<<6, 0)
  tmMulti.io.ap_start <> tmCtrlStart
  // clear logic
  when(tmCtrlStart){tmCtrlStart.clear()}
  ireg += 1

  for (e <- List(tmMulti.io.txnLen, tmMulti.io.txnCnt)) {
    val reg = ctrlReg.createReadAndWrite(e.clone(), (tmCtrlOff+ireg)<<6, 0)
    e <> reg
    ireg += 1
  }

  for (e <- List(tmMulti.io.ap_done ,tmMulti.io.clkCnt)) {
    ctrlReg.read(e, (tmCtrlOff + ireg) << 6, 0)
    ireg += 1
  }

  for (i <- 0 until nPE){
    val reg = ctrlReg.createReadAndWrite(tmMulti.io.addrOffset(i), (tmCtrlOff+ireg+i)<<6, 0)
    tmMulti.io.addrOffset(i) <> reg
    ctrlReg.read(tmMulti.io.txnExeCnt(i), (tmCtrlOff+ireg+nPE+i)<<6, 0)
    ctrlReg.read(tmMulti.io.txnAbortCnt(i), (tmCtrlOff+ireg+2*nPE+i)<<6, 0)
  }
}

object TmTopMain {
  def main(args: Array[String]): Unit = {
    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = LOW), targetDirectory = "rtl").generateVerilog{
      val top = TmTop()
      top.renameIO()
      top.setDefinitionName("design_user_wrapper_tmtop") // wrapper name in `dynamic_wrapper.sv`
      top
    }
  }
}
