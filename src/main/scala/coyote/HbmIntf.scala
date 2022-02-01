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
case class HbmIntf(hbmAxiConf: Axi4Config) extends Component with SetDefaultIO with RenameIO {

  val io = new Bundle with SetDefaultIO {
    // ctrl
    val mode = in UInt(2 bits)
    val hostAddr = in UInt(64 bits)
    val hbmAddr = in UInt(64 bits)
    val len = in UInt(16 bits)
    val cnt = in UInt(64 bits)
    val pid = in UInt(6 bits)
    val cntDone = out(Reg(UInt(64 bits))).init(0)

    // bpss h2c/c2h
    val bpss_rd_req = master Stream StreamData(96)
    val bpss_wr_req = master Stream StreamData(96)
    val bpss_rd_done = slave Stream StreamData(6)
    val bpss_wr_done = slave Stream StreamData(6)
    val axis_host_sink = slave Stream BpssData(512)
    val axis_host_src = master Stream BpssData(512)

    // hbm interface
    val axi_hbm = master(Axi4(hbmAxiConf))

    // tie-off
    def setDefault(): Unit ={
      for(stream <- List(axi_hbm.aw, axi_hbm.w, axi_hbm.ar, axi_hbm.r, axi_hbm.b))
        setDefStream(stream)
    }
  }

  io.setDefault()

  val cntWrData, cntRdData, cntReq, cntHbmReq = Reg(UInt(64 bits)).init(0)
  val hbmRdAddr, hbmWrAddr, reqAddr = Reg(UInt(64 bits)).init(0)

  val bpss_rd_req, bpss_wr_req = ReqT()
  for (e <- List(bpss_rd_req, bpss_wr_req)){
    e.vaddr := reqAddr.resized
    e.len := io.len.resized
    e.stream := False
    e.sync := False
    e.ctl := True
    e.host := True
    e.dest := 0
    e.pid := io.pid
    e.vfid := 0
    e.rsrvd := 0
  }

  io.bpss_rd_req.data.assignFromBits(bpss_rd_req.asBits)
  io.bpss_wr_req.data.assignFromBits(bpss_wr_req.asBits)
  io.bpss_rd_req.valid := False
  io.bpss_wr_req.valid := False

  io.bpss_rd_done.ready := True
  io.bpss_wr_done.ready := True

  io.axis_host_src.tdata := 0
  io.axis_host_src.tdest := 0
  io.axis_host_src.tlast := False
  io.axis_host_src.tkeep.setAll()
  io.axis_host_src.valid := False

  io.axis_host_sink.ready := False


  val fsm = new StateMachine {
    val IDLE = new State with EntryPoint
    val RD, WR = new State

    IDLE.whenIsActive {
      when(io.mode(0)) (goto(RD))
      when(io.mode(1)) (goto(WR))
      when(io.mode(0) || io.mode(1)) {
        reqAddr := io.hostAddr
        hbmRdAddr := io.hbmAddr
        hbmWrAddr := io.hbmAddr
        for (e <- List(cntRdData, cntWrData, cntReq, cntHbmReq, io.cntDone))
          e.clearAll()
      } // clean
    }

    RD.whenIsActive {

      // connect axis_sink to hbm
      io.axi_hbm.aw.setBurstINCR()
      io.axi_hbm.aw.len := ((io.len >> 6) - 1).resized
      io.axi_hbm.aw.size := log2Up(512/8)
      io.axi_hbm.aw.addr := hbmWrAddr.resized

      io.axi_hbm.b.ready := True

      when(io.axi_hbm.aw.fire){
        cntHbmReq := cntHbmReq + 1
        hbmWrAddr := hbmWrAddr + io.len
      }
      when(cntHbmReq =/= io.cnt)(io.axi_hbm.aw.valid := True)

      io.axi_hbm.w.strb.setAll()
      io.axi_hbm.w.data <> io.axis_host_sink.tdata
      io.axi_hbm.w.last <> io.axis_host_sink.tlast
      io.axi_hbm.w.valid <> io.axis_host_sink.valid
      io.axi_hbm.w.ready <> io.axis_host_sink.ready


      // rd_req
      when(io.bpss_rd_req.fire){
        cntReq := cntReq + 1
        reqAddr := reqAddr + io.len
      }
      when(cntReq =/= io.cnt)(io.bpss_rd_req.valid := True)

      // rd_resp
      when(io.bpss_rd_done.fire)(io.cntDone := io.cntDone + 1)
      when(io.axis_host_sink.fire) (cntRdData := cntRdData + 1)
      when(io.cntDone === io.cnt) (goto(IDLE))
    }


    WR.whenIsActive {

      // wr_req
      when(io.bpss_wr_req.fire){
        cntReq := cntReq + 1
        reqAddr := reqAddr + io.len
      }
      when(cntReq =/= io.cnt)(io.bpss_wr_req.valid := True)

      // connect axis_src to hbm
      io.axi_hbm.ar.setBurstINCR()
      io.axi_hbm.ar.len := ((io.len >> 6) - 1).resized
      io.axi_hbm.ar.size := log2Up(512/8)
      io.axi_hbm.ar.addr := hbmRdAddr.resized

      when(io.axi_hbm.ar.fire){
        cntHbmReq := cntHbmReq + 1
        hbmRdAddr := hbmRdAddr + io.len
      }
      when(cntHbmReq =/= io.cnt)(io.axi_hbm.ar.valid := True)

      io.axi_hbm.r.data <> io.axis_host_src.tdata
      io.axi_hbm.r.valid <> io.axis_host_src.valid
      io.axi_hbm.r.ready <> io.axis_host_src.ready

      // writeCnt
      when(io.axis_host_src.fire) {
        cntWrData := cntWrData + 1
      }

      // wr_resp
      when(io.bpss_wr_done.fire)(io.cntDone := io.cntDone + 1)
      when(io.cntDone === io.cnt) (goto(IDLE))
    }

  }
}