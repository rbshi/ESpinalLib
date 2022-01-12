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
case class HbmRW() extends Component with SetDefaultIO with RenameIO {

  val hbmAxiConf = Axi4Config(
    addressWidth = 33,
    dataWidth    = 512,
    idWidth = 6,
    useStrb = true,
    useBurst = true,
    useId = true,
    useLock      = false,
    useRegion    = false,
    useCache     = false,
    useProt      = false,
    useQos       = false,
    useLen       = true
  )

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
    val axis_rdma_1_src =  master Stream Axi4StreamData(512)

    // tie-off rdma and bpss_wr related
    def setDefault(): Unit ={
      for(stream <- List(rdma_1_rd_req, rdma_1_wr_req, rdma_1_rq, rdma_1_sq, axis_rdma_1_sink, axis_rdma_1_src))
        setDefStream(stream)

      for(ii <- 0 until 33){
        for(stream <- List(axi_hbm(ii).aw, axi_hbm(ii).w, axi_hbm(ii).ar, axi_hbm(ii).r, axi_hbm(ii).b))
          setDefStream(stream)
      }
    }
  }

  io.setDefault()

  // axilite control registers
  val ctlReg = new AxiLite4SlaveFactory(io.axi_ctrl)

  // RD mode: read from host and write to HBM
  // WR mode: read from HBM and write to host

  val mode = ctlReg.createReadAndWrite(UInt(2 bits), 0, 0).init(0) // RD: 1  WR: 2
  val addr = ctlReg.createReadAndWrite(UInt(64 bits), 8*8, 0).init(0) // request start addr
  val len = ctlReg.createReadAndWrite(UInt(16 bits), 16*8, 0).init(0) // request length
  val cnt = ctlReg.createReadAndWrite(UInt(64 bits), 24*8, 0).init(0) // request count
  val pid = ctlReg.createReadAndWrite(UInt(6 bits), 32*8, 0).init(0)
  val clk = ctlReg.createReadAndWrite(UInt(64 bits), 40*8, 0).init(0)
  val cntDone = ctlReg.createReadAndWrite(UInt(64 bits), 48*8, 0).init(0)

  val cntWrData, cntRdData, cntReq, cntHbmReq = Reg(UInt(64 bits)).init(0)
  val hbmRdAddr, hbmWrAddr, reqAddr = Reg(UInt(64 bits)).init(0)

  val bpss_rd_req, bpss_wr_req = ReqT()
  for (e <- List(bpss_rd_req, bpss_wr_req)){
    e.vaddr := reqAddr.resized
    e.len := len.resized
    e.stream := False
    e.sync := False
    e.ctl := True
    e.host := True
    e.dest := 0
    e.pid := pid
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
      when(mode(0)) (goto(RD))
      when(mode(1)) (goto(WR))
      when(mode(0) || mode(1)) {
        reqAddr := addr
        hbmRdAddr := 0
        hbmWrAddr := 0
        for (e <- List(clk, mode, cntRdData, cntWrData, cntReq, cntHbmReq, cntDone))
          e.clearAll()
      } // clean
    }

    RD.whenIsActive {
      clk := clk + 1

      // connect axis_sink to hbm
      io.axi_hbm(0).aw.setBurstINCR()
      io.axi_hbm(0).aw.len := ((len >> 6) - 1).resized
      io.axi_hbm(0).aw.size := log2Up(512/8)
      io.axi_hbm(0).aw.addr := hbmWrAddr.resized

      io.axi_hbm(0).b.ready := True

      when(io.axi_hbm(0).aw.fire){
        cntHbmReq := cntHbmReq + 1
        hbmWrAddr := hbmWrAddr + len
      }
      when(cntHbmReq =/= cnt)(io.axi_hbm(0).aw.valid := True)

      io.axi_hbm(0).w.strb.setAll()
//      io.axi_hbm(0).w.setStrb() // strb will be 0??
      io.axi_hbm(0).w.data <> io.axis_host_sink.tdata
      io.axi_hbm(0).w.last <> io.axis_host_sink.tlast
      io.axi_hbm(0).w.valid <> io.axis_host_sink.valid
      io.axi_hbm(0).w.ready <> io.axis_host_sink.ready


      // rd_req
      when(io.bpss_rd_req.fire){
        cntReq := cntReq + 1
        reqAddr := reqAddr + len
      }
      when(cntReq =/= cnt)(io.bpss_rd_req.valid := True)

      // rd_resp
      when(io.bpss_rd_done.fire)(cntDone := cntDone + 1)
      when(io.axis_host_sink.fire) (cntRdData := cntRdData + 1)
      when(cntDone === cnt) (goto(IDLE))
    }


    WR.whenIsActive {

      clk := clk + 1

      // wr_req
      when(io.bpss_wr_req.fire){
        cntReq := cntReq + 1
        reqAddr := reqAddr + len
      }
      when(cntReq =/= cnt)(io.bpss_wr_req.valid := True)

      // connect axis_src to hbm
      io.axi_hbm(0).ar.setBurstINCR()
      io.axi_hbm(0).ar.len := ((len >> 6) - 1).resized
      io.axi_hbm(0).ar.size := log2Up(512/8)
      io.axi_hbm(0).ar.addr := hbmRdAddr.resized

      when(io.axi_hbm(0).ar.fire){
        cntHbmReq := cntHbmReq + 1
        hbmRdAddr := hbmRdAddr + len
      }
      when(cntHbmReq =/= cnt)(io.axi_hbm(0).ar.valid := True)

      io.axi_hbm(0).r.data <> io.axis_host_src.tdata
      io.axi_hbm(0).r.valid <> io.axis_host_src.valid
      io.axi_hbm(0).r.ready <> io.axis_host_src.ready

      // writeCnt
      when(io.axis_host_src.fire) {
        cntWrData := cntWrData + 1
      }

      // wr_resp
      when(io.bpss_wr_done.fire)(cntDone := cntDone + 1)
      when(cntDone === cnt) (goto(IDLE))
    }

  }
}


object HbmRWMain {
  def main(args: Array[String]): Unit = {
    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = LOW), targetDirectory = "rtl").generateVerilog{
      val top = HbmRW()
      top.renameIO()
      top.setDefinitionName("design_user_wrapper_2") // wrapper name in `dynamic_wrapper.sv`
      top
    }
  }
}
