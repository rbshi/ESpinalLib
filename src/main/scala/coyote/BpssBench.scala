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
case class BpssBench() extends Component with SetDefaultIO with RenameIO {

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

    // rdma
    val rdma_1_rd_req = slave Stream StreamData(64)
    val rdma_1_wr_req = slave Stream StreamData(64)
    val rdma_1_rq = slave Stream StreamData(256)
    val rdma_1_sq = master Stream StreamData(256)
    val axis_rdma_1_sink = slave Stream Axi4StreamData(512)
    val axis_rdma_1_src =  master Stream Axi4StreamData(512)

    // tie-off rdma and bpss_wr related
    def setDefault(): Unit ={
      for (stream <- List(rdma_1_rd_req, rdma_1_wr_req, rdma_1_rq, rdma_1_sq, axis_rdma_1_sink, axis_rdma_1_src))
        setDefStream(stream)
    }
  }

  io.setDefault()

  // axilite control registers
  val ctlReg = new AxiLite4SlaveFactory(io.axi_ctrl)


  val mode = ctlReg.createReadAndWrite(UInt(2 bits), 0, 0).init(0) // RD: 1  WR: 2
  val addr = ctlReg.createReadAndWrite(UInt(64 bits), 8*8, 0).init(0) // request start addr
  val len = ctlReg.createReadAndWrite(UInt(16 bits), 16*8, 0).init(0) // request length
  val cnt = ctlReg.createReadAndWrite(UInt(64 bits), 24*8, 0).init(0) // request count
  val pid = ctlReg.createReadAndWrite(UInt(6 bits), 32*8, 0).init(0)
  val clk = ctlReg.createReadAndWrite(UInt(64 bits), 40*8, 0).init(0)
  val cntDone = ctlReg.createReadAndWrite(UInt(64 bits), 48*8, 0).init(0)

  val cntWrData, cntRdData, cntReq = Reg(UInt(64 bits)).init(0)

  val bpss_rd_req, bpss_wr_req = ReqT()
  for (e <- List(bpss_rd_req, bpss_wr_req)){
    e.vaddr := addr.resized
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
  io.axis_host_sink.ready := True

  io.axis_host_src.tdest := 0
  io.axis_host_src.tlast := False
  io.axis_host_src.tkeep.setAll()
  io.axis_host_src.tdata := cntWrData.asBits.resized
  io.axis_host_src.valid := False


  val fsm = new StateMachine {
    val IDLE = new State with EntryPoint
    val RD, WR = new State

    IDLE.whenIsActive {
      when(mode(0)) (goto(RD))
      when(mode(1)) (goto(WR))
      when(mode(0) || mode(1)) {
        for (e <- List(clk, mode, cntRdData, cntWrData, cntReq, cntDone))
          e.clearAll()
      } // clean
    }

    RD.whenIsActive {
      clk := clk + 1
      // rd_req
      when(io.bpss_rd_req.fire){
        cntReq := cntReq + 1
        addr := addr + len
      }
      when(cntReq =/= cnt)(io.bpss_rd_req.valid := True)

      // rd_resp
      when(io.bpss_rd_done.fire)(cntDone := cntDone + 1)
      when(io.axis_host_sink.fire) (cntRdData := cntRdData + 1)
      when(cntDone === cnt) (goto(IDLE))
    }


    WR.whenIsActive {
      // wr_req
      clk := clk + 1
      when(io.bpss_wr_req.fire){
        cntReq := cntReq + 1
        addr := addr + len
      }
      when(cntReq =/= cnt)(io.bpss_wr_req.valid := True)

      // write axis
      when(io.axis_host_src.fire) {
        cntWrData := cntWrData + 1
      }
      when(cntWrData =/= (cnt * (len >> 6))) (io.axis_host_src.valid := True)

      // wr_resp
      when(io.bpss_wr_done.fire)(cntDone := cntDone + 1)
      when(cntDone === cnt) (goto(IDLE))
    }

  }
}


object BpssBenchMain {
  def main(args: Array[String]): Unit = {
    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = LOW), targetDirectory = "rtl").generateVerilog{
      val top = BpssBench()
      top.renameIO()
      top.setDefinitionName("design_user_wrapper_1") // wrapper name in `dynamic_wrapper.sv`
      top
    }
  }
}
