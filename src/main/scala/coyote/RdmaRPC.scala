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
case class RdmaRPC() extends Component with SetDefaultIO with RenameIO {

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
    val rdma_1_rd_req = slave Stream StreamData(64)
    val rdma_1_wr_req = slave Stream StreamData(64)
    val rdma_1_rq = slave Stream StreamData(256)
    val rdma_1_sq = master Stream StreamData(256)
    val axis_rdma_1_sink = slave Stream Axi4StreamData(512)
    val axis_rdma_1_src =  master Stream Axi4StreamData(512)

    // tie-off
    def setDefault(): Unit ={
      for(stream <- List(bpss_rd_req, bpss_wr_req, bpss_rd_done, bpss_wr_done, axis_host_sink, axis_host_src, rdma_1_rd_req, rdma_1_wr_req, axis_rdma_1_sink, axis_rdma_1_src))
        setDefStream(stream)
    }
  }

  io.setDefault()

  // axilite control registers
  val ctlReg = new AxiLite4SlaveFactory(io.axi_ctrl)

  val mode = ctlReg.createReadAndWrite(UInt(2 bits), 0, 0).init(0) // bit0 (IDLE:0 WORK:1) bit1 (Slve:0, Mstr:1 )
  val cnt = ctlReg.createReadAndWrite(UInt(32 bits), 8*8, 0).init(0)
  val cntOnFly = ctlReg.createReadAndWrite(UInt(32 bits), 16*8, 0).init(0)
  val clkTimeOut = ctlReg.createReadAndWrite(UInt(64 bits), 24*8, 0).init(0)
  val cntSent = ctlReg.createReadAndWrite(UInt(32 bits), 32*8, 0).init(0)
  val cntRec = ctlReg.createReadAndWrite(UInt(32 bits), 40*8, 0).init(0)
  val clk = ctlReg.createReadAndWrite(UInt(64 bits), 48*8, 0).init(0)

  val rdma_1_sq = RdmaReqT()
  rdma_1_sq.opcode := 2 // FIXME: RPC parsed code = 0x18
  rdma_1_sq.qpn := 0
  rdma_1_sq.id := 0
  rdma_1_sq.host := False
  rdma_1_sq.mode := False
  rdma_1_sq.pkg := 0xff
  rdma_1_sq.rsrvd := 0

  io.rdma_1_sq.data.assignFromBits(rdma_1_sq.asBits)
  io.rdma_1_sq.valid := False
  io.rdma_1_rq.ready := False

  val fsm = new StateMachine {
    val IDLE = new State with EntryPoint
    val WORK_MSTR, WORK_SLVE = new State

    IDLE.whenIsActive{
      when(mode(0)){
        when(mode(1))(goto(WORK_MSTR)) otherwise(goto(WORK_SLVE))
        for (e <- List(cntSent, cntRec, clk))
          e.clearAll()
      }
    }

    WORK_MSTR.whenIsActive{
      clk := clk + 1

      // send
      when(cntSent<cnt && (cntSent - cntRec)<cntOnFly)(io.rdma_1_sq.valid := True)
      when(io.rdma_1_sq.fire)(cntSent := cntSent + 1)

      // rec
      io.rdma_1_rq.ready := True
      when(io.rdma_1_rq.fire)(cntRec := cntRec + 1)

      when((cntSent===cnt && cntRec===cnt) || clk===clkTimeOut){
        mode := 0
        goto(IDLE)
      }
    }

    WORK_SLVE.whenIsActive{
      clk := clk + 1

      // roundtrip
      val rpcFifo = StreamFifo(UInt(192 bits), 128)
      rpcFifo.io.push.ready <> io.rdma_1_rq.ready
      rpcFifo.io.push.valid <> io.rdma_1_rq.valid
      rpcFifo.io.push.payload <> io.rdma_1_rq.data(223 downto 32).asUInt // fixme
      rpcFifo.io.pop.ready <> io.rdma_1_sq.ready
      rpcFifo.io.pop.valid <> io.rdma_1_sq.valid
      rdma_1_sq.pkg := (rpcFifo.io.pop.payload<<4).resized
      
      when(io.rdma_1_rq.fire)(cntRec := cntRec + 1)
      when(io.rdma_1_sq.fire)(cntSent := cntSent + 1)

      // time out
      when((cntSent===cnt && cntRec===cnt) || clk===clkTimeOut){
        mode := 0
        goto(IDLE)
      }
    }

  }

}


object RdmaRPCMain {
  def main(args: Array[String]): Unit = {
    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = LOW), targetDirectory = "rtl").generateVerilog{
      val top = RdmaRPC()
      top.renameIO()
      top.setDefinitionName("design_user_wrapper_rpc") // wrapper name in `dynamic_wrapper.sv`
      top
    }
  }
}
