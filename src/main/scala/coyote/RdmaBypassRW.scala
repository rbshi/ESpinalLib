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
case class RdmaBypassRW() extends Component with SetDefaultIO with RenameIO {

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

    // tie-off
    def setDefault(): Unit ={
      for(stream <- List(bpss_rd_req, bpss_wr_req, bpss_rd_done, bpss_wr_done, axis_host_sink, axis_host_src, rdma_1_rd_req, rdma_1_wr_req, rdma_1_sq, rdma_1_rq, axis_rdma_1_sink, axis_rdma_1_src))
        setDefStream(stream)
    }
  }

  io.setDefault()

  // axilite control registers
  val ctlReg = new AxiLite4SlaveFactory(io.axi_ctrl)

  val mode = ctlReg.createReadAndWrite(UInt(3 bits), 0, 0).init(0) // bit0 (IDLE:0 WORK:1) | bit1 (Slve:0, Mstr:1 ) | bit2 (Mstr Rd 0, Mstr Wr 1)
  val len = ctlReg.createReadAndWrite(UInt(32 bits), 1<<6, 0).init(0)
  val cnt = ctlReg.createReadAndWrite(UInt(32 bits), 2<<6, 0).init(0)
  val itvlSq = ctlReg.createReadAndWrite(UInt(16 bits), 3<<6, 0).init(0) // interval of sq request

  val clkTimeOut = ctlReg.createReadAndWrite(UInt(64 bits), 4<<6, 0).init(0)
  val cntSent = ctlReg.createReadAndWrite(UInt(32 bits), 5<<6, 0).init(0)
  val cntRec = ctlReg.createReadAndWrite(UInt(32 bits), 6<<6, 0).init(0)
  val clk = ctlReg.createReadAndWrite(UInt(64 bits), 7<<6, 0).init(0)

  val laddr = ctlReg.createReadAndWrite(UInt(48 bits), 8<<6, 0).init(0)
  val raddr = ctlReg.createReadAndWrite(UInt(48 bits), 9<<6, 0).init(0)
  val nOnFly = ctlReg.createReadAndWrite(UInt(32 bits), 10<<6, 0).init(64)

  val fsm = new StateMachine {
    val IDLE = new State with EntryPoint
    val WORK_MSTR, WORK_SLVE = new State

    val cntSqSent = Reg(UInt(32 bits)).init(0)
    val cntAxiToSend = Reg(UInt(32 bits)).init(0)
    val cntOnFly = Reg(UInt(32 bits)).init(0)
    val cntWord = Reg(UInt(16 bits)).init(0)
    val sqItvlCnt = Reg(UInt(16 bits)).init(0)

    IDLE.whenIsActive{
      when(mode(0)){
        when(mode(1))(goto(WORK_MSTR)) otherwise(goto(WORK_SLVE))
        for (e <- List(cntSent, cntRec, cntSqSent, cntAxiToSend, cntOnFly, cntWord, clk))
          e.clearAll()
      }
    }

    WORK_MSTR.whenIsActive{
      clk := clk + 1
      val rdma_base = RdmaBaseT()
      rdma_base.lvaddr := laddr
      rdma_base.rvaddr := raddr
      rdma_base.len := len
      rdma_base.params := 0

      val rdma_1_sq = RdmaReqT()
      rdma_1_sq.opcode := mode(2).asUInt.resized
      rdma_1_sq.qpn := 0
      rdma_1_sq.id := 0
      rdma_1_sq.host := False
      rdma_1_sq.mode := False
      rdma_1_sq.pkg.assignFromBits(rdma_base.asBits)
      rdma_1_sq.rsrvd := 0
      io.rdma_1_sq.data.assignFromBits(rdma_1_sq.asBits)

      // ready of rd/wr req
      io.rdma_1_rd_req.ready.set()
      io.rdma_1_wr_req.ready.set()
      io.axis_rdma_1_sink.ready.set()

      io.rdma_1_rq.ready.set() // fixme: just in case, rm later

      when(mode(2)) {
        when(sqItvlCnt >= itvlSq && cntSqSent < cnt)(io.rdma_1_sq.valid := True)
        when(io.rdma_1_sq.fire)(sqItvlCnt.clearAll()) otherwise (sqItvlCnt := sqItvlCnt + 1)
        when(io.rdma_1_sq.fire)(cntSqSent := cntSqSent + 1)

        // receive the rd_req
        val incCntToSend = io.rdma_1_rd_req.fire
        val decCntToSend = io.axis_rdma_1_src.fire && io.axis_rdma_1_src.tlast
        cntAxiToSend := cntAxiToSend - decCntToSend.asUInt(1 bit) + incCntToSend.asUInt(1 bit)
        cntSent := cntSent + decCntToSend.asUInt(1 bit)

        io.axis_rdma_1_src.tdata := cntSqSent.asBits.resized
        io.axis_rdma_1_src.tkeep.setAll()
        when(cntAxiToSend > 0) {
          // have packet to send
          io.axis_rdma_1_src.valid.set()
          when(cntWord === ((len >> 6) - 1)) {
            io.axis_rdma_1_src.tlast.set()
          }
          when(io.axis_rdma_1_src.fire) {
            when(cntWord === ((len >> 6) - 1)) {
              cntWord.clearAll()
            } otherwise {
              cntWord := cntWord + 1
            }
          }
        }
      } otherwise {
        when(sqItvlCnt >= itvlSq && cntOnFly < nOnFly && cntSqSent < cnt)(io.rdma_1_sq.valid := True)
        when(io.rdma_1_sq.fire)(sqItvlCnt.clearAll()) otherwise (sqItvlCnt := sqItvlCnt + 1)
        when(io.rdma_1_sq.fire)(cntSqSent := cntSqSent + 1)

        val incCntToRecv = io.rdma_1_sq.fire
        val decCntToRecv = io.axis_rdma_1_sink.fire && io.axis_rdma_1_sink.tlast
        cntOnFly := cntOnFly - decCntToRecv.asUInt(1 bit) + incCntToRecv.asUInt(1 bit)
        cntRec := cntRec + decCntToRecv.asUInt(1 bit)
      }

      when(cntSent===cnt || cntRec===cnt || clk===clkTimeOut){
        mode := 0
        goto(IDLE)
      }
    }


    WORK_SLVE.whenIsActive{
      clk := clk + 1

      io.rdma_1_rd_req.ready.set()
      io.rdma_1_wr_req.ready.set()
      io.axis_rdma_1_sink.ready.set()
      io.rdma_1_rq.ready.set() // fixme: just in case, rm later

      when(mode(2)){
        when(io.axis_rdma_1_sink.fire && io.axis_rdma_1_sink.tlast){cntRec := cntRec + 1}
      } otherwise {
        val incCntToSend = io.rdma_1_rd_req.fire
        val decCntToSend = io.axis_rdma_1_src.fire && io.axis_rdma_1_src.tlast
        cntAxiToSend := cntAxiToSend - decCntToSend.asUInt(1 bit) + incCntToSend.asUInt(1 bit)
        cntSent := cntSent + decCntToSend.asUInt(1 bit)

        io.axis_rdma_1_src.tdata := cntSent.asBits.resized
        io.axis_rdma_1_src.tkeep.setAll()
        when(cntAxiToSend > 0) {
          // have packet to send
          io.axis_rdma_1_src.valid.set()
          when(cntWord === ((len >> 6) - 1)) {
            io.axis_rdma_1_src.tlast.set()
          }
          when(io.axis_rdma_1_src.fire) {
            when(cntWord === ((len >> 6) - 1)) {
              cntWord.clearAll()
            } otherwise {
              cntWord := cntWord + 1
            }
          }
        }
      }

      // time out
      when(cntSent===cnt || cntRec===cnt || clk===clkTimeOut){
        mode := 0
        goto(IDLE)
      }
    }

  }
}


object RdmaBypassRWMain {
  def main(args: Array[String]): Unit = {
    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = LOW), targetDirectory = "rtl").generateVerilog{
      val top = RdmaBypassRW()
      top.renameIO()
      top.setDefinitionName("design_user_wrapper_rdmabpss") // wrapper name in `dynamic_wrapper.sv`
      top
    }
  }
}
