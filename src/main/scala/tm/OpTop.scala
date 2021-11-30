package tm

import spinal.core.{UInt, _}
import spinal.core
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axilite._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import util.RenameIO

case class OpTop(numTxnMan: Int) extends Component with RenameIO {

  val conf = LockTableConfig(8, 64, 8, 10, 10, 8)

  val axiConfig = Axi4Config(
    addressWidth = 64,
    dataWidth    = 512,
    idWidth = 6,
    useStrb = false,
    useBurst = false,
    useId = true,
    useLock      = false,
    useRegion    = false,
    useCache     = false,
    useProt      = false,
    useQos       = false,
    useLen       = true
  )

  val io = new Bundle{
    val m_axi = master(Axi4(axiConfig))
    val req_axi = master(Axi4(axiConfig))
    val s_axi_control = slave(AxiLite4(AxiLite4Config(12, 32)))
  }

  // fixme: how to abstract this away?
  io.req_axi.aw.addr := 0
  io.req_axi.aw.valid := False
  io.req_axi.aw.id := 0
  io.req_axi.aw.size := 0
  io.req_axi.aw.len := 0
  io.req_axi.w.valid := False
  io.req_axi.w.data := 0
  io.req_axi.w.last := False
  io.req_axi.b.ready := True


  // control reg: vitis_kernel interface
  val ctlReg = new AxiLite4SlaveFactory(io.s_axi_control)
  val ap_start = ctlReg.createReadAndWrite(Bool(), 0, 0).init(False)
  val ap_done = ctlReg.createReadOnly(Bool(), 0, 1).init(False)
  val ap_idle = ctlReg.createReadOnly(Bool(), 0, 2).init(True)
  ctlReg.onRead(0)(ap_done:=False)

  // control reg: custom
  val txnLen = ctlReg.createReadAndWrite(UInt(8 bits), 0x10, 0)
  val txnCnt = ctlReg.createReadAndWrite(UInt(8 bits), 0x14, 0)

  val addrOffset = Vec(Reg(UInt(32 bits)), numTxnMan)
  val txnExeCnt = Vec(UInt(32 bits), numTxnMan)
  val txnAbortCnt = Vec(UInt(32 bits), numTxnMan)

  for (i <- 0 until numTxnMan){
    ctlReg.readAndWrite(addrOffset(i), 0x18 + 4 * i, 0)
    ctlReg.readAndWrite(txnExeCnt(i), 0x20 + 4 * i, 0)
    ctlReg.readAndWrite(txnAbortCnt(i), 0x28 + 4 * i, 0)
  }

  // instantiate a TxnManGrp that shares one AXI interface
  val txnManGrp = new TxnManGrp(conf, numTxnMan, axiConfig)

  // instantiate LockTable of one memory channel
  val lt = new LockTableCh(conf, numTxnMan)

  // add opStream for each TxnMan
  val opGrp = ArrayBuffer[OpStream]()
  for (i <- 0 until numTxnMan){
    opGrp += new OpStream(conf, axiConfig.copy(idWidth = axiConfig.idWidth - log2Up(numTxnMan)))
  }

  // txnMan <> LT
  txnManGrp.io.lt_req <> lt.io.lock_req
  txnManGrp.io.lt_resp <> lt.io.lock_resp

  // txnMan <> Op
  (txnManGrp.io.op_req, opGrp.map(_.io.op_req)).zipped.map(_ <> _)
  (txnManGrp.io.op_resp, opGrp.map(_.io.op_resp)).zipped.map(_ <> _)
  (txnManGrp.io.sig_txn_abort, opGrp.map(_.io.sig_txn_abort)).zipped.map(_ <> _)
  (txnManGrp.io.sig_txn_end, opGrp.map(_.io.sig_txn_end)).zipped.map(_ <> _)

  // ctlReg <> Op
  opGrp.foreach(_.io.txn_len := txnLen)
  opGrp.foreach(_.io.txn_cnt := txnCnt)
  (addrOffset, opGrp.map(_.io.addr_offset)).zipped.map(_.resize(conf.unitAddrWidth) <> _)
  (txnExeCnt, opGrp.map(_.io.txn_exe_cnt)).zipped.map(_ <> _)
  (txnAbortCnt, opGrp.map(_.io.txn_abort_cnt)).zipped.map(_ <> _)


  opGrp.foreach(_.io.start := ap_start)

//  val reduced_opdone = opGrp.map(_.io.done).foldLeft(False)(_ && _) // not work??
  val reduced_opdone = opGrp(0).io.done && opGrp(1).io.done

  val runState = Reg(Bool())

  // fixme: ap behavior should be packaged
  when(ap_start){
    ap_start.clear()
    ap_idle.clear()
    runState.set()
  }

  runState.clearWhen(runState && reduced_opdone)
  ap_done.setWhen(runState && reduced_opdone)
  ap_idle.setWhen(runState && reduced_opdone)

  io.m_axi <> txnManGrp.io.axi

  // req_axi <> Op axi via arbiter
  val axiRdArb = Axi4ReadOnlyArbiter(axiConfig, numTxnMan)
  // donot use to readonly here
  axiRdArb.io.output.ar <> io.req_axi.ar
  axiRdArb.io.output.r <> io.req_axi.r
  (axiRdArb.io.inputs, opGrp.map(_.io.axi)).zipped.map(_ <> _)

}


object OpTopMain {
  def main(args: Array[String]) {

    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = LOW), targetDirectory = "rtl").generateVerilog{
      val top = new OpTop(2)
      top.renameIO()
      top
    }
  }
}
