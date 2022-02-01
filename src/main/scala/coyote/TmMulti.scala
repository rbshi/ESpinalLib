package coyote

import spinal.core.{UInt, _}
import spinal.core
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axilite._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import util.RenameIO

import tm._

case class TmMulti(numTxnMan: Int, numLT: Int, numCh: Int, axiConfig: Axi4Config) extends Component with RenameIO {

  // txnIDWidth, unitAddrWidth, htBucketWidth, htTableWidth, llTableWidth, queueCntWidth, key2AddrShift
  // unitAddrWidth (of each channel): log2Up(8GB/64B(per tuple))
  val ltConf = LockTableConfig(8, 22, 6, 9, 4, 4, 6)
  val txnManConf = LockTableConfig(8, ltConf.unitAddrWidth+log2Up(numCh), 6, 9, 4, 4, 6)

  val io = new Bundle{
    val m_axi = Vec(master(Axi4(axiConfig)), numCh)
    val req_axi = Vec(master(Axi4(axiConfig)), numCh)

    val ap_start = in Bool()
    val ap_idle = out(Reg(Bool())).init(True)
    val ap_done = out(Reg(Bool())).init(False)
    val txnLen = in UInt(8 bits)
    val txnCnt = in UInt(16 bits)
    val clkCnt = out(Reg(UInt(64 bits))).init(0)
    val addrOffset = in Vec(UInt(32 bits), numTxnMan * numCh) // fixme: 4GB only
    val txnExeCnt = out(Vec(UInt(16 bits), numTxnMan * numCh))
    val txnAbortCnt = out(Vec(UInt(16 bits), numTxnMan * numCh))
  }

  for (iCh <- 0 until numCh) {
    io.req_axi(iCh).aw.addr := 0
    io.req_axi(iCh).aw.valid := False
    io.req_axi(iCh).aw.id := 0
    io.req_axi(iCh).aw.size := log2Up(512 / 8)
    io.req_axi(iCh).aw.len := 0
    io.req_axi(iCh).w.valid := False
    io.req_axi(iCh).w.data := 0
    io.req_axi(iCh).w.last := True // bug?
    io.req_axi(iCh).b.ready := True
  }


  // instantiate TxnManGrps
  val txnManGrp = ArrayBuffer[TxnManGrp]()
  for (iCh <- 0 until numCh){
    txnManGrp += new TxnManGrp(txnManConf, numTxnMan, axiConfig, iCh)
  }

  // instantiate LockTable of one memory channel
  val lt = Array.fill(numCh)(new LockTableCh(ltConf, numLT))

  // instantiate opStream for each TxnMan
  val opGrp = ArrayBuffer[OpStream]()
  for (_ <- 0 until numTxnMan*numCh){
    opGrp += new OpStream(txnManConf, axiConfig.copy(idWidth = axiConfig.idWidth - log2Up(numTxnMan)))
  }

  // txnMan <> LT
  // interconnect lt_req/lt_resp

  val ltChReqArb = Array.fill(numCh)(StreamArbiterFactory.roundRobin.noLock.build(LockReq(ltConf), numCh))
  val ltChRespArb = Array.fill(numCh)(StreamArbiterFactory.roundRobin.noLock.build(LockResp(txnManConf), numCh))

  // lt_req

  for (iTxnManGrp <- 0 until numCh){
    // use MSB of txnMan lock_addr to select the LTCh
    val ltReq = StreamDemux(txnManGrp(iTxnManGrp).io.lt_req, txnManGrp(iTxnManGrp).io.lt_req.lock_addr(txnManConf.unitAddrWidth-1 downto ltConf.unitAddrWidth).resized, numCh)

    (ltChReqArb.map(_.io.inputs(iTxnManGrp).lock_addr) , ltReq.map(_.lock_addr(ltConf.unitAddrWidth-1 downto 0))).zipped.map(_ <> _)
    (ltChReqArb.map(_.io.inputs(iTxnManGrp).txn_id), ltReq.map(_.txn_id)).zipped.map(_ <> _)
    (ltChReqArb.map(_.io.inputs(iTxnManGrp).lock_type), ltReq.map(_.lock_type)).zipped.map(_ <> _)
    (ltChReqArb.map(_.io.inputs(iTxnManGrp).lock_upgrade), ltReq.map(_.lock_upgrade)).zipped.map(_ <> _)
    (ltChReqArb.map(_.io.inputs(iTxnManGrp).lock_release), ltReq.map(_.lock_release)).zipped.map(_ <> _)
    (ltChReqArb.map(_.io.inputs(iTxnManGrp).lock_idx), ltReq.map(_.lock_idx)).zipped.map(_ <> _)
    (ltChReqArb.map(_.io.inputs(iTxnManGrp).ready), ltReq.map(_.ready)).zipped.map(_ <> _)
    (ltChReqArb.map(_.io.inputs(iTxnManGrp).valid), ltReq.map(_.valid)).zipped.map(_ <> _)
  }

  for (iLTCh <- 0 until numCh) {
    ltChReqArb(iLTCh).io.output >/-> lt(iLTCh).io.lock_req // buggy if with fully pipe
  }

  // lt_resp
  for (iLTCh <- 0 until numCh) {

    // use MSB of txn_id to dispatch the lock_resp
    val ltResp = StreamDemux(lt(iLTCh).io.lock_resp, lt(iLTCh).io.lock_resp.txn_id(txnManConf.txnIDWidth-1 downto log2Up(numTxnMan)).resized, numCh)

    val addrHighBit = UInt(log2Up(numCh) bits)
    addrHighBit := iLTCh

    (ltChRespArb.map(_.io.inputs(iLTCh).lock_addr), ltResp.map(addrHighBit ## _.lock_addr)).zipped.map(_ <> _.asUInt)
    (ltChRespArb.map(_.io.inputs(iLTCh).txn_id), ltResp.map(_.txn_id)).zipped.map(_ <> _)
    (ltChRespArb.map(_.io.inputs(iLTCh).resp_type), ltResp.map(_.resp_type)).zipped.map(_ <> _)
    (ltChRespArb.map(_.io.inputs(iLTCh).lock_type), ltResp.map(_.lock_type)).zipped.map(_ <> _)
    (ltChRespArb.map(_.io.inputs(iLTCh).lock_upgrade), ltResp.map(_.lock_upgrade)).zipped.map(_ <> _)
    (ltChRespArb.map(_.io.inputs(iLTCh).lock_idx), ltResp.map(_.lock_idx)).zipped.map(_ <> _)
    (ltChRespArb.map(_.io.inputs(iLTCh).ready), ltResp.map(_.ready)).zipped.map(_ <> _)
    (ltChRespArb.map(_.io.inputs(iLTCh).valid), ltResp.map(_.valid)).zipped.map(_ <> _)

  }

  for (iTxnManGrp <- 0 until numCh){
    ltChRespArb(iTxnManGrp).io.output >/-> txnManGrp(iTxnManGrp).io.lt_resp // buggy if with fully pipe
  }

  // txnMan <> Op
  for (iCh <- 0 until numCh){
    (txnManGrp(iCh).io.op_req, opGrp.slice(iCh*numTxnMan, (iCh+1)*numTxnMan).map(_.io.op_req)).zipped.map(_ << _)
    (txnManGrp(iCh).io.op_resp, opGrp.slice(iCh*numTxnMan, (iCh+1)*numTxnMan).map(_.io.op_resp)).zipped.map(_ >> _)
    (txnManGrp(iCh).io.sig_txn_abort, opGrp.slice(iCh*numTxnMan, (iCh+1)*numTxnMan).map(_.io.sig_txn_abort)).zipped.map(_ <> _)
    (txnManGrp(iCh).io.sig_txn_end, opGrp.slice(iCh*numTxnMan, (iCh+1)*numTxnMan).map(_.io.sig_txn_end)).zipped.map(_ <> _)
  }

  // ctlReg <> Op
  opGrp.foreach(_.io.txn_len := io.txnLen)
  opGrp.foreach(_.io.txn_cnt := io.txnCnt)
  (io.addrOffset, opGrp.map(_.io.addr_offset)).zipped.map(_.resize(axiConfig.addressWidth) <> _)
  (io.txnExeCnt, opGrp.map(_.io.txn_exe_cnt)).zipped.map(_ <> _)
  (io.txnAbortCnt, opGrp.map(_.io.txn_abort_cnt)).zipped.map(_ <> _)


  opGrp.foreach(_.io.start := io.ap_start)
  val reduced_opdone = opGrp.map(_.io.done).foldLeft(True)(_ && _) // && reduction: start with True
  val runState = Reg(Bool())

  when(io.ap_start){
//    io.ap_start.clear() // control in top
    io.ap_idle.clear()
    io.ap_done.clear()
    runState.set()
    // clear clkCnt
    io.clkCnt := 0
  }

  when(~io.ap_idle)(io.clkCnt := io.clkCnt + 1)

  runState.clearWhen(runState && reduced_opdone)
  io.ap_done.setWhen(runState && reduced_opdone)
  io.ap_idle.setWhen(runState && reduced_opdone)

  for (iCh <- 0 until numCh){
    io.m_axi(iCh) <> txnManGrp(iCh).io.axi

    // req_axi <> Op axi via arbiter
    val axiRdArb = Axi4ReadOnlyArbiter(axiConfig, numTxnMan)
    // pipe the axi interface
    axiRdArb.io.output.ar >/->  io.req_axi(iCh).ar
    axiRdArb.io.output.r <-/< io.req_axi(iCh).r
    //    (axiRdArb.io.inputs, opGrp.slice(iCh*numTxnMan, (iCh+1)*numTxnMan).map(_.io.axi)).zipped.map(_ <> _)
    (axiRdArb.io.inputs.map(_.ar), opGrp.slice(iCh*numTxnMan, (iCh+1)*numTxnMan).map(_.io.axi.ar)).zipped.map(_ <-/< _)
    (axiRdArb.io.inputs.map(_.r), opGrp.slice(iCh*numTxnMan, (iCh+1)*numTxnMan).map(_.io.axi.r)).zipped.map(_ >/-> _)

  }
}
