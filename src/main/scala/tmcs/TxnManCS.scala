package tmcs

import spinal.core.{UInt, _}
import spinal.core.Mem
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm.StateMachine
import util._

import scala.language.postfixOps

/**
* Number of node, channel, lock of each channel; txnMan on each node
* */
case class SysConfig(nNode: Int, nCh: Int, nLock: Int, nLtPart: Int, nTxnMan: Int){
  val wNId = log2Up(nNode)
  val wCId = log2Up(nCh)
  val wTId = log2Up(nLock)
  val wTxnManId = log2Up(nTxnMan)

  // txnMan
  val nTxnCS = 64 // concurrent txn count, limited by axi arid (6 bits)
  val maxTxnLen = 64 // max len of each txn, space of on-chip mem (include the txnHd)
  val wMaxTxnLen = log2Up(maxTxnLen)
  val wLkIdx = log2Up(maxTxnLen) // lkIdx in one Txn, for OoO response
  val wTxnId = log2Up(nTxnCS)

  val dTxnMem = nTxnCS * maxTxnLen
  val wTxnMemAddr = log2Up(dTxnMem)

  // lkTable
  val wOwnerCnt = 4
  val wHtValNW = 1 + wOwnerCnt
  val wHtBucket = 6
  val wHtTable = log2Up(nLock)

  val wChSize = 28 // 256MB of each channel (used as offset for global addressing)
}

case class TxnEntry(conf: SysConfig) extends Bundle {
  val nId = UInt(conf.wNId bits)
  val cId = UInt(conf.wCId bits)
  val tId = UInt(conf.wTId bits)
  val lkType = Bits(2 bits)
  val wLen = UInt(3 bits) // len(tuple)=2^wLen; maxLen = 64B << 7 = 8192 B

  def toLkReq(txnManId: UInt, curTxnId: UInt, release: Bool, lkIdx: UInt): LkReq = {
    val ret = LkReq(conf)
    ret.nId := this.nId
    ret.cId := this.cId
    ret.tId := this.tId
    ret.txnManId := txnManId
    ret.txnId:= curTxnId
    ret.lkType := this.lkType(0)
    ret.lkUpgrade := this.lkType(1)
    ret.lkRelease := False
    ret.lkIdx := lkIdx
    ret.wLen := this.wLen
    ret
  }
}

case class TxnManCSIO(conf: SysConfig, axiConf: Axi4Config) extends Bundle with SetDefaultIO {

  // local/rmt req interface
  val lkReqLoc, lkReqRmt = master Stream LkReq(conf)
  val lkRespLoc, lkRespRmt = slave Stream LkResp(conf)

  // rd/wr data from/to remote
  val rdRmt = slave Stream UInt(512 bits)
  val wrRmt = master Stream UInt(512 bits)

  // local data axi
  val axi = master(Axi4(axiConf))

  // cmd axi
  val cmdAxi = master(Axi4(axiConf))

  // control signals (wire the input to the top AXIL registers)
  val start = in Bool() //NOTE: hold for 1 cycle

  // txnMan config
  val nId = in UInt(conf.wNId bits)
  val txnManId = in UInt(conf.wTxnManId bits)
  val txnNumTotal = in UInt(32 bits)
  val cmdAddrOffs = in UInt(32 bits) //NOTE: unit size 64B


  val done = out(Reg(Bool())).init(False)
  val cntTxnCmt, cntTxnAbt, cntTxnLd = out(Reg(UInt(32 bits))).init(0)
  val cntClk = out(Reg(UInt(40 bits))).init(0)

  def setDefault() = {

    for (e <- List(lkRespLoc, lkRespRmt, rdRmt, wrRmt))
      setDefStream(e)

    // axi
    axi.ar.valid.clear()
    axi.aw.valid.clear()
    axi.w.valid.clear()

    cmdAxi.ar.valid.clear()
    cmdAxi.aw.valid.clear()
    cmdAxi.w.valid.clear()

    // cmdAxi is read only
    cmdAxi.aw.addr := 0
    cmdAxi.aw.id := 0
    cmdAxi.aw.len := 0
    cmdAxi.aw.size := log2Up(512/8)
    cmdAxi.aw.setBurstINCR()
    cmdAxi.w.last := False
    cmdAxi.w.data.clearAll()
    cmdAxi.b.ready := True
    cmdAxi.r.ready := False


    if(axiConf.useStrb) {
      axi.w.strb.setAll()
      cmdAxi.w.strb.setAll()
    }

  }
}

/**
* FIXME: each channel may contain multiple tables, the tId to address translation logic will be dedicated
* Txn entry: node_id, channel_id, lock_id, lock_type, data length (2^n B), (data is omitted here, included in the txn logic)
* */

class TxnManCS(conf: SysConfig, axiConf: Axi4Config) extends Component with RenameIO with SetDefaultIO {

  val io = TxnManCSIO(conf, axiConf)
  io.setDefault()

  // lkGet and lkRlse are be arbitrated and sent to io
  val lkReqGetLoc, lkReqRlseLoc, lkReqGetRmt, lkReqRlseRmt = Stream(LkReq(conf))
  io.lkReqLoc <> StreamArbiterFactory.roundRobin.onArgs(lkReqGetLoc, lkReqRlseLoc)
  io.lkReqRmt <> StreamArbiterFactory.roundRobin.onArgs(lkReqGetRmt, lkReqRlseRmt)

  for (e <- List(lkReqGetLoc, lkReqRlseLoc, lkReqGetRmt, lkReqRlseRmt))
    e.valid := False

  val txnMem = Mem(TxnEntry(conf), conf.dTxnMem)

  // store wr tnxs to commit
  val txnWrMemLoc = Mem(TxnEntry(conf), conf.dTxnMem)
  val txnWrMemRmt = Mem(TxnEntry(conf), conf.dTxnMem)
  // store the obtained lock items to release
  val lkMemLoc = Mem(LkResp(conf), conf.dTxnMem)
  val lkMemRmt = Mem(LkResp(conf), conf.dTxnMem)

  // context registers
  // NOTE: separate local / remote; some reg is redundant (may be simplified)
  val cntLkReqLoc, cntLkReqRmt, cntLkRespLoc, cntLkRespRmt, cntLkHoldLoc, cntLkHoldRmt, cntLkReqWrLoc, cntLkReqWrRmt, cntLkHoldWrLoc, cntLkHoldWrRmt, cntCmtReqLoc, cntCmtReqRmt, cntCmtRespLoc, cntCmtRespRmt, cntRlseReqLoc, cntRlseReqRmt, cntRlseReqWrLoc, cntRlseReqWrRmt, cntRlseRespLoc, cntRlseRespRmt = Vec(Reg(UInt(conf.wMaxTxnLen bits)), conf.nTxnCS)
  // status register
  val rAbort = Vec(RegInit(False), conf.nTxnCS)
  val rReqDone, rRlseDone = Vec(RegInit(True), conf.nTxnCS) // init to True, to trigger the first txnMem load and issue

  /**
   * component1: lock request
   */
  val compLkReq = new StateMachine {
    val CS_TXN = new State with EntryPoint
    val RD_TXN = new State

    val curTxnId = Reg(UInt(conf.wTxnId bits)).init(0)

    val txnMemRdCmd = Stream(UInt(conf.wTxnMemAddr bits))
    txnMemRdCmd.valid := False
    txnMemRdCmd.payload := 0
    val txnMemRd = txnMem.streamReadSync(txnMemRdCmd)
    txnMemRd.ready := False

    val txnLen, reqLen = Reg(UInt(conf.wMaxTxnLen bits)).init(0)
    val txnOffs = curTxnId << conf.wMaxTxnLen

    for (e <- List(lkReqGetLoc, lkReqGetRmt))
      e.payload := txnMemRd.toLkReq(io.txnManId, curTxnId, False, reqLen)

    CS_TXN.whenIsActive {
      // txn is invalid (all lk reqs have been sent OR txn abort)
      when(rReqDone(curTxnId) || rAbort(curTxnId)) {
        curTxnId := curTxnId + 1
      } otherwise {
        // read the txn hd cmd, will be ready in the next cycle
        txnMemRdCmd.valid := True
        txnMemRdCmd.payload := txnOffs
        txnMemRd.ready := True
      }

      when(txnMemRd.fire){
        txnMemRdCmd.valid := False
        txnLen := txnMemRd.payload.asBits(conf.wMaxTxnLen-1 downto 0).asUInt
        goto(RD_TXN)
      }
    }

    val lkReqFire = lkReqGetLoc.fire || lkReqGetRmt.fire
    val isLocal = txnMemRd.nId === io.nId

    RD_TXN.whenIsActive {

      val txnCntAddr = txnOffs + reqLen + 1
      txnMemRdCmd.valid := True
      txnMemRdCmd.payload := lkReqFire ? (txnCntAddr + 1) | txnCntAddr  // backpressure
      txnMemRd.ready := lkReqFire

      when(lkReqFire){
        reqLen := reqLen + 1
        switch(isLocal) {
          is(True)(cntLkReqLoc(curTxnId) := cntLkReqLoc(curTxnId) + 1)
          is(False) (cntLkReqRmt(curTxnId) := cntLkReqRmt(curTxnId) + 1)
        }
        when(txnMemRd.lkType(0)){
          switch(isLocal) {
            is(True){
              txnWrMemLoc.write(txnOffs+cntLkReqWrLoc(curTxnId), txnMemRd.payload)
              cntLkReqWrLoc(curTxnId) := cntLkReqWrLoc(curTxnId) + 1
            }
            is(False){
              txnWrMemRmt.write(txnOffs+cntLkReqWrRmt(curTxnId), txnMemRd.payload)
              cntLkReqWrRmt(curTxnId) := cntLkReqWrRmt(curTxnId) + 1
            }
          }
        }
      }

      // issue lkReq to local / rmt
      switch(isLocal) {
        is(True)(lkReqGetLoc.valid := txnMemRd.valid)
        is(False)(lkReqGetRmt.valid := txnMemRd.valid)
      }

      //NOTE: lkReq of next Txn OR if abort, stop issue the req
      when((lkReqFire && (reqLen===(txnLen-1))) || rAbort(curTxnId)){
        txnMemRdCmd.valid := False
        rReqDone(curTxnId).set()
        reqLen.clearAll()
        curTxnId := curTxnId + 1
        goto(CS_TXN)
      }

    }
  }


  /**
  * component2: lock response
  *
  * */
  val compLkRespLoc = new StateMachine {

    val WAIT_RESP = new State with EntryPoint
    val LOCAL_RD_REQ = new State

    val rLkResp = RegNextWhen(io.lkRespLoc, io.lkRespLoc.fire)
    val curTxnId = io.lkRespLoc.txnId
    val txnOffs = curTxnId << conf.wMaxTxnLen

    val rCurTxnId = RegNextWhen(curTxnId, io.lkRespLoc.fire)
    val getAllRlse = (cntRlseRespLoc(rCurTxnId) === cntLkHoldLoc(rCurTxnId)) && (cntRlseRespRmt(rCurTxnId) === cntLkHoldRmt(rCurTxnId))
    val getAllLkResp = (cntLkReqLoc(rCurTxnId) === cntLkRespLoc(rCurTxnId)) && (cntLkReqRmt(rCurTxnId) === cntLkRespRmt(rCurTxnId))

    // Since rReqDone will have two cycles latency (io.lkResp (c0) -> R -> rAbort (c1) -> R -> rReqDone (c2)), the following logic occurs in c1, so use ~(xxx) as extra statements to avoid lkReq happens in c2.
    val firstReqAbt = rAbort(rCurTxnId) && ~(io.lkReqLoc.fire && io.lkReqLoc.txnId===rCurTxnId) && ~(io.lkReqRmt.fire && io.lkReqRmt.txnId===rCurTxnId)

    val rFire = RegNext(io.lkRespLoc.fire)

    // FIXME: may conflict with LkRespRmt
    // release after get all lkResp and rReqDone
    when(rFire && getAllRlse && getAllLkResp && (rReqDone(rCurTxnId) || firstReqAbt)){
      rRlseDone(rCurTxnId).set()
      when(rAbort(rCurTxnId)) (io.cntTxnAbt := io.cntTxnAbt +1) otherwise(io.cntTxnCmt := io.cntTxnCmt +1)
    }

    WAIT_RESP.whenIsActive {
      io.lkRespLoc.ready := True

      when(io.lkRespLoc.fire) {

        when(io.lkRespLoc.respType === LockRespType.grant) {
          // note: ooo arrive
          // should use lkHold as wr address
          lkMemLoc.write(txnOffs+cntLkHoldLoc(curTxnId), io.lkRespLoc.payload)

          // NOTE:
          cntLkRespLoc(curTxnId) := cntLkRespLoc(curTxnId) + 1
          cntLkHoldLoc(curTxnId) := cntLkHoldLoc(curTxnId) + 1

          when(io.lkRespLoc.lkType){
            cntLkHoldWrLoc(curTxnId) := cntLkHoldWrLoc(curTxnId) + 1
          } otherwise {
            // issue local rd req once get the lock
            goto(LOCAL_RD_REQ)
          }
        }

        when(io.lkRespLoc.respType === LockRespType.abort) {
          // FIXME: rAbort set conflict
          rAbort(curTxnId) := True
          cntLkRespLoc(curTxnId) := cntLkRespLoc(curTxnId) + 1
        }

        when(io.lkRespLoc.respType === LockRespType.release) {
          cntRlseRespLoc(curTxnId) := cntRlseRespLoc(curTxnId) + 1
//          when(cntRlseRespLoc(curTxnId) === cntLkHoldLoc(curTxnId) - 1){
//            rRlseDone(curTxnId) := True
//          }
        }
      }
    }

    // TODO: data path
    // FIXME: tId -> addr translation logic
    io.axi.ar.addr := (((rLkResp.tId << rLkResp.wLen) << 6) + (rLkResp.cId << conf.wChSize)).resized
    io.axi.ar.id := rLkResp.txnId
    io.axi.ar.len := (U(1)<<rLkResp.wLen) -1
    io.axi.ar.size := log2Up(512/8)
    io.axi.ar.setBurstINCR()

    LOCAL_RD_REQ.whenIsActive {
      io.axi.ar.valid := True
      when(io.axi.ar.fire)(goto(WAIT_RESP))
    }

  }

  val compLkRespRmt = new StateMachine {

    val WAIT_RESP = new State with EntryPoint
    val RMT_RD_CONSUME = new State

    val rLkResp = RegNextWhen(io.lkRespRmt, io.lkRespRmt.fire)
    val nBeat = Reg(UInt(8 bits)).init(0)
    val curTxnId = io.lkRespRmt.txnId
    val txnOffs = curTxnId << conf.wMaxTxnLen

    val rCurTxnId = RegNextWhen(curTxnId, io.lkRespRmt.fire)
    val getAllRlse = (cntRlseRespLoc(rCurTxnId) === cntLkHoldLoc(rCurTxnId)) && (cntRlseRespRmt(rCurTxnId) === cntLkHoldRmt(rCurTxnId))
    val getAllLkResp = (cntLkReqLoc(rCurTxnId) === cntLkRespLoc(rCurTxnId)) && (cntLkReqRmt(rCurTxnId) === cntLkRespRmt(rCurTxnId))

    // io.lkResp -> R -> rAbort -> R -> rReqDone
    val firstReqAbt = rAbort(rCurTxnId) && ~(io.lkReqLoc.fire && io.lkReqLoc.txnId===rCurTxnId) && ~(io.lkReqRmt.fire && io.lkReqRmt.txnId===rCurTxnId)

    val rFire = RegNext(io.lkRespRmt.fire)

    // release after get all lkResp and rReqDone
    when(rFire && getAllRlse && getAllLkResp && (rReqDone(rCurTxnId) || firstReqAbt)){
      rRlseDone(rCurTxnId).set()
      when(rAbort(rCurTxnId)) (io.cntTxnAbt := io.cntTxnAbt +1) otherwise(io.cntTxnCmt := io.cntTxnCmt +1)
    }


    WAIT_RESP.whenIsActive {
      io.lkRespRmt.ready := True

      when(io.lkRespRmt.fire) {

        when(io.lkRespRmt.respType === LockRespType.grant) {
          // note: ooo arrive
          lkMemRmt.write(txnOffs+cntLkHoldRmt(curTxnId), io.lkRespRmt.payload)

          cntLkRespRmt(curTxnId) := cntLkRespRmt(curTxnId) + 1
          cntLkHoldRmt(curTxnId) := cntLkHoldRmt(curTxnId) + 1

          when(io.lkRespRmt.lkType){
            // FIXME: not used
            cntLkHoldWrRmt(curTxnId) := cntLkHoldWrRmt(curTxnId) + 1
          } otherwise {
            goto(RMT_RD_CONSUME)
          }
        }

        when(io.lkRespRmt.respType === LockRespType.abort) {
          // FIXME: rAbort set conflict
          rAbort(curTxnId) := True
          cntLkRespRmt(curTxnId) := cntLkRespRmt(curTxnId) + 1
        }

        when(io.lkRespRmt.respType === LockRespType.release) {
          cntRlseRespRmt(curTxnId) := cntRlseRespRmt(curTxnId) + 1
        }
      }
    }

    // REMOTE_RD data come with the lkResp, consume it!
    RMT_RD_CONSUME.whenIsActive {
      io.rdRmt.ready := True
      when(io.rdRmt.fire) {
        nBeat := nBeat + 1
        when(nBeat === (U(1)<<rLkResp.wLen) -1){
          nBeat.clearAll()
          goto(WAIT_RESP)
        }
      }
    }
  }


  /**
  * component3: axi response
  * */

  val compAxiResp = new Area {
    // rd resp
    io.axi.r.ready := True
    // write resp
    io.axi.b.ready := True
    when(io.axi.b.fire) {
      cntCmtRespLoc(io.axi.b.id) := cntCmtRespLoc(io.axi.b.id) + 1
    }
  }


  /**
  * component4: txnCommit
  * */

  val compTxnCmtLoc = new StateMachine {

    val CS_TXN = new State with EntryPoint
    val LOCAL_AW, LOCAL_W = new State

    val curTxnId = Reg(UInt(conf.wTxnId bits)).init(0)
    val txnOffs = curTxnId << conf.wMaxTxnLen

    val cmtTxn = txnWrMemLoc.readSync(txnOffs+cntCmtReqLoc(curTxnId)) //
    val rCmtTxn = RegNext(cmtTxn)

    val nBeat = Reg(UInt(8 bits)).init(0)

    val getAllLkResp = (cntLkReqLoc(curTxnId) === cntLkRespLoc(curTxnId)) && (cntLkReqRmt(curTxnId) === cntLkRespRmt(curTxnId))

    CS_TXN.whenIsActive {
      /**
      * 1. get All lk resp
      * 2. sent out all lk req
      * 3. no abort
      * 4. send #cmtReq < #LkHoldWr local
      * */
      val cmtCret = getAllLkResp && rReqDone(curTxnId) && ~rAbort(curTxnId) && (cntCmtReqLoc(curTxnId) < cntLkHoldWrLoc(curTxnId))
      when(cmtCret) {
        goto(LOCAL_AW)
      } otherwise {
        curTxnId := curTxnId + 1
      }
    }

    // TODO: data path
    // fixme: tId -> addr translation logic
    io.axi.aw.addr := ((cmtTxn.tId << cmtTxn.wLen) + (cmtTxn.cId << conf.wChSize)).resized
    io.axi.aw.id := curTxnId
    io.axi.aw.len := (U(1)<<cmtTxn.wLen) -1
    io.axi.aw.size := log2Up(512/8)
    io.axi.aw.setBurstINCR()

    LOCAL_AW.whenIsActive {
      io.axi.aw.valid := True
      when(io.axi.aw.fire){
        goto(LOCAL_W)
      }
    }


    io.axi.w.data.setAll()
    io.axi.w.last := (nBeat === (U(1)<<rCmtTxn.wLen) -1)

    LOCAL_W.whenIsActive {
      io.axi.w.valid := True
      when(io.axi.w.fire){
        nBeat := nBeat + 1
        when(io.axi.w.last) {
          cntCmtReqLoc(curTxnId) := cntCmtReqLoc(curTxnId) + 1
          nBeat.clearAll()
          goto(CS_TXN)
        }
      }
      // io.axi.b will be tackled in component3
    }
  }


  /**
  * component5: lkRelease
  *
  * */

  val compLkRlseLoc = new StateMachine {

    val CS_TXN = new State with EntryPoint
    val LK_RLSE = new State

    val curTxnId = Reg(UInt(conf.wTxnId bits)).init(0)
    val txnOffs = curTxnId << conf.wMaxTxnLen
    val lkItem = lkMemLoc.readSync(txnOffs+cntRlseReqLoc(curTxnId))
    val getAllLkResp = (cntLkReqLoc(curTxnId) === cntLkRespLoc(curTxnId)) && (cntLkReqRmt(curTxnId) === cntLkRespRmt(curTxnId))

    CS_TXN.whenIsActive {

      /**
      * 1. get all lk resp
      * 2. rAbort || rReqDone
      * 3. (rAbort || WrLoc <= CmtRespLoc )
      * 4. send #RlseReq loc === #LkHold local
      * */
      val rlseCret = getAllLkResp && (rAbort(curTxnId) || rReqDone(curTxnId)) && (rAbort(curTxnId) || cntRlseReqWrLoc(curTxnId)<=cntCmtRespLoc(curTxnId)) && cntRlseReqLoc(curTxnId) < cntLkHoldLoc(curTxnId)
      when(rlseCret) {
        goto(LK_RLSE)
      } otherwise {
        curTxnId := curTxnId + 1
      }
    }

    lkReqRlseLoc.payload := lkItem.toLkReq(True, cntRlseReqLoc(curTxnId))
    LK_RLSE.whenIsActive {
      lkReqRlseLoc.valid := True
      when(lkReqRlseLoc.fire){
        cntRlseReqLoc(curTxnId) := cntRlseReqLoc(curTxnId) + 1
        goto(CS_TXN)
      }
      when(lkReqRlseLoc.fire && lkItem.lkType){
        cntRlseReqWrLoc(curTxnId) := cntRlseReqWrLoc(curTxnId) + 1
      }
    }

  }


  val compLkRlseRmt = new StateMachine {

    val CS_TXN = new State with EntryPoint
    val RMT_LK_RLSE, RMT_WR = new State

    val curTxnId = Reg(UInt(conf.wTxnId bits)).init(0)
    val nBeat = Reg(UInt(8 bits)).init(0)

    val txnOffs = curTxnId << conf.wMaxTxnLen
    val lkItem = lkMemRmt.readSync(txnOffs+cntRlseReqRmt(curTxnId))
    val getAllLkResp = (cntLkReqRmt(curTxnId) === cntLkRespRmt(curTxnId)) && (cntLkReqRmt(curTxnId) === cntLkRespRmt(curTxnId))

    CS_TXN.whenIsActive {

      /**
      * 1. get all lk resp
      * 2. rAbort || rReqDone
      * 3. send #RlseReq loc === #LkHold local
      * */

      val rlseCret = getAllLkResp && (rAbort(curTxnId) || rReqDone(curTxnId)) && cntRlseReqRmt(curTxnId) < cntLkHoldRmt(curTxnId)

      when(rlseCret) {
        goto(RMT_LK_RLSE)
      } otherwise {
        curTxnId := curTxnId + 1
      }
    }

    lkReqRlseRmt.payload := lkItem.toLkReq(True, cntRlseReqRmt(curTxnId))
    RMT_LK_RLSE.whenIsActive {
      lkReqRlseRmt.valid := True
      when(lkReqRlseRmt.fire){
        cntRlseReqRmt(curTxnId) := cntRlseReqRmt(curTxnId) + 1

        // when wr lock & ~rAbort
        when(lkItem.lkType && ~rAbort(curTxnId)) {
          cntRlseReqWrRmt(curTxnId) := cntRlseReqWrRmt(curTxnId) + 1
          goto(RMT_WR)
        } otherwise(goto(CS_TXN))
      }
    }

    RMT_WR.whenIsActive {
      io.wrRmt.valid := True
      io.wrRmt.payload.setAll()

      when(io.wrRmt.fire) {
        nBeat := nBeat + 1
        when(nBeat === (U(1)<<lkItem.wLen) -1) {
          nBeat.clearAll()
          goto(CS_TXN)
        }
      }
    }

  }

  /**
  * component6: loadTxnMem
  *
  * */

  val compLoadTxn = new StateMachine {
    val IDLE = new State with EntryPoint
    val CS_TXN, RD_CMDAXI, LD_TXN = new State

    val curTxnId = Reg(UInt(conf.wTxnId bits)).init(0)
    val cntTxn = Reg(UInt(32 bits)).init(0)
    val txnOffs = curTxnId << conf.wMaxTxnLen

    io.cmdAxi.ar.addr := (((cntTxn << conf.wMaxTxnLen) << log2Up(8)) + (io.cmdAddrOffs<<6)).resized // each txn takes 8 Bytes
    io.cmdAxi.ar.id := 0
    // each 512 b contains 8 txn word (64 b / word)
    io.cmdAxi.ar.len := ((U(1)<<(conf.wMaxTxnLen-3)) -1).resized
    io.cmdAxi.ar.size := log2Up(512/8)
    io.cmdAxi.ar.setBurstINCR()


    val rCmdAxiData = RegNextWhen(io.cmdAxi.r.data, io.cmdAxi.r.fire)
    val rCmdAxiFire = RegNext(io.cmdAxi.r.fire)
    // 512 / 64
    val cmdAxiDataSlice = rCmdAxiData.subdivideIn(8 slices)

    val rTxnMemLd = RegInit(False)
    val cntTxnWordInLine = Counter(8, rTxnMemLd)
    val cntTxnWord = Counter(conf.wMaxTxnLen bits, rTxnMemLd)

    // IDLE: wait start signal
    IDLE.whenIsActive {
      when(io.start) {
        // reset regs
        curTxnId.clearAll()
        cntTxn.clearAll()
        goto(CS_TXN)
      }
    }

    //
    CS_TXN.whenIsActive {
      //NOTE: use rlseDone as flag of empty txn slot
      when(rRlseDone(curTxnId)) {
        goto(RD_CMDAXI)
      } otherwise {
        curTxnId := curTxnId + 1
      }
    }

    // rd on cmdAxi
    RD_CMDAXI.whenIsActive {
      io.cmdAxi.ar.valid := True
      when(io.cmdAxi.ar.fire) {
        rTxnMemLd.clear()
        cntTxnWordInLine.clear()
        cntTxnWord.clear()
        goto(LD_TXN)
      }
    }

    // load txnMem
    LD_TXN.whenIsActive {

      io.cmdAxi.r.ready := (cntTxnWordInLine === 0 && ~rCmdAxiFire) ? True | False

      when(io.cmdAxi.r.fire) (rTxnMemLd.set())

      val txnSt = TxnEntry(conf)
      txnSt.assignFromBits(cmdAxiDataSlice(cntTxnWordInLine))
      txnMem.write(txnOffs+cntTxnWord, txnSt, rTxnMemLd)

      when(cntTxnWordInLine.willOverflow) (rTxnMemLd.clear())
      when(cntTxnWord.willOverflow) {
        // clear all cnt register
        val zero = UInt(conf.wMaxTxnLen bits).default(0)
        for (e <- List(cntLkReqLoc, cntLkReqRmt, cntLkRespLoc, cntLkRespRmt, cntLkHoldLoc, cntLkHoldRmt, cntLkReqWrLoc, cntLkReqWrRmt, cntLkHoldWrLoc, cntLkHoldWrRmt, cntCmtReqLoc, cntCmtReqRmt, cntCmtRespLoc, cntCmtRespRmt, cntRlseReqLoc, cntRlseReqRmt, cntRlseReqWrLoc, cntRlseReqWrRmt, cntRlseRespLoc, cntRlseRespRmt))
          e(curTxnId) := zero // why the clearAll() DOES NOT work?
        for (e <- List(rReqDone, rAbort, rRlseDone))
          e(curTxnId).clear()

        io.cntTxnLd := io.cntTxnLd + 1
        cntTxn := cntTxn + 1

        when(cntTxn === (io.txnNumTotal-1))(goto(IDLE)) otherwise(goto(CS_TXN))
      }  // load one txn finished
    }
  }

  // io.done: all txn rlseDone; all txn loaded; set done only once
  when(rRlseDone.andR && io.cntTxnLd===io.txnNumTotal && ~io.done)(io.done.set())

  // io.cntClk
  val clkCnt = new StateMachine {
    val IDLE = new State with EntryPoint
    val CNT = new State
    IDLE.whenIsActive{
      when(io.start){
        io.cntClk.clearAll()
        goto(CNT)
      }
    }

    CNT.whenIsActive {
      io.cntClk := io.cntClk + 1
      when(io.done)(goto(IDLE))
    }
  }


}

object TxnManCSMain {
  def main(args: Array[String]): Unit = {

    val sysConf = SysConfig(1, 1, 1024, 1, 1)

    val axiConf = Axi4Config(
      addressWidth = 64,
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

    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = LOW), targetDirectory = "rtl").generateVerilog{
      val top = new TxnManCS(sysConf, axiConf)
      top.renameIO()
      top.setDefinitionName("txnman_cs")
      top
    }
  }
}

