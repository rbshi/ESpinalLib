package tmcs

import spinal.core.{UInt, _}
import spinal.core.Mem
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm.StateMachine
import util._

import scala.language.postfixOps

/*
* Number of node, channel, lock of each channel; txnMan on each node
* */
case class SysConfig(nNode: Int, nCh: Int, nLock:Int, nTxnMan: Int){
  val wNId = log2Up(nNode)
  val wCId = log2Up(nCh)
  val wLId = log2Up(nLock)
  val wTxnManId = log2Up(nTxnMan)

  // txnMan
  val nTxnCS = 64 // concurrent txn count, limited by axi arid (6 bits)
  val maxTxnLen = 64 // max len of each txn, space of on-chip mem
  val wMaxTxnLen = log2Up(maxTxnLen)
  val wLkIdx = log2Up(maxTxnLen) // lkIdx in one Txn, for OoO response
  val wTxnId = log2Up(nTxnCS)

  val dTxnMem = nTxnCS * maxTxnLen
  val wTxnMemAddr = log2Up(dTxnMem)

  // lkTable
  val wOwnerCnt = 4
  val wHtValNW = 1 + wOwnerCnt
  val wHtBucket = 6
  val wHtTable = 9

  val wChSize = 28 // 256MB of each channel (used as offset for global addressing)
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

  // txnMan config
  val nId = in UInt(conf.wNId bits)
  val txnManId = in UInt(conf.wTxnManId bits)

  def setDefault() = {

    for (e <- List(lkReqLoc, lkReqRmt, lkRespLoc, lkRespRmt, rdRmt, wrRmt))
      setDefStream(e)

    // axi
    axi.ar.valid.clear()
    axi.aw.valid.clear()
    axi.w.valid.clear()

    if(axiConf.useStrb)
      axi.w.strb.setAll()

  }
}

case class TxnEntry(conf: SysConfig) extends Bundle {
  val nId = UInt(conf.wNId bits)
  val cId = UInt(conf.wCId bits)
  val lId = UInt(conf.wLId bits)
  val lkType = Bits(2 bits)
  val wLen = UInt(12 bits) // len(tuple)=2^wLen

  def toLkReq(txnManId: UInt, curTxnId: UInt, release: Bool, lkIdx: UInt): LkReq = {
    val ret = LkReq(conf)
    ret.nId := this.nId
    ret.cId := this.cId
    ret.lId := this.lId
    ret.txnManId := txnManId
    ret.txnId:= curTxnId
    ret.lkType := this.lkType(0)
    ret.lkUpgrade := this.lkType(1)
    ret.lkRelease := False
    ret.lkIdx := lkIdx
    ret
  }
}

/*
* FIXME: now the txns are initialized in the mem
* FIXME: each channel may contain multiple tables, the lId to address translation logic will be fixed
* Txn entry: node_id, channel_id, lock_id, lock_type, data length (2^n B), (data is omitted here, included in the txn logic)
* */

class TxnManCS(conf: SysConfig, axiConf: Axi4Config) extends Component with RenameIO {

  val io = TxnManCSIO(conf, axiConf)
  io.setDefault()

  // lkGet and lkRlse are be arbitrated and sent to io
  val lkReqGetLoc, lkReqRlseLoc, lkReqGetRmt, lkReqRlseRmt = master Stream LkReq(conf)
  io.lkReqLoc := StreamArbiterFactory.roundRobin.onArgs(lkReqGetLoc, lkReqRlseLoc)
  io.lkReqRmt := StreamArbiterFactory.roundRobin.onArgs(lkReqGetRmt, lkReqRlseRmt)

  // todo: init in tb
  val txnMem = Mem(TxnEntry(conf), conf.dTxnMem)

  // store wr items to commit
  val txnWrMemLoc = Mem(TxnEntry(conf), conf.dTxnMem)
  val txnWrMemRmt = Mem(TxnEntry(conf), conf.dTxnMem)
  // store the obtained lock items to release
  val lkMemLoc = Mem(LkResp(conf), conf.dTxnMem)
  val lkMemRmt = Mem(LkResp(conf), conf.dTxnMem)

  // reg array
  // NOTE: separate local / remote
  val cntLkReqLoc, cntLkReqRmt, cntLkRespLoc, cntLkRespRmt, cntLkHoldLoc, cntLkHoldRmt, cntLkReqWrLoc, cntLkReqWrRmt, cntLkHoldWrLoc, cntLkHoldWrRmt, cntCmtReqLoc, cntCmtReqRmt, cntCmtRespLoc, cntCmtRespRmt, cntRlseReqLoc, cntRlseReqRmt, cntRlseReqWrLoc, cntRlseReqWrRmt, cntRlseRespLoc, cntRlseRespRmt = Vec(Reg(UInt(conf.wMaxTxnLen bits)), conf.nTxnCS)
  // status register
  val rReqDone, rAbort, rRlseDone = Vec(RegInit(False), conf.nTxnCS)

  // meta signals
  val txnEntryInvalid = TxnEntry(conf)
  txnEntryInvalid.assignFromBits(0)


  /*
   * component1: lock request
   */
  val compLkReq = new StateMachine {
    val RD_HEADER = new State with EntryPoint
    val RD_TXN = new State

    val curTxnId = Reg(UInt(log2Up(conf.wTxnId) bits)).init(0)

    val txnMemRdCmd = Stream(UInt(conf.wTxnMemAddr bits))
    txnMemRdCmd.valid := False
    txnMemRdCmd.payload := 0
    val txnMemRd = txnMem.streamReadSync(txnMemRdCmd)
    txnMemRd.ready := False

    val txnLen, reqLen = Reg(UInt(conf.wMaxTxnLen bits)).init(0)
    val txnHdAddr = curTxnId << conf.wMaxTxnLen

    for (e <- List(lkReqGetLoc, lkReqGetRmt))
      e.payload := txnMemRd.toLkReq(io.txnManId, curTxnId, False, reqLen)

    RD_HEADER.whenIsActive {
      // read the txn header
      txnMemRdCmd.valid := True
      txnMemRdCmd.payload := txnHdAddr
      txnMemRd.ready := True

      when(txnMemRd.fire){
        // txn is invalid (all reqs have been sent / abort)
        when(rReqDone(curTxnId) || rAbort(curTxnId)){
          curTxnId := curTxnId + 1
        } otherwise {
          txnLen := txnMemRd.asBits(conf.wMaxTxnLen-1 downto 0).asUInt
          txnMemRdCmd.valid := False
        }
      }
    }

    val lkReqFire = lkReqGetLoc.fire || lkReqGetRmt.fire
    val isLocal = txnMemRd.nId === io.nId

    RD_TXN.whenIsActive {

      txnMemRdCmd.valid := True
      txnMemRd.ready := lkReqFire

      when(lkReqFire){
        txnMemRdCmd.payload := txnHdAddr + reqLen + 2
        reqLen := reqLen + 1

        switch(isLocal) {
          is(True)(cntLkReqLoc(curTxnId) := cntLkReqLoc(curTxnId) + 1)
          is(False) (cntLkReqRmt(curTxnId) := cntLkReqRmt(curTxnId) + 1)
        }
        when(txnMemRd.lkType(0)){
          switch(isLocal) {
            is(True){
              txnWrMemLoc.write(cntLkReqWrLoc(curTxnId), txnMemRd.payload)
              cntLkReqWrLoc(curTxnId) := cntLkReqWrLoc(curTxnId) + 1
            }
            is(False){
              txnWrMemRmt.write(cntLkReqWrRmt(curTxnId), txnMemRd.payload)
              cntLkReqWrRmt(curTxnId) := cntLkReqWrRmt(curTxnId) + 1
            }
          }
        }
      } otherwise{
        txnMemRdCmd.payload := txnHdAddr + reqLen + 1 // skip the txnHD
      }

      // get the data and issue lkReq
      switch(isLocal) {
        is(True)(lkReqGetLoc.valid := txnMemRd.valid)
        is(False)(lkReqGetRmt.valid := txnMemRd.valid)
      }

      // lkReq of next Txn
      when(lkReqFire && (reqLen===(txnLen-1))){
        txnMemRdCmd.valid := False
        // write lsb of txnHd
        rReqDone(curTxnId).set()
        curTxnId := curTxnId + 1

        goto(RD_HEADER)
      }
    }
  }


  /*
  * component2: lock response
  *
  * */
  val compLkRespLoc = new StateMachine {

    val WAIT_RESP = new State with EntryPoint
    val LOCAL_RD_REQ = new State

    val rLkResp = RegNextWhen(io.lkRespLoc, io.lkRespLoc.fire)
    val curTxnId = io.lkRespLoc.txnId

    val rCurTxnId = RegNextWhen(curTxnId, io.lkRespLoc.fire)
    val getAllRlse = (cntRlseRespLoc(rCurTxnId) === cntLkHoldLoc(rCurTxnId)) && (cntRlseRespRmt(rCurTxnId) === cntLkHoldRmt(rCurTxnId))
    when(getAllRlse)(rRlseDone(rCurTxnId).set())

    WAIT_RESP.whenIsActive {
      io.lkRespLoc.ready := True

      when(io.lkRespLoc.fire) {

        when(io.lkRespLoc.respType === LockRespType.grant) {
          // note: ooo arrive
          lkMemLoc.write(cntLkRespLoc(curTxnId), io.lkRespLoc)

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
          // TODO: timing
          when(cntRlseRespLoc(curTxnId) === cntLkHoldLoc(curTxnId) - 1){
            rRlseDone(curTxnId) := True
          }
        }
      }
    }

    // TODO: data path
    // FIXME: lId -> addr translation logic
    io.axi.ar.addr := (rLkResp.lId << rLkResp.wLen) + (rLkResp.cId << conf.wChSize)
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

    val rCurTxnId = RegNextWhen(curTxnId, io.lkRespRmt.fire)
    val getAllRlse = (cntRlseRespLoc(rCurTxnId) === cntLkHoldLoc(rCurTxnId)) && (cntRlseRespRmt(rCurTxnId) === cntLkHoldRmt(rCurTxnId))
    when(getAllRlse)(rRlseDone(rCurTxnId).set())


    WAIT_RESP.whenIsActive {
      io.lkRespRmt.ready := True

      when(io.lkRespRmt.fire) {

        when(io.lkRespRmt.respType === LockRespType.grant) {
          // note: ooo arrive
          lkMemRmt.write(cntLkRespRmt(curTxnId), io.lkRespRmt)

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


  /*
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


  /*
  * component4: txnCommit
  * */

  val compTxnCmtLoc = new StateMachine {

    val CS_TXN = new State with EntryPoint
    val LOCAL_AW, LOCAL_W = new State

    val curTxnId = Reg(UInt(log2Up(conf.wTxnId) bits)).init(0)
    val txnOffs = curTxnId << conf.wMaxTxnLen

    val cmtTxn = txnWrMemLoc.readSync(txnOffs+cntCmtReqLoc(curTxnId))
    val rCmtTxn = RegNext(cmtTxn)

    val nBeat = Reg(UInt(8 bits)).init(0)

    val getAllLkResp = (cntLkReqLoc(curTxnId) === cntLkRespLoc(curTxnId)) && (cntLkReqRmt(curTxnId) === cntLkRespRmt(curTxnId))

    CS_TXN.whenIsActive {
      /*
      * 1. get All lk resp
      * 2. sent out all lk req
      * 3. no abort
      * 4. send #cmtReq === #LkHoldWr local
      * */
      val cmtCret = getAllLkResp && rReqDone(curTxnId) && ~rAbort(curTxnId) && (cntCmtReqLoc(curTxnId) < cntLkHoldWrLoc(curTxnId))
      when(cmtCret) {
        goto(LOCAL_AW)
      } otherwise {
        curTxnId := curTxnId + 1
      }
    }

    // TODO: data path
    // fixme: lId -> addr translation logic
    io.axi.aw.addr := (rCmtTxn.lId << rCmtTxn.wLen) + (rCmtTxn.cId << conf.wChSize)
    io.axi.aw.id := curTxnId
    io.axi.aw.len := (U(1)<<rCmtTxn.wLen) -1
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


  /*
  * component5: lkRelease
  *
  * */

  val compLkRlseLoc = new StateMachine {

    val CS_TXN = new State with EntryPoint
    val LK_RLSE = new State

    val curTxnId = Reg(UInt(log2Up(conf.wTxnId) bits)).init(0)
    val txnOffs = curTxnId << conf.wMaxTxnLen
    val lkItem = lkMemLoc.readSync(txnOffs+cntRlseReqLoc(curTxnId))
    val getAllLkResp = (cntLkReqLoc(curTxnId) === cntLkRespLoc(curTxnId)) && (cntLkReqRmt(curTxnId) === cntLkRespRmt(curTxnId))

    CS_TXN.whenIsActive {

      /*
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

    LK_RLSE.whenIsActive {
      lkReqRlseLoc.payload := lkItem.toLkReq(True, cntRlseReqLoc(curTxnId))
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

    val curTxnId = Reg(UInt(log2Up(conf.wTxnId) bits)).init(0)
    val nBeat = Reg(UInt(8 bits)).init(0)

    val txnOffs = curTxnId << conf.wMaxTxnLen
    val lkItem = lkMemRmt.readSync(txnOffs+cntRlseReqRmt(curTxnId))
    val getAllLkResp = (cntLkReqRmt(curTxnId) === cntLkRespRmt(curTxnId)) && (cntLkReqRmt(curTxnId) === cntLkRespRmt(curTxnId))

    CS_TXN.whenIsActive {

      /*
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

    RMT_LK_RLSE.whenIsActive {
      lkReqRlseRmt.payload := lkItem.toLkReq(True, cntRlseReqRmt(curTxnId))
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

}

object TxnManCSMain {
  def main(args: Array[String]): Unit = {

    val sysConf = SysConfig(1, 1, 1024, 1)

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

