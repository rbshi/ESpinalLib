package tm

import spinal.core.{UInt, _}
import spinal.core.Mem
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm.StateMachine

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

case class TxnManCSIO(conf: SysConfig, axiConf: Axi4Config) extends Bundle {
  //
  val lkReq, lkReqRmt = master Stream LkReq(conf)

  val rdRmt = slave Stream UInt(512 bits)
  val wrRmt = master Stream UInt(512 bits)

  // arbitered from local / remote
  val lkResp = slave Stream LkResp(conf)

  // data axi
  val axi = master(Axi4(axiConf))

  // txnMan config
  val nId = in UInt(conf.wNId bits)
  val txnManId = in UInt(conf.wTxnManId bits)

  def setDefault() = {
    lt_req.valid := False
    //    lt_req.txn_id := 0
    lt_resp.ready := False

    axi.readCmd.size := log2Up(512 / 8)
    axi.readCmd.addr := 0
    axi.readCmd.id := 0
    axi.readCmd.valid := False
    axi.readCmd.len := 0
    //    axi.writeCmd.valid := False
    axi.writeCmd.size := log2Up(512 / 8) // 3'b110: 64Byte/line
    //    axi.writeCmd.addr := 0
    axi.writeCmd.id := 0
    axi.writeCmd.len := 0
    //    axi.readRsp.ready := True
    //    axi.writeRsp.ready := True
    //    axi.writeData.data := 0
    axi.writeData.last := True
    //    axi.writeData.valid := False

    if(axiConf.useBurst) {
      axi.ar.setBurstINCR()
      axi.aw.setBurstINCR()
    }
    if(axiConf.useStrb) {
      axi.w.strb.setAll()
    }

    sig_txn_end := False
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
* fixme: now the txns are initialized in the mem
* Txn cmd format: lock_id (includes node_id:channel_id:lock_id), lock_mode, table_offset, data length (2^n B), (data is omitted here, included in the txn logic)
* */

class TxnManCS(conf: SysConfig, axiConf: Axi4Config) extends Component {

  val io = TxnManCSIO(conf, axiConf)
  io.setDefault()

  // lkGet and lkRlse should be arbitered and sent to io
  val lkReqGet, lkReqRlse, lkReqRmtGet, lkReqRmtRlse = master Stream LkReq(conf)

  // todo: init in tb
  val txnMem = Mem(TxnEntry(conf), conf.dTxnMem)

  // store wr item for commit
  val txnWrMem = Mem(TxnEntry(conf), conf.dTxnMem)
  // store lock item for release
  val lkMem = Mem(LkResp(conf), conf.dTxnMem)

  // reg array
  // FIXME: cntRlseReq is only for mem index
  val cntLkReq, cntLkResp, cntLkHold, cntLkReqWr, cntLkHoldWr, cntCmtReq, cntCmtResp, cntRlseReq, cntRlseReqLoc, cntRlseReqRmt, cntRlseReqWrLoc, cntRlseReqWrRmt, cntRlseResp = Vec(Reg(UInt(conf.wMaxTxnLen bits)), conf.nTxnCS)
  val rAbort, r2Cmt, r2Rlse = Vec(RegInit(False), conf.nTxnCS)

  // meta signals
  val txnEntryInvalid = TxnEntry(conf)
  txnEntryInvalid.assignFromBits(0)


  /*
   * component1: lock request
   * req get lock
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

    for (e <- List(lkReqGet, lkReqRmtGet))
      e.payload := txnMemRd.toLkReq(io.txnManId, curTxnId, False, reqLen)

    RD_HEADER.whenIsActive{
      // read the txn header
      txnMemRdCmd.valid := True
      txnMemRdCmd.payload := txnHdAddr
      txnMemRd.ready := True

      when(txnMemRd.fire){
        // txn is invalid (all reqs have been sent / abort)
        when(~txnMemRd.asBits(0)){
          curTxnId := curTxnId + 1
        } otherwise {
          txnLen := txnMemRd.asBits(conf.wMaxTxnLen downto 1).asUInt
          txnMemRdCmd.valid := False
        }
      }
    }

    RD_TXN.whenIsActive{

      val lkReqFire = lkReqGet.fire || lkReqRmtGet.fire

      txnMemRdCmd.valid := True
      txnMemRd.ready := lkReqFire

      when(lkReqFire){
        txnMemRdCmd.payload := txnHdAddr + reqLen + 2
        reqLen := reqLen + 1

        cntLkReq(curTxnId) := cntLkReq(curTxnId) + 1
        when(txnMemRd.lkType(0)){
          txnWrMem.write(cntLkReqWr(curTxnId), txnMemRd.payload)
          cntLkReqWr(curTxnId) := cntLkReqWr(curTxnId) + 1
        }
      } otherwise{
        txnMemRdCmd.payload := txnHdAddr + reqLen + 1 // skip the txnHD
      }

      // get the data and issue lkReq
      when(txnMemRd.nId === io.nId)(lkReqGet.valid := txnMemRd.valid) otherwise(lkReqRmtGet.valid := txnMemRd.valid)

      // lkReq of next Txn
      when(lkReqFire && (reqLen===(txnLen-1))){
        txnMemRdCmd.valid := False
        // write lsb of txnHd
        txnMem.write(txnHdAddr, txnEntryInvalid)
        curTxnId := curTxnId + 1

        goto(RD_HEADER)
      }
    }
  }


  /*
  * component2: lock response
  *
  * */
  val compLkResp = new StateMachine {

    val WAIT_RESP = new State with EntryPoint
    val LOCAL_RD_REQ, RMT_RD_CONSUME = new State

    val rLkResp = RegNextWhen(io.lkResp, io.lkResp.fire)
    val nBeat = Reg(UInt(8 bits)).init(0)

    WAIT_RESP.whenIsActive {
      io.lkResp.ready := True

      when(io.lkResp.fire) {
        when(io.lkResp.respType === LockRespType.grant) {
          // note: ooo arrive
          lkMem.write(cntLkResp(io.lkResp.txnId), io.lkResp)

          cntLkResp(io.lkResp.txnId) := cntLkResp(io.lkResp.txnId) + 1
          cntLkHold(io.lkResp.txnId) := cntLkHold(io.lkResp.txnId) + 1

          // FIXME: should it be in a blocking way?
          when(io.lkResp.lkType){
            cntLkHoldWr(io.lkResp.txnId) := cntLkHoldWr(io.lkResp.txnId) + 1
          } otherwise {
            when(io.lkResp.nId === io.nId) (goto(LOCAL_RD_REQ)) otherwise(goto(RMT_RD_CONSUME))
          }
        }

        when(io.lkResp.respType === LockRespType.abort) {
          rAbort(io.lkResp.txnId) := True
          cntLkResp(io.lkResp.txnId) := cntLkResp(io.lkResp.txnId) + 1
        }

        when(io.lkResp.respType === LockRespType.release) {
          cntRlseResp(io.lkResp.txnId) := cntRlseResp(io.lkResp.txnId) + 1
        }
      }
    }

    LOCAL_RD_REQ.whenIsActive {
      // fixme: lId -> addr translation logic
      io.axi.ar.addr := (rLkResp.lId << rLkResp.wLen) + (rLkResp.cId << conf.wChSize)
      io.axi.ar.id := rLkResp.txnId
      io.axi.ar.len := (U(1)<<rLkResp.wLen) -1
      io.axi.ar.size := log2Up(512/8)
      io.axi.ar.setBurstINCR()
      io.axi.ar.valid := True
      when(io.axi.ar.fire)(goto(WAIT_RESP))
    }

    // REMOTE_RD data come with the lkResp
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
      cntCmtResp(io.axi.b.id) := cntCmtResp(io.axi.b.id) + 1
    }
  }


  /*
  * component4: txnCommit
  * */

  val compTxnCommit = new StateMachine {

    val CS_TXN = new State with EntryPoint
    val MUX_LOCAL_RMT, LOCAL_AW, LOCAL_W, RMT_LK_RLSE, RMT_WR = new State

    val curTxnId = Reg(UInt(log2Up(conf.wTxnId) bits)).init(0)
    val txnOffs = curTxnId << conf.wMaxTxnLen

    val cmtTxn = txnWrMem.readSync(txnOffs+cntCmtReq(curTxnId))
    val rCmtTxn = RegNext(cmtTxn)

    val nBeat = Reg(UInt(8 bits)).init(0)

    // switch to txn with r2Cmt
    CS_TXN.whenIsActive {
      // fixme: stage?
      val cmtCret = (cntLkReq(curTxnId) === cntLkResp(curTxnId)) && ~rAbort(curTxnId) && r2Cmt(curTxnId) && (cntCmtReq(curTxnId) < cntLkHoldWr(curTxnId))
      when(cmtCret) {
        goto(MUX_LOCAL_RMT)
      } otherwise {
        curTxnId := curTxnId + 1
      }
    }

    MUX_LOCAL_RMT.whenIsActive {
      when(cmtTxn.nId===io.nId)(goto(LOCAL_AW)) otherwise(goto(RMT_LK_RLSE))
    }

    LOCAL_AW.whenIsActive {
      // fixme: lId -> addr translation logic
      io.axi.aw.addr := (rCmtTxn.lId << rCmtTxn.wLen) + (rCmtTxn.cId << conf.wChSize)
      io.axi.aw.id := curTxnId
      io.axi.aw.len := (U(1)<<rCmtTxn.wLen) -1
      io.axi.aw.size := log2Up(512/8)
      io.axi.aw.setBurstINCR()
      io.axi.aw.valid := True
      when(io.axi.aw.fire){
        goto(LOCAL_W)
      }
    }

    LOCAL_W.whenIsActive {
      io.axi.w.valid := True
      io.axi.w.data.setAll()
      io.axi.w.last := (nBeat === (U(1)<<rCmtTxn.wLen) -1)
      when(io.axi.w.fire){
        nBeat := nBeat + 1
        when(io.axi.w.last) {
          cntCmtReq(curTxnId) := cntCmtReq(curTxnId) + 1
          nBeat.clearAll()
          goto(CS_TXN)
        }
      }
      // io.axi.b will be tackled in component3
    }

    // release the lock here, so that will not block the local lk release
    // fixme: avoid conflict with release phase
    RMT_LK_RLSE.whenIsActive {
      lkReqRmtRlse.valid := True
      lkReqRmtRlse.payload := rCmtTxn.toLkReq(io.txnManId, curTxnId, True, 0) // fixme: lkIdx -> 0
      when(lkReqRmtRlse.fire){
        cntRlseReqRmt(curTxnId) := cntRlseReqRmt(curTxnId) + 1
        cntRlseReqWrRmt(curTxnId) := cntRlseReqWrRmt(curTxnId) + 1
        goto(RMT_WR)
      }
    }

    RMT_WR.whenIsActive {
      io.wrRmt.valid := True
      io.wrRmt.payload.setAll()

      when(io.wrRmt.fire) {
        nBeat := nBeat + 1
        when(nBeat === (U(1)<<rCmtTxn.wLen) -1) {
          nBeat.clearAll()
          goto(CS_TXN)
        }
      }
    }
  }


  /*
  * component5: lkRelease
  *
  * */

  val compLkRlse = new StateMachine {

    val CS_TXN = new State with EntryPoint
    val LK_RLSE = new State

    val curTxnId = Reg(UInt(log2Up(conf.wTxnId) bits)).init(0)
    val txnOffs = curTxnId << conf.wMaxTxnLen

    val lkItem = lkMem.readSync(txnOffs+cntRlseReq(curTxnId))


    // switch to txn with r2Cmt
    CS_TXN.whenIsActive {
      val cntRlseReqWrTotal = cntRlseReqWrLoc(curTxnId)+cntRlseReqWrRmt(curTxnId)
      val cntRlseReqTotal = cntRlseReqLoc(curTxnId)+cntRlseReqRmt(curTxnId)
      val rlseCret = (cntLkReq(curTxnId) === cntLkResp(curTxnId)) && r2Rlse(curTxnId) && (~r2Cmt(curTxnId) || cntRlseReqWrLoc(curTxnId)<cntCmtResp(curTxnId) || (cntRlseReqWrTotal===cntLkReqWr(curTxnId))) && cntRlseReqTotal < cntLkHold(curTxnId)
      when(rlseCret) {
        goto(LK_RLSE)
      } otherwise {
        curTxnId := curTxnId + 1
      }
    }

    LK_RLSE.whenIsActive {
      lkReqRlse.payload := lkItem.toLkReq(True, cntRlseReq(curTxnId))
      when(lkItem.nId===io.nId){
        lkReqRlse.valid := True
        when(lkReqRlse.fire){
          cntRlseReq(curTxnId) := cntRlseReq(curTxnId) + 1
          goto(CS_TXN)
        }
      } otherwise {
        cntRlseReq(curTxnId) := cntRlseReq(curTxnId) + 1
        goto(CS_TXN)
      }

      when(lkReqRlse.fire && lkItem.lkType){
        cntRlseReqWrLoc(curTxnId) := cntRlseReqWrLoc(curTxnId) + 1
      }
    }

  }


}



































