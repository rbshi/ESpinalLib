package tmcs

import spinal.core._
import spinal.lib.bus.amba4.axi._
import spinal.lib._

import util._

trait SysConfig {

  // sys params
  val nNode : Int
  val nCh : Int
  val nLock : Int
  val nTxnMan : Int

  // derivative params
  def wNId = log2Up(nNode)
  def wCId = log2Up(nCh)
  def wTId = log2Up(nLock)
  def wTxnManId = log2Up(nTxnMan)

  // txnMan params
  val nTxnCS = 64 // concurrent txn count, limited by axi arid (6 bits)
  val maxTxnLen = 64 // max len of each txn, space of on-chip mem (include the txnHd)

  def wMaxTxnLen = log2Up(maxTxnLen)
  def wLkIdx = log2Up(maxTxnLen) // lkIdx in one Txn, for OoO response
  def wTxnId = log2Up(nTxnCS)

  def dTxnMem = nTxnCS * maxTxnLen
  def wTxnMemAddr = log2Up(dTxnMem)

  // LT params
  val nLtPart : Int

  val wOwnerCnt = 4
  def wHtValNW = 1 + wOwnerCnt
  val wHtBucket = 6
  def wHtTable = log2Up(nLock)
  def wLtPart = log2Up(nLtPart)

  val wChSize = 28 // 256MB of each channel (used as offset with global addressing)

  val wLkAttr = 2
  val wTupLenPow = 3 //len(tuple)=2^wLen; maxLen = 64B << 7 = 8192 B

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

  def printConf: Unit = {
    println(
      s"""
         |Config
         |=======================
         |wTId = $wTId
         |
         |""".stripMargin)
  }

}

case class TxnEntry(conf: SysConfig) extends Bundle {
  val nId = UInt(conf.wNId bits)
  val cId = UInt(conf.wCId bits)
  val tId = UInt(conf.wTId bits)
  val lkAttr = Bits(conf.wLkAttr bits)
  val wLen = UInt(conf.wTupLenPow bits) // len(tuple)=2^wLen; maxLen = 64B << 7 = 8192 B

  def toLkReq(txnManId: UInt, curTxnId: UInt, release: Bool, lkIdx: UInt): LkReq = {
    val lkReq = LkReq(this.conf, false) // process in txnMan, so false
    lkReq.assignSomeByName(this)
    lkReq.txnManId := txnManId
    lkReq.txnId := curTxnId
    lkReq.lkType := this.lkAttr(0)
    lkReq.lkUpgrade := this.lkAttr(1)
    lkReq.lkRelease := release
    lkReq.lkIdx := lkIdx
    lkReq
  }
}

case class LkReq(conf: SysConfig, isTIdTrunc: Boolean) extends Bundle {
  val nId = UInt(conf.wNId bits)
  val cId = UInt(conf.wCId bits)
  val tId = if(isTIdTrunc) UInt(conf.wTId - conf.wLtPart bits) else UInt(conf.wTId bits)
  val txnManId = UInt(conf.wTxnManId bits)
  val txnId = UInt(conf.wTxnId bits)
  val lkType = Bool()
  val lkUpgrade = Bool()
  val lkRelease = Bool()
  val lkIdx = UInt(conf.wLkIdx bits)
  val wLen = UInt(conf.wTupLenPow bits)
}

// TODO: now LkResp bypass all info in LkReq
case class LkResp(conf: SysConfig, isTIdTrunc: Boolean) extends Bundle {
  val nId = UInt(conf.wNId bits)
  val cId = UInt(conf.wCId bits)
  val tId = if(isTIdTrunc) UInt(conf.wTId - conf.wLtPart bits) else UInt(conf.wTId bits)
  val txnManId = UInt(conf.wTxnManId bits)
  val txnId = UInt(conf.wTxnId bits)
  val lkType = Bool()
  val lkUpgrade = Bool()
  val lkRelease = Bool()
  val lkIdx = UInt(conf.wLkIdx bits)
  val wLen = UInt(conf.wTupLenPow bits)
  // TODO: how to reuse the above W/O bundle hierarchy

  val respType = LockRespType()

  def toLkReq(release: Bool, lkIdx: UInt): LkReq = {
    val lkReq = LkReq(this.conf, false) // process in txnMan, so false
    lkReq.assignSomeByName(this)
    lkReq.lkRelease.allowOverride := release
    lkReq.lkIdx.allowOverride := lkIdx
    lkReq
  }

}

object LockTableIO {

  def apply(conf: SysConfig, isTIdTrunc: Boolean): LockTableIO = {
    val ret = new LockTableIO(conf, isTIdTrunc)
    ret
  }

  def apply(conf: SysConfig): LockTableIO = {
    val ret = new LockTableIO(conf, false)
    ret
  }
}


class LockTableIO(conf: SysConfig, isTIdTrunc: Boolean) extends Bundle{
  val lkReq = slave Stream(LkReq(conf, isTIdTrunc))
  val lkResp = master Stream(LkResp(conf, isTIdTrunc))
}
























