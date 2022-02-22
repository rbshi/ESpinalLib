package tmcs
import spinal.core.sim._
import scodec._
import scodec.bits._
import scodec.codecs._

import scala.collection.mutable.ArrayBuffer

trait MemStructSim {
  def asBytes : Seq[Byte]
}

trait BitSerializable[T <: BitSerializable[T]] {
  self : T =>
  val codec : Codec[T]
  def serialize : BitVector = codec.encode(self).require
}

case class TxnEntrySim(
                        nId: Int,
                        cId: Int,
                        tId: Int,
                        lkAttr: Int,
                        wLen: Int
                      ) extends MemStructSim {
  override def asBytes = SimConversions.txnEntrySimToBytes(this, 8)
}

object SimConversions {

  var sysConf = new SysConfig {
    override val nNode: Int = 1
    override val nCh: Int = 1
    override val nLock: Int = 1024
    override val nTxnMan: Int = 1
    override val nLtPart: Int = 1
  }

  implicit def txnEntrySimToBytes(req: TxnEntrySim, byteLen: Int) : Seq[Byte] = {
    val vBigInt = req.nId +
        (req.cId << (sysConf.wNId)) +
        (req.tId << (sysConf.wNId+sysConf.wCId)) +
        (req.lkAttr << (sysConf.wNId+sysConf.wCId+sysConf.wTId)) +
        (req.wLen << (sysConf.wNId+sysConf.wCId+sysConf.wTId+2))
    bigIntToBytes(vBigInt, byteLen)
  }

  implicit def bigIntToBytes(v: BigInt, byteLen: Int) : Seq[Byte] = {
    v.toByteArray.reverse.padTo(byteLen, 0.toByte)
  }

}

object SimInit {
  var sysConf = new SysConfig {
    override val nNode: Int = 1
    override val nCh: Int = 1
    override val nLock: Int = 1024
    override val nTxnMan: Int = 1
    override val nLtPart: Int = 1
  }

  implicit def txnEntrySimInt(txnCnt: Int, txnLen: Int, txnMaxLen: Int): Seq[Byte] = {
    var txnMem = Seq.empty[Byte]
    for (i <- 0 until txnCnt) {
      // txnHd
      txnMem = txnMem ++ SimConversions.bigIntToBytes(BigInt(txnLen), 8)
      for (j <- 0 until txnLen)
//        txnMem = txnMem ++ TxnEntrySim(0, j%sysConf.nCh, j+i, 0, 0).asBytes
        txnMem = txnMem ++ TxnEntrySim(1, 0, j+i, 1, 0).asBytes
      for (j <- 0 until (txnMaxLen-txnLen))
        txnMem = txnMem ++ SimConversions.bigIntToBytes(BigInt(0), 8)
    }
    txnMem
  }

}