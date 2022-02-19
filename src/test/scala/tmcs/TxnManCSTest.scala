package tmcs

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config}
import util.{AxiMemorySim, AxiMemorySimConfig}

import scala.collection._
import scala.util.Random
import scala.math._
import util._
import spinal.lib.{master, slave}

import scala.collection._
import scala.collection.mutable.ArrayBuffer
import scala.math.BigInt
import util._


/*
* connect the txnManCS to a lt
* */
class TxnManCSTop(sysConf: SysConfig) extends Component with SetDefaultIO {

  val txnMan = new TxnManCS(sysConf)
  val ltCh = new LtCh(sysConf)

  val io = new Bundle {
    val axi = master(Axi4(sysConf.axiConf))
    val cmdAxi = master(Axi4(sysConf.axiConf))
    val start = in Bool()
    val txnNumTotal = in UInt(32 bits)
    val cmdAddrOffs = in UInt(32 bits) //NOTE: unit size 64B
  }

  io.start <> txnMan.io.start
  io.txnNumTotal <> txnMan.io.txnNumTotal
  io.cmdAddrOffs <> txnMan.io.cmdAddrOffs

  io.axi <> txnMan.io.axi
  io.cmdAxi <> txnMan.io.cmdAxi
  txnMan.io.lkReqLoc <> ltCh.io.lkReq
  txnMan.io.lkRespLoc <> ltCh.io.lkResp
  txnMan.io.nId := 0
  txnMan.io.txnManId := 0

  for (e <- List(txnMan.io.lkReqRmt, txnMan.io.wrRmt))
    setDefStream(e, true)
  for (e <- List(txnMan.io.lkRespRmt, txnMan.io.rdRmt))
    setDefStream(e, false)

}


class TxnManCSTest extends AnyFunSuite with SimFunSuite {

  val sysConf = new SysConfig {
    override val nNode: Int = 1
    override val nCh: Int = 1
    override val nLock: Int = 1024
    override val nTxnMan: Int = 1
    override val nLtPart: Int = 4
  }

  def txnEntry2BigInt(nId: Int, cId: Int, tId: Int, lkType: Int, wLen: Int): BigInt = {
    nId + (cId << (sysConf.wNId)) + (tId << (sysConf.wNId+sysConf.wCId)) + (lkType << (sysConf.wNId+sysConf.wCId+sysConf.wTId)) + (wLen << (sysConf.wNId+sysConf.wCId+sysConf.wTId+2))
  }

  def initTxnMem(sysConf: SysConfig, txnCnt: Int, txnLen: Int, txnMaxLen: Int) = {
    var txnMem = mutable.Queue.empty[BigInt]
    for (i <- 0 until txnCnt) {
      // txnHd
      txnMem += txnLen
      for (j <- 0 until txnLen)
        txnMem += txnEntry2BigInt(0, 0, j+i, 1, 0) // len=64B << 0
        // txnMem += txnEntry2BigInt(0, 0, txnLen*(i%sysConf.nTxnCS)/2+j, 1, 0) // conflict between neighboring txn
        // txnMem += txnEntry2BigInt(0, 0, txnLen*(i%sysConf.nTxnCS)+j, 1, 0)
        // txnMem += txnEntry2BigInt(0, 0, txnLen*i+j, 0, 0) // len=64B << 0
      for (j <- 0 until (txnMaxLen-txnLen))
        txnMem += 0
    }
    txnMem
  }



  def txnManCS(dut: TxnManCSTop): Unit = {

    // params
    val txnLen = 16
    val txnCnt = 128
    val txnMaxLen = sysConf.maxTxnLen-1

    dut.clockDomain.forkStimulus(period = 10)

    // data memory
    val axi_mem = AxiMemorySim(dut.io.axi, dut.clockDomain, AxiMemorySimConfig(
      maxOutstandingReads = 128,
      maxOutstandingWrites = 128,
      readResponseDelay = 3,
      writeResponseDelay = 2
    ))
    axi_mem.start()
    val mem_init = Array.fill[Byte](1<<20)(0.toByte)
    axi_mem.memory.writeArray(0, mem_init)

    // cmd memory
    val cmd_axi_mem = AxiMemorySim(dut.io.cmdAxi, dut.clockDomain, AxiMemorySimConfig(
      maxOutstandingReads = 128,
      maxOutstandingWrites = 128,
      readResponseDelay = 3,
      writeResponseDelay = 3
    ))
    cmd_axi_mem.start()

    val txnMem = initTxnMem(sysConf, txnCnt, txnLen, txnMaxLen)
    var cmd_axi_mem_init = new Array[Byte](txnCnt*(txnMaxLen+1)*8)
    for (i <- 0 until txnCnt*(txnMaxLen+1)) {
      // pad zero byte to MSBs
      val tmp = txnMem(i).toByteArray.reverse.padTo(8, 0.toByte)
      Array.copy(tmp, 0, cmd_axi_mem_init, i*8, 8)
    }
    // init to cmd memory
    cmd_axi_mem.memory.writeArray(0, cmd_axi_mem_init)

    dut.io.start #= false
    // wait the fifo (empty_ptr) to reset
    dut.clockDomain.waitSampling(2000)

    // config
    dut.io.cmdAddrOffs #= 0
    dut.io.txnNumTotal #= txnCnt

    // start
    dut.io.start #= true
    dut.clockDomain.waitSampling()
    dut.io.start #= false


    // wait for a while
//    dut.clockDomain.waitSampling(64000)

    dut.clockDomain.waitSamplingWhere(dut.txnMan.io.done.toBoolean)
    println(s"[txnMan] cntTxnCmt: ${dut.txnMan.io.cntTxnCmt.toBigInt}")
    println(s"[txnMan] cntTxnAbt: ${dut.txnMan.io.cntTxnAbt.toBigInt}")
    println(s"[txnMan] cntTxnLd: ${dut.txnMan.io.cntTxnLd.toBigInt}")
    println(s"[txnMan] cntClk: ${dut.txnMan.io.cntClk.toBigInt}")

  }


  test("txnman_cs") {
    SimConfig.withWave.compile {
      val dut = new TxnManCSTop(sysConf)
      dut.txnMan.io.simPublic()
      dut
    }.doSim("txnman_cs", 99)(txnManCS)
  }

}