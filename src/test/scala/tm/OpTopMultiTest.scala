package tm

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._
//import spinal.lib.bus.amba4.axi.sim._
import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.Stream
import spinal.sim.SimThread

import scala.collection._
import scala.collection.mutable.ArrayBuffer
import scala.math.BigInt

import util._

class OpTopMultiTest extends AnyFunSuite with SimFunSuite {

  /**
   * Transform each txn op to 512-bit entry that will be initialized to DRAM
   *
   * Bit organization: {upgrade(1):mode(1):data(64):addr(64)}
   */
  def opReq2BigInt(addr: Int, data: BigInt, mode: BigInt, upgrade: BigInt): BigInt = {
    addr + (data << 64) + (mode << (64 + 64)) + (upgrade << (64 + 64 + 1))
  }

  def multi_op(dut: OpTopMulti): Unit = {
    val numPE = dut.numTxnMan
    val numLT = dut.numLT
    val numCh = dut.numCh
    val txnCnt = 10
    val txnLen = 16

    // default
    dut.io.s_axi_control.ar.valid #= false
    dut.io.s_axi_control.aw.valid #= false
    dut.io.s_axi_control.w.valid #= false

    dut.clockDomain.forkStimulus(period = 10)

    // init txn instructions
    val req_mem_init = Array.fill[Byte](1<<18)(0.toByte)

    // init txn array, each element in array is a txn with multi r/w operations
    var arrayTxn = new ArrayBuffer[mutable.ListBuffer[BigInt]]()
    for (c <- 0 until numCh) {
      for (j <- 0 until numPE) {
        for (i <- 0 until txnCnt) {
          var reqQueue = new mutable.ListBuffer[BigInt]()
          for (k <- 0 until txnLen) {
            // contention in ch
            // reqQueue += opReq2BigInt(k+i*txnLen, k+i*txnLen+j*txnLen*txnCnt, 0, 0)
            // non-contention in ch
            reqQueue += opReq2BigInt(k + i * txnLen + j * txnLen * txnCnt, k + i * txnLen + j * txnLen * txnCnt, 0, 0)
          }
          arrayTxn += reqQueue
        }
      }
    }

    var req_mem_ptr = 0
    for (reqQueue <- arrayTxn) {
      for (req <- reqQueue) {
        val reqByte = req.toByteArray
        for (ii <- reqByte.indices) {
          req_mem_init(req_mem_ptr + ii) = reqByte(reqByte.length - 1 - ii) // toByteArray: the MSB is the 0th byte..
        }
        req_mem_ptr += 64 // 512-bit lane
      }
    }

    // AXI DRAM model
    for (iCh <- 0 until numCh) {

      val m_axi_mem = AxiMemorySim(dut.io.m_axi(iCh), dut.clockDomain, AxiMemorySimConfig(
        maxOutstandingReads = 128,
        maxOutstandingWrites = 128,
        readResponseDelay = 10,
        writeResponseDelay = 10
      ))
      m_axi_mem.start()

      // init data in axi mem, otherwise will show page fault
      val mem_init = Array.fill[Byte](1 << 18)(0.toByte)
      m_axi_mem.memory.writeArray(0, mem_init)

      val req_axi_mem = AxiMemorySim(dut.io.req_axi(iCh), dut.clockDomain, AxiMemorySimConfig(
        maxOutstandingReads = 128,
        maxOutstandingWrites = 128,
        readResponseDelay = 10,
        writeResponseDelay = 10
      ))
      req_axi_mem.start()

      // init to req memory
      req_axi_mem.memory.writeArray(0, req_mem_init)
    }


    // wait the fifo (empty_ptr) to reset
    dut.clockDomain.waitSampling(2000)

    // start
    setAxi4LiteReg(dut, dut.io.s_axi_control, 0x10, txnLen) // txnLen
    setAxi4LiteReg(dut, dut.io.s_axi_control, 0x14, txnCnt) // txnCnt

    for (i <- 0 until numPE * numCh) {
      setAxi4LiteReg(dut, dut.io.s_axi_control, 24 + 4 * i, i * txnCnt * txnLen * 64) // addrOffset
    }

    setAxi4LiteReg(dut, dut.io.s_axi_control, 0x00, 1) // ap_start

    dut.clockDomain.waitSampling(20000)


    for (i <- 0 until numPE*numCh) {
      println(s"txnExeCnt[$i] = ${readAxi4LiteReg(dut, dut.io.s_axi_control, 24+4*numPE*numCh+i*4)}")
    }

    for (i <- 0 until numPE*numCh) {
      println(s"txnAbortCnt[$i] = ${readAxi4LiteReg(dut, dut.io.s_axi_control, 24+8*numPE*numCh+i*4)}")
    }

    println(s"clkCnt[clkCnt] = ${readAxi4LiteReg(dut, dut.io.s_axi_control, 24+12*numPE*numCh)}")


    println(s"Ctrl reg= ${readAxi4LiteReg(dut, dut.io.s_axi_control, 0)}")
    dut.clockDomain.waitSampling(10)
    println(s"Ctrl reg= ${readAxi4LiteReg(dut, dut.io.s_axi_control, 0)}")


    //    for (j <- 0 until numPE) {
    //      for (i <- 0 until txnCnt) {
    //        for (k <- 0 until txnLen) {
    //          val idxTuple = k+i*txnLen+j*txnLen*txnCnt
    //          println(s"Tuple[$idxTuple]=${m_axi_mem.memory.readBigInt(idxTuple*64, 64)}")
    //        }
    //      }
    //    }

  }


  test("multi_op") {
    SimConfig.withWave.compile {
      val dut = new OpTopMulti(4, 16, 2)
      dut
    }.doSim("multi_op", 99)(multi_op)
  }

}