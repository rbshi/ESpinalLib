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

class OpTopTest extends AnyFunSuite with SimFunSuite {

  def opReq2BigInt(addr: Int, data: BigInt, mode: BigInt, upgrade: BigInt): BigInt = {
    addr + (data << 64) + (mode << (64 + 64)) + (upgrade << (64 + 64 + 1))
  }

  def multi_op(dut: OpTop): Unit = {
    val numPE = 4
    val numLT = 16
    val txnCnt = 20
    val txnLen = 8

    // default
    dut.io.s_axi_control.ar.valid #= false
    dut.io.s_axi_control.aw.valid #= false
    dut.io.s_axi_control.w.valid #= false

    dut.clockDomain.forkStimulus(period = 10)

    val m_axi_mem = AxiMemorySim(dut.io.m_axi, dut.clockDomain, AxiMemorySimConfig(
      maxOutstandingReads = 128,
      maxOutstandingWrites = 128,
      readResponseDelay = 3,
      writeResponseDelay = 3
    ))
    m_axi_mem.start()
    // init data in axi mem
    val mem_init = Array.fill[Byte](1<<16)(0.toByte)
    m_axi_mem.memory.writeArray(0, mem_init)

    val req_axi_mem = AxiMemorySim(dut.io.req_axi, dut.clockDomain, AxiMemorySimConfig(
      maxOutstandingReads = 128,
      maxOutstandingWrites = 128,
      readResponseDelay = 3,
      writeResponseDelay = 3
    ))
    req_axi_mem.start()

    val req_mem_init = Array.fill[Byte](1<<16)(0.toByte)

    // init txn array, each element in array is a txn with multi r/w operations
    var arrayTxn = new ArrayBuffer[mutable.ListBuffer[BigInt]]()
    for (j <- 0 until numPE) {
      for (i <- 0 until txnCnt) {
        var reqQueue = new mutable.ListBuffer[BigInt]()
        for (k <- 0 until txnLen) {
          reqQueue += opReq2BigInt(k+i*txnLen, k+i*txnLen+j*txnLen*txnCnt, 1, 0) // write req
//          reqQueue += opReq2BigInt(k+i*txnLen+j*txnLen*txnCnt, k+i*txnLen+j*txnLen*txnCnt, 1, 0) // write req
        }
        arrayTxn += reqQueue
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
    // init to req memory
    req_axi_mem.memory.writeArray(0, req_mem_init)

    // wait the fifo (empty_ptr) to reset
    dut.clockDomain.waitSampling(2000)

    // start
    setAxi4LiteReg(dut, dut.io.s_axi_control, 0x10, txnLen) // txnLen
    setAxi4LiteReg(dut, dut.io.s_axi_control, 0x14, txnCnt) // txnCnt

    for (i <- 0 until numPE) {
      setAxi4LiteReg(dut, dut.io.s_axi_control, 0x18 + 4 * i, i * txnCnt * txnLen * 64) // addrOffset
    }



    setAxi4LiteReg(dut, dut.io.s_axi_control, 0x00, 1) // ap_start

    dut.clockDomain.waitSampling(20000)

    for (i <- 0 until numPE) {
      println(s"Reg[$i] = ${readAxi4LiteReg(dut, dut.io.s_axi_control, 0x28+i*4)}")
    }

    for (i <- 0 until numPE) {
      println(s"Reg[$i] = ${readAxi4LiteReg(dut, dut.io.s_axi_control, 0x38+i*4)}")
    }

    println(s"Reg[clkCnt] = ${readAxi4LiteReg(dut, dut.io.s_axi_control, 0x48)}")

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
      val dut = new OpTop(4, 16)
      dut
    }.doSim("multi_op", 99)(multi_op)
  }

}