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

import esim._

class OpTopTest extends AnyFunSuite {

  def setAxi4LiteReg(dut: Component, bus: AxiLite4, addr: Int, data: Int): Unit ={
    val awa = fork {
      bus.aw.addr #= addr
      bus.w.data #= data
      bus.w.strb #= 0xF // strb for 4 Bytes
      bus.aw.valid #= true
      bus.w.valid #= true
      dut.clockDomain.waitSamplingWhere(bus.aw.ready.toBoolean && bus.w.ready.toBoolean)
      bus.aw.valid #= false
      bus.w.valid #= false
    }

    val b = fork {
      bus.b.ready #= true
      dut.clockDomain.waitSamplingWhere(bus.b.valid.toBoolean)
      bus.b.ready #= false
    }
    awa.join()
    b.join()
  }

  def readAxi4LiteReg(dut: Component, bus: AxiLite4, addr: Int): BigInt ={
    var data: BigInt = 1
    val ar = fork{
      bus.ar.addr #= addr
      bus.ar.valid #= true
      dut.clockDomain.waitSamplingWhere(bus.ar.ready.toBoolean)
      bus.ar.valid #= false
    }

    val r = fork{
      bus.r.ready #= true
      dut.clockDomain.waitSamplingWhere(bus.r.valid.toBoolean)
      data = bus.r.data.toBigInt
    }
    ar.join()
    r.join()
    return data
  }

  def opReq2BigInt(addr: Int, data: BigInt, mode: BigInt, upgrade: BigInt): BigInt ={
    addr + (data << 64) + (mode << (64+64)) + (upgrade << (64 + 64 + 1))
  }

  def multi_op(dut: OpTop): Unit ={
    dut.clockDomain.forkStimulus(period = 10)

    val m_axi_mem = AxiMemorySim(dut.io.m_axi, dut.clockDomain, AxiMemorySimConfig(
      maxOutstandingReads=128,
      maxOutstandingWrites=128,
      readResponseDelay=10,
      writeResponseDelay=10,
      useCustom = true
    ))
    m_axi_mem.start()
    // init data in axi mem
    val mem_init = Array.fill[Byte](1024)(0.toByte)
    m_axi_mem.memory.writeArray(0, mem_init)



    val req_axi_mem = AxiMemorySim(dut.io.req_axi, dut.clockDomain, AxiMemorySimConfig(
      maxOutstandingReads=128,
      maxOutstandingWrites=128,
      readResponseDelay=10,
      writeResponseDelay=10,
      useCustom = true
    ))
    req_axi_mem.start()

    val req_mem_init = Array.fill[Byte](8192)(0.toByte)

    // init txn array, each element in array is a txn with multi r/w operations
    var arrayTxn = new ArrayBuffer[mutable.ListBuffer[BigInt]]()
    for (_ <- 0 until 2){
      var reqQueue = new mutable.ListBuffer[BigInt]()
      for (k <- 0 until 8){
        reqQueue += opReq2BigInt(k * 64, 0, 0, 0)
        reqQueue += opReq2BigInt((4 + k)*64, 0, 1, 0) // write req
      }
      arrayTxn += reqQueue
    }

    var req_mem_ptr = 0
    for (reqQueue <- arrayTxn) {
      for (req <- reqQueue){
        val reqByte = req.toByteArray
        for (ii <- reqByte.indices) {
          req_mem_init(req_mem_ptr + ii) = reqByte(reqByte.length-1-ii) // toByteArray: the MSB is the 0th byte..
        }
        req_mem_ptr += 64 // 512-bit lane
      }
    }
    // init to
    req_axi_mem.memory.writeArray(0, req_mem_init)

    // start
    setAxi4LiteReg(dut, dut.io.s_axi_control, 0x10, 8) // txnLen
    setAxi4LiteReg(dut, dut.io.s_axi_control, 0x14, 2) // txnCnt
    setAxi4LiteReg(dut, dut.io.s_axi_control, 0x18, 0) // addrOffset Op0
    setAxi4LiteReg(dut, dut.io.s_axi_control, 0x1c, 64 * 16 * 1) // addrOffset Op1

    setAxi4LiteReg(dut, dut.io.s_axi_control, 0x00, 1) // ap_start

    dut.clockDomain.waitSampling(1000)

    for (i <- 0 until 4) {
      println(s"Reg[$i] = ${readAxi4LiteReg(dut, dut.io.s_axi_control, 0x20 + 4 * i)}")
    }

    println(s"Ctrl reg= ${readAxi4LiteReg(dut, dut.io.s_axi_control, 0)}")
    dut.clockDomain.waitSampling(10)
    println(s"Ctrl reg= ${readAxi4LiteReg(dut, dut.io.s_axi_control, 0)}")

//    dut.io.txnLen #= 8
//    dut.io.txnCnt #= 2
//    dut.io.addrOffset(0) #= 0
//    dut.io.addrOffset(1) #= 64 * 16 * 1
//
//    dut.io.ap.start #= true

//    dut.clockDomain.waitSamplingWhere(dut.io.ap.done.toBoolean)

//      for (i <- 0 until 2) {
//        println(s"txnExeCnt($i)=${dut.io.txnExeCnt(i).toBigInt}")
//        println(s"txnAbortCnt($i)=${dut.io.txnAbortCnt(i).toBigInt}")
//      }

  }


  test("multi_op") {
    SimConfig.withWave.compile {
      val dut = new OpTop(2)
      dut
    }.doSim("multi_op", 99)(multi_op)
  }

}