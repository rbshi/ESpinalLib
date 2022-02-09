package util

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite

import scala.language.postfixOps

class EMemTest extends AnyFunSuite {

  def wr_after_rd(dut: EMem): Unit ={
    dut.clockDomain.forkStimulus(period = 10)
    dut.clockDomain.waitSampling(10)
    val addone = fork {
      for (i <- 0 until 16){
        setAdd(dut, i%4, 1)
        dut.clockDomain.waitSampling(1)
      }

      setDef(dut)

      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()

      for (i <- 0 until 16){
        setRead(dut, i)
        dut.clockDomain.waitRisingEdge()
        println(s"Addr[$i]=${dut.io.data.toBigInt}")
      }

    }
    addone.join()
  }

  def setAdd(dut: EMem, addr: Int, addVal: Int): Unit ={
    dut.io.addr #= addr
    dut.io.addVal #= addVal
    dut.io.enAdd #= true
    dut.io.enRd #= false
  }

  def setRead(dut: EMem, addr: Int): Unit ={
    dut.io.addr #= addr
    dut.io.enRd #= true
    dut.io.enAdd #= false
  }

  def setDef(dut: EMem): Unit ={
    dut.io.enRd #= false
    dut.io.enAdd #= false
  }



  test("wr_after_rd") {
    SimConfig.withWave.compile {
      val dut = new EMem(UInt(32 bits), 128)
      dut.io.simPublic()
      dut
    }.doSim("wr_after_rd", 99)(wr_after_rd)
  }

}