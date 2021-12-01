// Functions used for simulation

package util

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config}

import scala.collection._
import scala.util.Random
import scala.math._
import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.{master, slave}


trait SimFunSuite{

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

}


