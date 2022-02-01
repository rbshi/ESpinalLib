package coyote

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

class RdmaFlowTopTest extends AnyFunSuite with SimFunSuite {

  def rdma_flow_test(dut: RdmaFlowTop): Unit = {

    // default
    dut.io.axi_ctrl.ar.valid #= false
    dut.io.axi_ctrl.aw.valid #= false
    dut.io.axi_ctrl.w.valid #= false
    dut.io.axis_rdma_1_sink.valid #= false
    dut.io.rdma_1_rd_req.valid #= false
    dut.io.rdma_1_wr_req.valid #= false

    dut.clockDomain.forkStimulus(period = 10)

    setAxi4LiteReg(dut, dut.io.axi_ctrl, 1<<6, 10) // lenpw
    setAxi4LiteReg(dut, dut.io.axi_ctrl, 2<<6, 256) // cnt
    setAxi4LiteReg(dut, dut.io.axi_ctrl, 3<<6, 2) // itvlpush
    setAxi4LiteReg(dut, dut.io.axi_ctrl, 4<<6, 2) // itvlpop
    setAxi4LiteReg(dut, dut.io.axi_ctrl, 5<<6, 32) // nOnFly
    setAxi4LiteReg(dut, dut.io.axi_ctrl, 6<<6, 1000) // clkTimeOut
    // start
    setAxi4LiteReg(dut, dut.io.axi_ctrl, 0<<6, 3) // mstr

    dut.clockDomain.waitSampling(1000)

  }

  test("rdma_flow_test") {
    SimConfig.withWave.compile {
      val dut = new RdmaFlowTop()
      dut
    }.doSim("rdma_flow_test", 99)(rdma_flow_test)

  }

}