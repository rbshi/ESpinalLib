package tm


import spinal.core.{UInt, _}
import spinal.core
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.fsm.StateMachine

import util.RenameIO

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class VitisAdder extends Component with RenameIO {

  val io = new Bundle {
    val s_axi_control = slave(AxiLite4(AxiLite4Config(12, 32)))
  }

  val ctlReg = new AxiLite4SlaveFactory(io.s_axi_control)

  val ap_start = ctlReg.createReadAndWrite(Bool(), 0, 0).init(False)
  val ap_done = ctlReg.createReadOnly(Bool(), 0, 1).init(False)
  val ap_idle = ctlReg.createReadOnly(Bool(), 0, 2).init(True)
  val ap_ready = ctlReg.createReadOnly(Bool(), 0, 3).init(False)
  ctlReg.onRead(0){ap_done.clear()}

  val regA = ctlReg.createReadAndWrite(UInt(32 bits), 0x10, 0)
  val regB = ctlReg.createReadAndWrite(UInt(32 bits), 0x14, 0)
  val regC = ctlReg.createReadAndWrite(UInt(32 bits), 0x18, 0)

  val sm = new StateMachine{

    val IDLE = new State with EntryPoint
    val START = new State

    IDLE.whenIsActive{
      ap_idle := True
      ap_ready := False
      when(ap_start) {
        ap_idle := False
        goto(START)
      }
    }

    START.whenIsActive{
      ap_done := True
      ap_idle := True
      ap_ready := True
      regC := regA + regB
      goto(IDLE)
    }
  }
}

object VitisAdderMain {
  def main(args: Array[String]) {
    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = LOW), targetDirectory = "rtl").generateVerilog{
      val top = new VitisAdder
      top.renameIO()
      top
    }
  }
}
