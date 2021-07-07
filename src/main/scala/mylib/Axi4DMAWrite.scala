/*
 *
 */

package mylib

import spinal.core.{UInt, _}
import spinal.lib._
import spinal.lib.bus.amba4.axi._

/**
 * State of the state machine of the wrapper
 */
object Axi4DMAWritePhase extends SpinalEnum{
  val IDLE, SETUP, WRITE, RESPONSE = newElement
}


object Axi4DMAWrite {
  /**
   * Return the axi and bram configuration
   */
  def getConfigs(addressAxiWidth: Int, dataWidth: Int): Axi4Config =
    Axi4Config(
      addressWidth = addressAxiWidth,
      dataWidth    = dataWidth,
      idWidth = 1,
      useStrb = true,
      useBurst = true,
      useId = true,
      useLock      = false,
      useRegion    = false,
      useCache     = false,
      useProt      = false,
      useQos       = false
    )
}


/**
 * Issue Axi4 write transactions in strode memory addresses with burst
 */
case class Axi4DMAWrite(addressAxiWidth: Int, dataWidth: Int) extends Component {

  import Axi4DMAWritePhase._

  val axiConfig = Axi4DMAWrite.getConfigs(addressAxiWidth, dataWidth)

  val io = new Bundle {
    val axi = master(Axi4WriteOnly(axiConfig))
    val start_addr = in UInt (axiConfig.addressWidth bits)
    val ap_start = in Bool()
    val ap_ready = out(Reg(Bool()) init(false))
    val ap_done = out(Reg(Bool()) init(false))
    val ap_idle = out(Reg(Bool()) init(true))
  }

  val phase = RegInit(IDLE)
  val lenBurst = Reg(cloneOf(io.axi.aw.len))


  io.axi.aw.setBurstINCR()
  io.axi.aw.len := 0 // FIXME (Number of beat inside the burst)
  io.axi.aw.setSize(log2Up(axiConfig.dataWidth / 8)) // (byte of each beat)
  io.axi.aw.id := 0

  io.axi.w.strb := B(widthOf(io.axi.w.strb) bits, default -> True)

  io.axi.aw.addr := 0
  io.axi.aw.valid := False
  io.axi.w.data := 0
  io.axi.w.valid := False
  io.axi.w.last := False
  io.axi.b.ready := True



  io.ap_idle := True
  io.ap_ready := False
  io.ap_done := False

  /**
   * Main state machine
   */
  val sm = new Area {

    switch(phase) {
      is(IDLE) {
        io.ap_ready := False
        io.ap_done := False
        io.ap_idle := True
        when(io.ap_start) {
          phase := SETUP
          io.ap_idle := False
        }
      }

      is(SETUP) {
        io.axi.aw.addr := io.start_addr
        phase := WRITE
      }

      is(WRITE) {
        // axi.aw
        io.axi.aw.valid := True
        when(io.axi.aw.ready) {
          Axi4.incr(io.axi.aw.addr , io.axi.aw.burst, io.axi.aw.len, io.axi.aw.size, axiConfig.dataWidth/8)
          lenBurst := lenBurst - 1
        }
        // axi.w
        io.axi.w.valid := True
        io.axi.w.data := B(widthOf(io.axi.w.data) bits, default -> true)
        io.axi.w.last := True

        phase := RESPONSE
      }

      is(RESPONSE) {
        when(io.axi.b.valid) {
          io.ap_idle := True
          io.ap_ready := True
          io.ap_done := True

          io.axi.b.ready := True
          phase := IDLE
        } otherwise {
          io.axi.b.ready := False
        }
      }
    }
  }

}

object Axi4DMAWriteMain {
  def main(args: Array[String]) {
    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC), targetDirectory = "rtl").generateVerilog(new Axi4DMAWrite(32, 512))
  }
}