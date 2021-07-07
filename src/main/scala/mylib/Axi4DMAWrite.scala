/*
 *
 */

package mylib

import spinal.core.{UInt, _}
import spinal.lib._
import spinal.lib.bus.amba4.axi._

import scala.language.postfixOps


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
    // axi write interface
    val axi = master(Axi4WriteOnly(axiConfig))

    // control signal (traslated from AXI-lite)
    val start_addr = in UInt (axiConfig.addressWidth bits)

    val ap = ApIO()
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

  io.ap.setDefault()


  /**
   * Main state machine
   */
  val sm = new Area {

    switch(phase) {
      is(IDLE) {

        when(io.ap.reqStart()) {
          phase := SETUP
          io.ap.setIdle(False)
        }
      }

      is(SETUP) {
        io.ap.setIdle(False)

        io.axi.aw.addr := io.start_addr
        phase := WRITE
      }

      is(WRITE) {

        io.ap.setIdle(False)

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
        io.ap.setIdle(False)

        when(io.axi.b.valid) {
          io.ap.setReady(True)
          io.ap.setDone(True)
          io.axi.b.ready := True
          phase := IDLE
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