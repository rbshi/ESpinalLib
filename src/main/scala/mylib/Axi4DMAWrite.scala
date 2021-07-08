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
    val len_burst = in UInt (widthOf(axi.aw.len) bits) // unit: beats in a burst FIXME: width
    val num_burst = in UInt (8 bits)
    val stride = in UInt (8 bits) // unit: burst

    // standard Xilinx accelerator control signals
    val ap = ApIO()
  }


  io.ap.setDefault()

  val phase = RegInit(IDLE)
  val lenBurst = Reg(cloneOf(io.axi.aw.len)) init(0)
  val numBurst = Reg(cloneOf(io.num_burst))
  val numBurstA = Reg(cloneOf(io.num_burst))

  val addr = Reg(cloneOf(io.axi.aw.addr))
  val aw_valid = Reg(cloneOf(io.axi.aw.valid)) init(False)
  val aw_ready = Reg(cloneOf(io.axi.aw.valid)) init(True)


  val aw = Reg(cloneOf(io.axi.aw))
  val w = Reg(cloneOf(io.axi.w))
  val b = Reg(cloneOf(io.axi.b))

  // init axi control registers
  aw.valid init(False)
  aw.ready init(False)
  aw.addr init(0)
  w.valid init(False)
  w.ready init(False)
  w.data init(0)


  io.axi.aw.setBurstINCR()
  io.axi.aw.len := io.len_burst // FIXME (Number of beat inside the burst)
  io.axi.aw.setSize(log2Up(axiConfig.dataWidth / 8)) // (byte of each beat)
  io.axi.aw.id := 0

  io.axi.w.strb := B(widthOf(io.axi.w.strb) bits, default -> True)

  io.axi.aw.addr := aw.addr
  io.axi.aw.valid := aw.valid
  io.axi.w.data := w.data
  io.axi.w.valid := w.valid
  io.axi.w.last := False
  io.axi.b.ready := True

  aw.ready := io.axi.aw.ready
  w.ready := io.axi.w.ready

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
        aw.addr := io.start_addr
        lenBurst := io.len_burst
        numBurst := io.num_burst
        numBurstA := io.num_burst
        phase := WRITE
      }

      is(WRITE) {

        io.ap.setIdle(False)

        // axi.aw
        when(aw.valid && aw.ready) {
          numBurstA := numBurstA - 1
          aw.addr := aw.addr + ((io.stride * (io.len_burst + 1)) << io.axi.aw.size)
        }

        when(aw.ready && aw.valid && numBurstA === 0){
          aw.valid := False
        }otherwise{
          aw.valid := True
        }

        // axi.w
        w.valid := True
        w.data := B(widthOf(io.axi.w.data) bits, default -> true)

        when(w.valid && w.ready) {
          when(lenBurst === 0) {
            lenBurst := io.len_burst
            numBurst := numBurst - 1
          }otherwise{
            lenBurst := lenBurst - 1
          }
        }

        when(lenBurst === 0) {
          io.axi.w.last := True
        }

        when(numBurst === 0 && lenBurst === 0 && io.axi.w.ready) {
          w.valid := False
          phase := RESPONSE
        }
      }


      is(RESPONSE) {
        io.ap.setIdle(False)
        io.ap.setReady(True)
        io.ap.setDone(True)
        io.axi.b.ready := True
        phase := IDLE
      }
    }

  }

}

object Axi4DMAWriteMain {
  def main(args: Array[String]) {
    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC), targetDirectory = "rtl").generateVerilog(new Axi4DMAWrite(32, 512))
  }
}