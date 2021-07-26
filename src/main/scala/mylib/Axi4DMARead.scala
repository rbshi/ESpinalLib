/*
 *
 */

package mylib

import spinal.core.{UInt, _}
import spinal.lib._
import spinal.lib.bus.amba4.axi._

import scala.language.postfixOps


object Axi4DMARead{

  /**
   * State of the state machine of the wrapper
   */
  object Axi4DMAReadPhase extends SpinalEnum{
    val IDLE, SETUP, READ, DONE = newElement
  }

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
case class Axi4DMARead(addressAxiWidth: Int, dataWidth: Int) extends Component {

  import Axi4DMARead._

  val axiConfig = Axi4DMARead.getConfigs(addressAxiWidth, dataWidth)

  val io = new Bundle {
    // axi interface
    val axi = master(Axi4ReadOnly(axiConfig))

    // control signal (traslated from AXI-lite)
    val start_addr = in UInt (axiConfig.addressWidth bits)
    val len_burst = in UInt (widthOf(axi.ar.len) bits) // unit: beats in a burst FIXME: width
    val num_burst = in UInt (32 bits)
    val stride = in UInt (8 bits) // unit: burst
    val cnt_clk = out UInt (32 bits)

    // standard Xilinx accelerator control signals
    val ap = ApIO()
  }

  val streamAddrRd, streamAddrIssue = new Stream(axiConfig.addressType)
  val streamFifo = StreamFifo(dataType = axiConfig.addressType, depth = 16)
  streamFifo.io.push << streamAddrRd
  streamFifo.io.pop >> streamAddrIssue

  io.ap.setDefault()
  val phase = RegInit(Axi4DMAReadPhase.IDLE)

  val rCntClk = Reg(cloneOf(io.cnt_clk)) init(0)
  io.cnt_clk := rCntClk

  io.axi.readCmd.size := (log2Up(axiConfig.dataWidth / 8))
  io.axi.readCmd.id := 0
  io.axi.readCmd.len := io.len_burst
  io.axi.readCmd.addr := streamAddrIssue.payload
  io.axi.readCmd.setBurstINCR()
  io.axi.readCmd.valid := streamAddrIssue.valid

  streamAddrIssue.ready := io.axi.readCmd.ready

  io.axi.readRsp.ready := True // keep high, drop the read input

  val rAddr = Reg(cloneOf(io.axi.ar.addr))
  val rAddrRdValid = Reg(cloneOf(streamAddrRd.valid)) init(False)

  val rAddrInc = Reg(cloneOf(io.axi.ar.addr)) init(0)

  streamAddrRd.valid := rAddrRdValid
  streamAddrRd.payload := rAddr

  val rCntRead = Reg(cloneOf(io.num_burst)) init(0)
  val rTotalRead = Reg(cloneOf(io.num_burst)) init(0)

  val lenBurst = Reg(cloneOf(io.axi.ar.len)) init(0)
  val numBurst = Reg(cloneOf(io.num_burst))

  /**
   * Main state machine
   */
  val sm = new Area {
    switch(phase) {
      is(Axi4DMAReadPhase.IDLE) {
        when(io.ap.reqStart()) {
          phase := Axi4DMAReadPhase.SETUP
          io.ap.setIdle(False)
        }
      }

      is(Axi4DMAReadPhase.SETUP) {
        io.ap.setIdle(False)

        rCntClk := 0
        rCntRead := 0

        rAddrRdValid := True
        rAddr := io.start_addr
        rAddrInc := ((io.stride * (io.len_burst + 1)) << io.axi.ar.size).resized
        rTotalRead := ((io.num_burst+1) * (io.len_burst+1)).resized
        // rTotalRead := (io.num_burst+1)

        numBurst := io.num_burst

        phase := Axi4DMAReadPhase.READ
      }

      is(Axi4DMAReadPhase.READ) {

        io.ap.setIdle(False)
        rCntClk := rCntClk + 1

//        when(streamAddrRd.fire){
//          when(~(numBurst === 0)) {
//            rAddrRdValid := True
//            numBurst := numBurst - 1
//            rAddr := rAddr + rAddrInc
//          }otherwise{
//            rAddrRdValid := False
//          }
//        }

        when(streamAddrRd.fire && numBurst === 0){
          rAddrRdValid := False
        }

        when(streamAddrRd.fire && ~(numBurst === 0)){
          rAddrRdValid := True
          numBurst := numBurst - 1
          rAddr := rAddr + rAddrInc
        }


        when(io.axi.readRsp.fire){
          rCntRead := rCntRead + 1
        }

        when(rCntRead === rTotalRead){
          phase := Axi4DMAReadPhase.DONE
        }
      }

      is(Axi4DMAReadPhase.DONE) {
        io.ap.setIdle(False)
        io.ap.setReady(True)
        io.ap.setDone(True)
        phase := Axi4DMAReadPhase.IDLE
      }

    }
  }

}

object Axi4DMAReadMain {
  def main(args: Array[String]) {
    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC), targetDirectory = "rtl").generateVerilog(new Axi4DMARead(64, 512))
  }
}