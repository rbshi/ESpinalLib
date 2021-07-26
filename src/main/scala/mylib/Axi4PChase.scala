/*
 *
 */

package mylib

import spinal.core.{UInt, _}
import spinal.lib._
import spinal.lib.bus.amba4.axi._

import scala.language.postfixOps


object Axi4PChase {

  /**
   * State of the state machine of the wrapper
   */
  object AxiPChasePhase extends SpinalEnum{
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
case class Axi4PChase(addressAxiWidth: Int, dataWidth: Int) extends Component {

  import Axi4PChase._

  val axiConfig = Axi4PChase.getConfigs(addressAxiWidth, dataWidth)

  val io = new Bundle {
    // axi write interface
    val axi = master(Axi4ReadOnly(axiConfig))

    // control signal (traslated from AXI-lite)
    val start_addr = in UInt (axiConfig.addressWidth bits)
    val num_burst = in UInt (32 bits)

    val cnt_clk = out UInt (32 bits)

    // standard Xilinx accelerator control signals
    val ap = ApIO()
  }

  val streamAddrRd, streamAddrIssue = new Stream(axiConfig.addressType)
  val streamFifo = StreamFifo(dataType = axiConfig.addressType, depth = 16)
  streamFifo.io.push << streamAddrRd
  streamFifo.io.pop >> streamAddrIssue

  io.ap.setDefault()
  val phase = RegInit(AxiPChasePhase.IDLE)

  val rCntClk = Reg(cloneOf(io.cnt_clk))
  rCntClk init(0)
  io.cnt_clk := rCntClk

  streamAddrRd.valid := False

  io.axi.readCmd.size := (log2Up(axiConfig.dataWidth / 8))
  io.axi.readCmd.id := 0
  io.axi.readCmd.len := 0
  io.axi.readCmd.addr := streamAddrIssue.payload
  io.axi.readCmd.setBurstINCR()

  io.axi.readCmd.valid := streamAddrIssue.valid
  streamAddrIssue.ready := io.axi.readCmd.ready

  io.axi.readRsp.ready := streamAddrRd.ready

  streamAddrRd.payload := 0

  val rCntRead = Reg(cloneOf(io.num_burst))
  rCntRead init(0)

  /**
   * Main state machine
   */
  val sm = new Area {
    switch(phase) {
      is(AxiPChasePhase.IDLE) {
        when(io.ap.reqStart()) {
          phase := AxiPChasePhase.SETUP
          io.ap.setIdle(False)
        }
      }

      is(AxiPChasePhase.SETUP) {
        io.ap.setIdle(False)
        rCntClk := 0
        rCntRead := 0

        streamAddrRd.valid := True
        streamAddrRd.payload := io.start_addr

        phase := AxiPChasePhase.READ

      }

      is(AxiPChasePhase.READ) {

        io.ap.setIdle(False)

        rCntClk := rCntClk + 1

        // connect
        streamAddrRd.valid := io.axi.readRsp.valid
        streamAddrRd.payload := io.axi.readRsp.data(axiConfig.addressWidth-1 downto 0).asUInt

        when(io.axi.readRsp.fire){
          rCntRead := rCntRead + 1
        }

        when(rCntRead === io.num_burst){
          phase := AxiPChasePhase.DONE
        }
      }

      is(AxiPChasePhase.DONE) {
        io.ap.setIdle(False)
        io.ap.setReady(True)
        io.ap.setDone(True)
        phase := AxiPChasePhase.IDLE
      }

    }
  }

}

object Axi4PChaseMain {
  def main(args: Array[String]) {
    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC), targetDirectory = "rtl").generateVerilog(new Axi4PChase(64, 512))
  }
}