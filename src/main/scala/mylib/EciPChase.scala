/*
 *
 */

package mylib

import spinal.core.{UInt, _}
import spinal.lib._
import spinal.lib.bus.amba4.axi._

import scala.language.postfixOps

object EciPChase {

  /**
   * State of the state machine of the wrapper
   */
  object EciPChasePhase extends SpinalEnum{
    val IDLE, READ, SEND = newElement
  }

  /**
   * Return the axi and bram configuration
   */
  def getConfigs(addressAxiWidth: Int, dataWidth: Int): Axi4Config =
    Axi4Config(
      addressWidth = addressAxiWidth,
      dataWidth    = dataWidth,
      idWidth = 2,
      useStrb = false,
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
case class EciPChase(addressAxiWidth: Int, dataWidth: Int) extends Component {

  import EciPChase._

  val axiConfig = EciPChase.getConfigs(addressAxiWidth, dataWidth)

  val io = new Bundle {
    // axi write interface
    val axi = master(Axi4ReadOnly(axiConfig))

    // eci request
    val line_number = in UInt(33 bits)
    val transaction_id = in UInt(5 bits)
    val req_valid = in Bool()
    val req_ready = out Bool()

    // meta data
    val md_out = out UInt(38 bits)
    val md_out_valid = out Bool()
    val md_out_ready = in Bool()

    // output data
    val dr_out = out UInt(1024 bits)
    val dr_out_valid = out Bool()
    val dr_out_ready = in Bool()
  }


  val rdAddrFifo = StreamFifo(dataType = axiConfig.addressType, depth = 16)
  rdAddrFifo.io.push.valid := False
  rdAddrFifo.io.push.payload := 0
  rdAddrFifo.io.pop.ready := io.axi.readCmd.ready

  io.axi.readCmd.valid := rdAddrFifo.io.pop.valid
  io.axi.readCmd.size := (log2Up(axiConfig.dataWidth / 8))
  io.axi.readCmd.id := 0
  io.axi.readCmd.len := 0
  io.axi.readCmd.addr := rdAddrFifo.io.pop.payload
  io.axi.readCmd.setBurstINCR()

  io.axi.readRsp.ready := True // always ready to take the input

  val rLineNum = Reg(cloneOf(io.line_number)).init(0)
  val rTransId = Reg(cloneOf(io.transaction_id)).init(0)

  val rDrOut = Reg(cloneOf(io.dr_out)).init(0)

  // default outout
  io.dr_out := rDrOut
  io.dr_out_valid := False
  io.md_out := (rTransId ## rLineNum).asUInt
  io.md_out_valid := False
  io.req_ready := False

  val key = io.axi.readRsp.data(63 downto 0).asUInt
  val next = io.axi.readRsp.data(127 downto 64).asUInt
  val value = io.axi.readRsp.data(511 downto 0).asUInt // FIXME

  val rCntRd = Reg(UInt(32 bits)).init(0)


  /**
   * Main state machine
   */
  val sm = new Area {

    val phase = RegInit(EciPChasePhase.IDLE)

    switch(phase) {
      is(EciPChasePhase.IDLE) {
        io.req_ready := True
        rCntRd := 0

        when(io.req_valid){
          rLineNum := io.line_number
          rTransId := io.transaction_id

          rdAddrFifo.io.push.valid := True
          rdAddrFifo.io.push.payload := (io.line_number << 7).resized // start search address (line_number * 128)
          phase := EciPChasePhase.READ
        }
      }

      is(EciPChasePhase.READ) {

        when(io.axi.readRsp.fire){

          rCntRd := rCntRd + 1

          when(key === rLineNum){
            rDrOut := value.resized
            phase := EciPChasePhase.SEND
          }otherwise{
            rdAddrFifo.io.push.valid := io.axi.readRsp.valid
            rdAddrFifo.io.push.payload := next
          }

          when(rCntRd(9) === True){
            rDrOut := (default -> true)
            phase := EciPChasePhase.SEND
          }
        }
      }

      is(EciPChasePhase.SEND){
        io.dr_out_valid := True
        io.md_out_valid := True
        when(io.md_out_ready & io.dr_out_ready){
          phase := EciPChasePhase.IDLE
        }
      }

    }
  }
}

object EciPChaseMain {
  def main(args: Array[String]) {
    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC), targetDirectory = "rtl").generateVerilog(new EciPChase(64, 512))
  }
}