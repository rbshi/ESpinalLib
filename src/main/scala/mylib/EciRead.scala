/*
 *
 */

package mylib

import spinal.core.Component.push
import spinal.core.{UInt, _}
import spinal.lib._
import spinal.lib.bus.amba4.axi._

import scala.language.postfixOps

object EciRead {

  /**
   * State of the state machine of the wrapper
   */
  object EciRdCmdPhase extends SpinalEnum{
    val IDLE, READ = newElement
  }

  object EciRdRespPhase extends SpinalEnum{
    val P1, P2 = newElement
  }

  /**
   * Return the axi and bram configuration
   */
  def getConfigs(addressAxiWidth: Int, dataWidth: Int): Axi4Config =
    Axi4Config(
      addressWidth = addressAxiWidth,
      dataWidth    = dataWidth,
      idWidth = 6,
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
case class EciRead(addressAxiWidth: Int, dataWidth: Int) extends Component {

  import EciRead._

  val axiConfig = EciRead.getConfigs(addressAxiWidth, dataWidth)

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


  val reqFifo = StreamFifo(dataType = UInt(widthOf(io.transaction_id ## io.line_number) bits), depth = 16)
  reqFifo.io.push.valid := io.req_valid
  io.req_ready := reqFifo.io.push.ready
  reqFifo.io.push.payload := (io.transaction_id ## io.line_number).asUInt
//  reqFifo.io.pop.ready := False

  val mdFifo = StreamFifo(dataType = UInt(widthOf(io.transaction_id ## io.line_number) bits), depth = 16)
  mdFifo.io.push.valid := False
  mdFifo.io.push.payload := reqFifo.io.pop.payload
  mdFifo.io.pop.ready := False

  val rdAddrFifo = StreamFifo(dataType = axiConfig.addressType, depth = 16)
  rdAddrFifo.io.push.valid := False
  rdAddrFifo.io.push.payload := (reqFifo.io.pop.payload(widthOf(io.line_number)-1 downto 0) << 7).resized // line_number * 128 (cache line size) (duplicate two banks)

  io.axi.readCmd.valid := rdAddrFifo.io.pop.valid

  io.axi.readCmd.size := (log2Up(axiConfig.dataWidth / 8))
  io.axi.readCmd.id := 0
  io.axi.readCmd.len := 1 // read two lane to construct one cache line
  io.axi.readCmd.addr := rdAddrFifo.io.pop.payload
  io.axi.readCmd.setBurstINCR()
  rdAddrFifo.io.pop.ready := io.axi.readCmd.ready

  io.axi.readRsp.ready := io.dr_out_ready & io.md_out_ready // FIXME

  io.md_out_valid := False
  io.dr_out_valid := False


  val sm_rd_cmd = new Area {
    reqFifo.io.pop.ready := mdFifo.io.push.ready & rdAddrFifo.io.push.ready
    when(reqFifo.io.pop.fire) {
      mdFifo.io.push.valid := True
      rdAddrFifo.io.push.valid := True
    }
  }

  val sm_rd_resp = new Area {
    val phase = RegInit(EciRdRespPhase.P1)
    val resp_cahceline_p1 = Reg(axiConfig.dataType).init(0)
    switch(phase){
      is(EciRdRespPhase.P1){
        when(io.axi.readRsp.fire){
          resp_cahceline_p1 := io.axi.readRsp.data
          phase := EciRdRespPhase.P2
        }
      }

      is(EciRdRespPhase.P2){
        when(io.axi.readRsp.fire){
          mdFifo.io.pop.ready := True

          io.md_out_valid := True
          io.dr_out_valid := True

          phase := EciRdRespPhase.P1
        }
      }
    }
  }


  io.md_out := mdFifo.io.pop.payload
  io.dr_out := (io.axi.readRsp.data ## sm_rd_resp.resp_cahceline_p1).asUInt

}

object EciReadMain {
  def main(args: Array[String]) {
    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC), targetDirectory = "rtl").generateVerilog(new EciRead(64, 512))
  }
}