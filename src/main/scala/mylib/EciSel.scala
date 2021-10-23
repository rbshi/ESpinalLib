/*
 *
 */

package mylib

import spinal.core.Component.push
import spinal.core.{UInt, _}
import spinal.lib._
import spinal.lib.bus.amba4.axi._

import scala.language.postfixOps

object EciSel {

  /**
   * State of the state machine of the wrapper
   */
  object EciRdCmdPhase extends SpinalEnum{
    val IDLE, CONFIG, READ, DONE = newElement
  }

  object EciRdRespPhase extends SpinalEnum{
    val RD_P1, RD_P2 = newElement
  }

  object EciWatch extends SpinalEnum{
    val IDLE, WATCH = newElement
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
case class EciSel(addressAxiWidth: Int, dataWidth: Int) extends Component {

  import EciSel._

  val axiConfig = EciSel.getConfigs(addressAxiWidth, dataWidth)

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

  val mdFifo = StreamFifo(dataType = UInt(widthOf(io.transaction_id ## io.line_number) bits), depth = 16)
  mdFifo.io.push.valid := False
  mdFifo.io.push.payload := reqFifo.io.pop.payload

  val drFifo = StreamFifo(dataType = UInt(1024 bits), depth = 512)
  drFifo.io.push.valid := False


  val rdAddrFifo = StreamFifo(dataType = axiConfig.addressType, depth = 512)
  rdAddrFifo.io.push.valid := False
  rdAddrFifo.io.push.payload := 0

  io.axi.readCmd.valid := rdAddrFifo.io.pop.valid

  io.axi.readCmd.size := (log2Up(axiConfig.dataWidth / 8))
  io.axi.readCmd.id := 0
  io.axi.readCmd.len := 1 // read two lane to construct one cache line
  io.axi.readCmd.addr := rdAddrFifo.io.pop.payload
  io.axi.readCmd.setBurstINCR()
  rdAddrFifo.io.pop.ready := io.axi.readCmd.ready

  io.axi.readRsp.ready := drFifo.io.push.ready

  val rFlagConfig = Reg(Bool()).init(False)
  val rFlagEndRd = Reg(Bool()).init(False)
  val rFlagEndRdD = Reg(cloneOf(rFlagEndRd)).init(False)
  rFlagEndRdD := rFlagEndRd

  val rRdAddr = Reg(axiConfig.addressType).init(128) // start search from 1st cache line
  val rEndAddr = Reg(axiConfig.addressType).init(0) // last search address
  val rDoneCnt = Reg(UInt(32 bits)).init(0)


  reqFifo.io.flush := False
  mdFifo.io.flush := False
  drFifo.io.flush := False
  rdAddrFifo.io.flush := False

  val watch_phase = RegInit(EciWatch.IDLE)

  val sm_rd_cmd = new Area {
    val phase = RegInit(EciSel.EciRdCmdPhase.IDLE)

    reqFifo.io.pop.ready := mdFifo.io.availability =/= 0
    when(reqFifo.io.pop.fire){
      mdFifo.io.push.valid := True
    }

    switch(phase){
      is(EciRdCmdPhase.IDLE){
        when(reqFifo.io.pop.fire) {
          rdAddrFifo.io.push.valid := True
          rdAddrFifo.io.push.payload := 0 // config parameters @Addr 0
          rFlagConfig := False
          rFlagEndRd := False
          rFlagEndRdD := False

          drFifo.io.flush := True // in case there's data on axi.resp

          phase := EciRdCmdPhase.CONFIG
        }
      }

      is(EciRdCmdPhase.CONFIG){
        when(rFlagConfig){
          rRdAddr := 128 // reset start from the first line
          phase := EciRdCmdPhase.READ
        }
      }

      is(EciRdCmdPhase.READ){
        rdAddrFifo.io.push.payload := rRdAddr
        rdAddrFifo.io.push.valid := True
        when(rdAddrFifo.io.push.fire){
          when(rRdAddr =/= rEndAddr){
            rRdAddr := rRdAddr + 128
          } otherwise {
            phase := EciRdCmdPhase.DONE
          }
        }
      }

      is(EciRdCmdPhase.DONE){
        when(watch_phase === EciWatch.IDLE){
          phase := EciRdCmdPhase.IDLE
        }
      }

    }
  }



  val sm_watch = new Area {
    switch(watch_phase){
      is(EciWatch.IDLE){
        when(sm_rd_cmd.phase === EciRdCmdPhase.READ){
          watch_phase := EciWatch.WATCH
        }
      }

      is(EciWatch.WATCH){
        rDoneCnt := rDoneCnt + 1
        when(rDoneCnt === (1<<30)){ // 1<<30 cycle ~= 3 seconds
          rDoneCnt := 0
          watch_phase := EciWatch.IDLE
          // clean the fifos for another query
          reqFifo.io.flush := True
          mdFifo.io.flush := True
          drFifo.io.flush := True
          rdAddrFifo.io.flush := True

          // force the readCmdPhase to IDLE
          sm_rd_cmd.phase := EciRdCmdPhase.IDLE
        }
      }
    }
  }



  val sm_rd_resp = new Area {
    val phase = RegInit(EciRdRespPhase.RD_P1)
    val resp_cahceline_p1 = Reg(axiConfig.dataType).init(0)
    val aVal = resp_cahceline_p1(31 downto 0).asUInt
    val bVal = resp_cahceline_p1(63 downto 32).asUInt

    val rRdCnt = Reg(UInt(32 bits)).init(0 )

    val aMax = Reg(UInt(32 bits)).init(0)
    val bMax = Reg(UInt(32 bits)).init(0)

    switch(phase){
      is(EciRdRespPhase.RD_P1){
        when(io.axi.readRsp.fire){
          resp_cahceline_p1 := io.axi.readRsp.data
          phase := EciRdRespPhase.RD_P2
        }
      }

      is(EciRdRespPhase.RD_P2){
        when(io.axi.readRsp.fire){
          when(sm_rd_cmd.phase === EciRdCmdPhase.CONFIG){
            // parse config
            aMax := resp_cahceline_p1(31 downto 0).asUInt
            bMax := resp_cahceline_p1(63 downto 32).asUInt
            rEndAddr := resp_cahceline_p1(127 downto 64).asUInt
            rRdCnt := (resp_cahceline_p1(127 downto 64).asUInt >> 7).resized
            rFlagConfig := True
          } otherwise {
            when((aVal <= aMax) & (bVal <= bMax)) {
              // io.axi.readRsp.ready = drFifo.io.push.ready
              drFifo.io.push.valid := True
            }
            rRdCnt := rRdCnt -1
          }
          phase := EciRdRespPhase.RD_P1

          when(rRdCnt === 1){
            rFlagEndRd := True
          }
        }
      }
    }
  }

  drFifo.io.push.payload := (io.axi.readRsp.data ## sm_rd_resp.resp_cahceline_p1).asUInt


  val outReady = Bool()

  when(rFlagEndRdD & (drFifo.io.occupancy === 0)){
    outReady := io.md_out_ready & io.dr_out_ready & mdFifo.io.pop.valid
    io.md_out_valid := mdFifo.io.pop.valid
    io.dr_out_valid := mdFifo.io.pop.valid
    io.dr_out := (default -> true)
  }otherwise{
    outReady := io.md_out_ready & io.dr_out_ready & mdFifo.io.pop.valid & drFifo.io.pop.valid

    io.md_out_valid := mdFifo.io.pop.valid & drFifo.io.pop.valid
    io.dr_out_valid := mdFifo.io.pop.valid & drFifo.io.pop.valid
    io.dr_out := drFifo.io.pop.payload
  }

  io.md_out := mdFifo.io.pop.payload

  mdFifo.io.pop.ready := outReady
  drFifo.io.pop.ready := outReady

}

object EciSelMain {
  def main(args: Array[String]) {
    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC), targetDirectory = "rtl").generateVerilog(new EciSel(64, 512))
  }
}