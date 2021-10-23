/*
 *
 */

package mylib

import spinal.core.{UInt, _}
import spinal.lib._
import spinal.lib.StreamArbiterFactory
import spinal.lib.StreamDispatcherSequencial
import spinal.lib.slave
import spinal.lib.master
import spinal.lib.bus.amba4.axi._

import scala.language.postfixOps

object EciPChaseMT {

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

    val numPE = 16

}

case class EciPChaseMT(addressAxiWidth: Int, dataWidth: Int) extends Component {

  import EciPChaseMT._

  val axiConfig = EciPChaseMT.getConfigs(addressAxiWidth, dataWidth)

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

  val wid_out = widthOf(io.md_out)+widthOf(io.dr_out)
  val wid_md = widthOf(io.md_out)

//  val vecOutStream = Vec(master Stream(Bits(wid_out bits)), numPE)
  val vecOutStream = Vec(Stream(Bits(wid_out bits)), numPE)
  val outArbiter = StreamArbiterFactory.roundRobin.build(Bits(wid_out bits), numPE)
  (vecOutStream, outArbiter.io.inputs).zipped.foreach(_ >> _)
  io.md_out := outArbiter.io.output.payload(wid_out-1 downto widthOf(io.dr_out)).asUInt
  io.dr_out := outArbiter.io.output.payload(widthOf(io.dr_out)-1 downto 0).asUInt
  io.md_out_valid := outArbiter.io.output.valid
  io.dr_out_valid := outArbiter.io.output.valid
  outArbiter.io.output.ready := io.md_out_ready & io.dr_out_ready

  val reqStream = Stream(Bits(wid_md bits))
  io.req_ready := reqStream.ready
  reqStream.valid := io.req_valid
  reqStream.payload := io.transaction_id ## io.line_number

//  val vecReqStream = Vec(Stream(Bits(wid_md bits)), numPE)
//  (vecReqStream, StreamDispatcherSequencial(reqStream, numPE)).zipped.foreach(_ << _)

  // some issue with `StreamDispatcherSequencial` a simple dispatcher here
  val select_cnt = Counter(numPE)
  when (reqStream.fire){
    select_cnt.increment()
  }
  val vecReqStream = Vec(Stream(Bits(wid_md bits)), numPE)
  reqStream.ready := False
  for (i <- 0 until numPE) {
    vecReqStream(i).payload := reqStream.payload
    when(i === select_cnt.value){
      vecReqStream(i).valid := reqStream.valid
      reqStream.ready := vecReqStream(i).ready
    } otherwise {
      vecReqStream(i).valid := False
    }
  }


  val arrayPE = Array.fill(numPE)(new EciPChase(addressAxiWidth, dataWidth))
  val axiArbiter = Axi4ReadOnlyArbiter(axiConfig, numPE)
  axiArbiter.io.output <> io.axi

  for (i <- 0 until numPE) {

    // request connection
    arrayPE(i).io.req_valid := vecReqStream(i).valid
    arrayPE(i).io.line_number := vecReqStream(i).payload(widthOf(io.line_number)-1 downto 0).asUInt
    arrayPE(i).io.transaction_id := vecReqStream(i).payload(wid_md-1 downto widthOf(io.line_number)).asUInt
    vecReqStream(i).ready := arrayPE(i).io.req_ready

    // out connection
    vecOutStream(i).valid := arrayPE(i).io.md_out_valid & arrayPE(i).io.dr_out_valid
    vecOutStream(i).payload := arrayPE(i).io.md_out ## arrayPE(i).io.dr_out
    arrayPE(i).io.md_out_ready := vecOutStream(i).ready
    arrayPE(i).io.dr_out_ready := vecOutStream(i).ready

    // axir connection
    axiArbiter.io.inputs(i) <> arrayPE(i).io.axi
  }

}

object EciPChaseMTMain {
  def main(args: Array[String]) {
    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC), targetDirectory = "rtl").generateVerilog(new EciPChaseMT(64, 512))
  }
}