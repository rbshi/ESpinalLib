package tmcs

import spinal.core.{UInt, _}
import spinal.lib._

class LtCh(sysConf: SysConfig) extends Component{
  val io = new LockTableIO(sysConf)
  val ltAry = Array.fill(sysConf.nLtPart)(new LockTable(sysConf))

  // use the LSB bits for lkReq dispatching
  val lkReq2Lt = Stream(LkReq(sysConf, log2Up(sysConf.nLtPart)))

  lkReq2Lt.arbitrationFrom(io.lkReq)
  for ((name, elem) <- lkReq2Lt.payload.elements){
    if (name != "tId") elem := io.lkReq.payload.find(name)
  }
  lkReq2Lt.tId := io.lkReq.payload.tId(lkReq2Lt.tId.high downto log2Up(sysConf.nLtPart)).resized

  // demux lock_req to multiple LTs
  val lkReq2LtDemux = StreamDemux(lkReq2Lt, io.lkReq.payload.tId(log2Up(sysConf.nLtPart)-1 downto 0), sysConf.nLtPart)
  (ltAry, lkReq2LtDemux).zipped.map(_.io.lkReq <-/< _) // pipelined and avoid the high fanout

  // arb the lkResp
  val lkRespArb = StreamArbiterFactory.roundRobin.build(LkResp(sysConf, log2Up(sysConf.nLtPart)), sysConf.nLtPart)
  (lkRespArb.io.inputs, ltAry).zipped.map(_ <-/< _.io.lkResp)

  io.lkResp.arbitrationFrom(lkRespArb.io.output)
  for ((name, elem) <- io.lkResp.payload.elements){
    if (name != "tId") elem := lkRespArb.io.output.payload.find(name)
  }
  io.lkResp.tId := (lkRespArb.io.output.tId ## lkRespArb.io.chosen).asUInt.resized
}
