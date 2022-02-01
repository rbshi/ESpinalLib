package tm

import spinal.core.{UInt, _}
import spinal.core
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axilite._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import util.RenameIO

// numTxnMan, numLT is for per channel;
case class OpTopMulti(numTxnMan: Int, numLT: Int, numCh: Int) extends Component with RenameIO {

  // txnIDWidth, unitAddrWidth, htBucketWidth, htTableWidth, llTableWidth, queueCntWidth, key2AddrShift
  // unitAddrWidth (of each channel): log2Up(8GB/64B(per tuple))
  val ltConf = LockTableConfig(8, 22, 6, 9, 4, 4, 6)
  val txnManConf = LockTableConfig(8, ltConf.unitAddrWidth+log2Up(numCh), 6, 9, 4, 4, 6)

  val axiConfig = Axi4Config(
    addressWidth = 64,
    dataWidth    = 512,
    idWidth = 6,
    useStrb = false,
    useBurst = false,
    useId = true,
    useLock      = false,
    useRegion    = false,
    useCache     = false,
    useProt      = false,
    useQos       = false,
    useLen       = true
  )

  val io = new Bundle{
    val m_axi = Vec(master(Axi4(axiConfig)), numCh)
    val req_axi = Vec(master(Axi4(axiConfig)), numCh)
    val s_axi_control = slave(AxiLite4(AxiLite4Config(12, 32)))
  }

  for (iCh <- 0 until numCh) {
    io.req_axi(iCh).aw.addr := 0
    io.req_axi(iCh).aw.valid := False
    io.req_axi(iCh).aw.id := 0
    io.req_axi(iCh).aw.size := log2Up(512 / 8)
    io.req_axi(iCh).aw.len := 0
    io.req_axi(iCh).w.valid := False
    io.req_axi(iCh).w.data := 0
    io.req_axi(iCh).w.last := True // bug?
    io.req_axi(iCh).b.ready := True
  }


  // control reg: vitis_kernel interface
  val ctlReg = new AxiLite4SlaveFactory(io.s_axi_control)
  val ap_start = ctlReg.createReadAndWrite(Bool(), 0, 0).init(False)
  val ap_done = ctlReg.createReadOnly(Bool(), 0, 1).init(False)
  val ap_idle = ctlReg.createReadOnly(Bool(), 0, 2).init(True)
  ctlReg.onRead(0)(ap_done:=False)

  // control reg: custom
  val txnLen = ctlReg.createReadAndWrite(UInt(8 bits), 0x10, 0)
  val txnCnt = ctlReg.createReadAndWrite(UInt(16 bits), 0x14, 0)

  val addrOffset = Vec(Reg(UInt(32 bits)), numTxnMan * numCh) // fixme: 4GB only
  val txnExeCnt = Vec(UInt(16 bits), numTxnMan * numCh)
  val txnAbortCnt = Vec(UInt(16 bits), numTxnMan * numCh)

  for (i <- 0 until numTxnMan * numCh){
    ctlReg.readAndWrite(addrOffset(i), 24 + 4 * i, 0)
    ctlReg.readAndWrite(txnExeCnt(i), 24 + (4*numTxnMan*numCh) + 4 * i, 0)
    ctlReg.readAndWrite(txnAbortCnt(i), 24 + (8*numTxnMan*numCh) + 4 * i, 0)
  }

  val clkCnt = ctlReg.createReadAndWrite(UInt(32 bits), 24 + (12*numTxnMan*numCh), 0).init(0)

  // instantiate TxnManGrps
  val txnManGrp = ArrayBuffer[TxnManGrp]()
  for (iCh <- 0 until numCh){
    txnManGrp += new TxnManGrp(txnManConf, numTxnMan, axiConfig, iCh)
  }

  // instantiate LockTable of one memory channel
  val lt = Array.fill(numCh)(new LockTableCh(ltConf, numLT))

  // add opStream for each TxnMan
  val opGrp = ArrayBuffer[OpStream]()
  for (_ <- 0 until numTxnMan*numCh){
    opGrp += new OpStream(txnManConf, axiConfig.copy(idWidth = axiConfig.idWidth - log2Up(numTxnMan)))
  }

  // txnMan <> LT
  // interconnect lt_req/lt_resp

  val ltChReqArb = Array.fill(numCh)(StreamArbiterFactory.roundRobin.noLock.build(LockReq(ltConf), numCh))
  val ltChRespArb = Array.fill(numCh)(StreamArbiterFactory.roundRobin.noLock.build(LockResp(txnManConf), numCh))

  // lt_req

  for (iTxnManGrp <- 0 until numCh){
    // use MSB of txnMan lock_addr to select the LTCh
    val ltReq = StreamDemux(txnManGrp(iTxnManGrp).io.lt_req, txnManGrp(iTxnManGrp).io.lt_req.lock_addr(txnManConf.unitAddrWidth-1 downto ltConf.unitAddrWidth).resized, numCh)

    (ltChReqArb.map(_.io.inputs(iTxnManGrp).lock_addr) , ltReq.map(_.lock_addr(ltConf.unitAddrWidth-1 downto 0))).zipped.map(_ <> _)
    (ltChReqArb.map(_.io.inputs(iTxnManGrp).txn_id), ltReq.map(_.txn_id)).zipped.map(_ <> _)
    (ltChReqArb.map(_.io.inputs(iTxnManGrp).lock_type), ltReq.map(_.lock_type)).zipped.map(_ <> _)
    (ltChReqArb.map(_.io.inputs(iTxnManGrp).lock_upgrade), ltReq.map(_.lock_upgrade)).zipped.map(_ <> _)
    (ltChReqArb.map(_.io.inputs(iTxnManGrp).lock_release), ltReq.map(_.lock_release)).zipped.map(_ <> _)
    (ltChReqArb.map(_.io.inputs(iTxnManGrp).lock_idx), ltReq.map(_.lock_idx)).zipped.map(_ <> _)
    (ltChReqArb.map(_.io.inputs(iTxnManGrp).ready), ltReq.map(_.ready)).zipped.map(_ <> _)
    (ltChReqArb.map(_.io.inputs(iTxnManGrp).valid), ltReq.map(_.valid)).zipped.map(_ <> _)

  }

  for (iLTCh <- 0 until numCh) {
    ltChReqArb(iLTCh).io.output >/-> lt(iLTCh).io.lock_req // buggy if with fully pipe
  }



  // lt_resp
  for (iLTCh <- 0 until numCh) {

    // use MSB of txn_id to dispatch the lock_resp
    val ltResp = StreamDemux(lt(iLTCh).io.lock_resp, lt(iLTCh).io.lock_resp.txn_id(txnManConf.txnIDWidth-1 downto log2Up(numTxnMan)).resized, numCh)

    val addrHighBit = UInt(log2Up(numCh) bits)
    addrHighBit := iLTCh

    (ltChRespArb.map(_.io.inputs(iLTCh).lock_addr), ltResp.map(addrHighBit ## _.lock_addr)).zipped.map(_ <> _.asUInt)
    (ltChRespArb.map(_.io.inputs(iLTCh).txn_id), ltResp.map(_.txn_id)).zipped.map(_ <> _)
    (ltChRespArb.map(_.io.inputs(iLTCh).resp_type), ltResp.map(_.resp_type)).zipped.map(_ <> _)
    (ltChRespArb.map(_.io.inputs(iLTCh).lock_type), ltResp.map(_.lock_type)).zipped.map(_ <> _)
    (ltChRespArb.map(_.io.inputs(iLTCh).lock_upgrade), ltResp.map(_.lock_upgrade)).zipped.map(_ <> _)
    (ltChRespArb.map(_.io.inputs(iLTCh).lock_idx), ltResp.map(_.lock_idx)).zipped.map(_ <> _)
    (ltChRespArb.map(_.io.inputs(iLTCh).ready), ltResp.map(_.ready)).zipped.map(_ <> _)
    (ltChRespArb.map(_.io.inputs(iLTCh).valid), ltResp.map(_.valid)).zipped.map(_ <> _)

  }

  for (iTxnManGrp <- 0 until numCh){
    ltChRespArb(iTxnManGrp).io.output >/-> txnManGrp(iTxnManGrp).io.lt_resp // buggy if with fully pipe
  }


  // txnMan <> Op
  for (iCh <- 0 until numCh){
    (txnManGrp(iCh).io.op_req, opGrp.slice(iCh*numTxnMan, (iCh+1)*numTxnMan).map(_.io.op_req)).zipped.map(_ << _)
    (txnManGrp(iCh).io.op_resp, opGrp.slice(iCh*numTxnMan, (iCh+1)*numTxnMan).map(_.io.op_resp)).zipped.map(_ >> _)
    (txnManGrp(iCh).io.sig_txn_abort, opGrp.slice(iCh*numTxnMan, (iCh+1)*numTxnMan).map(_.io.sig_txn_abort)).zipped.map(_ <> _)
    (txnManGrp(iCh).io.sig_txn_end, opGrp.slice(iCh*numTxnMan, (iCh+1)*numTxnMan).map(_.io.sig_txn_end)).zipped.map(_ <> _)
  }

  // ctlReg <> Op
  opGrp.foreach(_.io.txn_len := txnLen)
  opGrp.foreach(_.io.txn_cnt := txnCnt)
  (addrOffset, opGrp.map(_.io.addr_offset)).zipped.map(_.resize(axiConfig.addressWidth) <> _)
  (txnExeCnt, opGrp.map(_.io.txn_exe_cnt)).zipped.map(_ <> _)
  (txnAbortCnt, opGrp.map(_.io.txn_abort_cnt)).zipped.map(_ <> _)


  opGrp.foreach(_.io.start := ap_start)
  val reduced_opdone = opGrp.map(_.io.done).foldLeft(True)(_ && _) // && reduction: start with True
  val runState = Reg(Bool())

  when(ap_start){
    ap_start.clear()
    ap_idle.clear()
    runState.set()
    // clear clkCnt
    clkCnt := 0
  }

  when(~ap_idle)(clkCnt := clkCnt + 1)

  runState.clearWhen(runState && reduced_opdone)
  ap_done.setWhen(runState && reduced_opdone)
  ap_idle.setWhen(runState && reduced_opdone)

  for (iCh <- 0 until numCh){
    io.m_axi(iCh) <> txnManGrp(iCh).io.axi

    // req_axi <> Op axi via arbiter
    val axiRdArb = Axi4ReadOnlyArbiter(axiConfig, numTxnMan)
    // pipe the axi interface
    axiRdArb.io.output.ar >/->  io.req_axi(iCh).ar
    axiRdArb.io.output.r <-/< io.req_axi(iCh).r
//    (axiRdArb.io.inputs, opGrp.slice(iCh*numTxnMan, (iCh+1)*numTxnMan).map(_.io.axi)).zipped.map(_ <> _)
    (axiRdArb.io.inputs.map(_.ar), opGrp.slice(iCh*numTxnMan, (iCh+1)*numTxnMan).map(_.io.axi.ar)).zipped.map(_ <-/< _)
    (axiRdArb.io.inputs.map(_.r), opGrp.slice(iCh*numTxnMan, (iCh+1)*numTxnMan).map(_.io.axi.r)).zipped.map(_ >/-> _)

  }

}


object OpTopMultiMain {
  def main(args: Array[String]): Unit = {
    val numTxnMan = 4
    val numLT = 16
    val numCh = 4

    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = LOW), targetDirectory = "rtl").generateVerilog{
      val top = OpTopMulti(numTxnMan, numLT, numCh)
      top.renameIO()
      top.setDefinitionName("tmop_multi")
      top
    }

    val txt1 =
      """  set bifparam [::ipx::add_bus_parameter -quiet "MAX_BURST_LENGTH" $bif]
        |  set_property value        64           $bifparam
        |  set_property value_source constant     $bifparam
        |  set bifparam [::ipx::add_bus_parameter -quiet "NUM_READ_OUTSTANDING" $bif]
        |  set_property value        32           $bifparam
        |  set_property value_source constant     $bifparam
        |  set bifparam [::ipx::add_bus_parameter -quiet "NUM_WRITE_OUTSTANDING" $bif]
        |  set_property value        32           $bifparam
        |  set_property value_source constant     $bifparam""".stripMargin

    println("=======.tcl============================")

    for (ii <- 0 until numCh){
      println(s"""  set bif      [::ipx::get_bus_interfaces -of $$core  "m_axi_$ii"] """)
      println(txt1)
    }

    for (ii <- 0 until numCh){
      println(s"""  set bif      [::ipx::get_bus_interfaces -of $$core  "req_axi_$ii"] """)
      println(txt1)
    }



    for (ii <- 0 until numCh){
      println(s"""  ::ipx::associate_bus_interfaces -busif "m_axi_$ii" -clock "ap_clk" $$core """)
      println(s"""  ::ipx::associate_bus_interfaces -busif "req_axi_$ii" -clock "ap_clk" $$core """)
    }

    println("=======.tcl============================")

    for (iPE <- 0 until numTxnMan*numCh){
      println(s"""  set reg      [::ipx::add_register -quiet "addrOffset$iPE" $$addr_block]""")
      println(s"""  set_property address_offset 0x${(24+4*iPE).toHexString} $$reg""")
      println(s"""  set_property size           [expr {4*8}]   $$reg \n""")
    }

    for (iPE <- 0 until numTxnMan*numCh){
      println(s"""  set reg      [::ipx::add_register -quiet "txnExeCnt$iPE" $$addr_block]""")
      println(s"""  set_property address_offset 0x${(24+4*(numTxnMan*numCh)+4*iPE).toHexString} $$reg""")
      println(s"""  set_property size           [expr {4*8}]   $$reg \n""")
    }

    for (iPE <- 0 until numTxnMan*numCh){
      println(s"""  set reg      [::ipx::add_register -quiet "txnAbortCnt$iPE" $$addr_block]""")
      println(s"""  set_property address_offset 0x${(24+8*(numTxnMan*numCh)+4*iPE).toHexString} $$reg""")
      println(s"""  set_property size           [expr {4*8}]   $$reg \n""")
    }

    println(s"""  set reg      [::ipx::add_register -quiet "clkCnt" $$addr_block]""")
    println(s"""  set_property address_offset 0x${(24+12*(numTxnMan*numCh)).toHexString} $$reg""")
    println(s"""  set_property size           [expr {4*8}]   $$reg \n""")


    for (ii <- 0 until numCh){
      println(
        s"""
           |  set reg      [::ipx::add_register -quiet "m_axi_${ii}_ptr0" $$addr_block]
           |  set_property address_offset 0x${(24+12*(numTxnMan*numCh)+4+ii*16).toHexString} $$reg
           |  set_property size           [expr {8*8}]   $$reg
           |  set regparam [::ipx::add_register_parameter -quiet {ASSOCIATED_BUSIF} $$reg]
           |  set_property value m_axi_${ii} $$regparam
           |""".stripMargin)

      println(
        s"""
           |  set reg      [::ipx::add_register -quiet "req_axi_${ii}_ptr0" $$addr_block]
           |  set_property address_offset 0x${(24+12*(numTxnMan*numCh)+4+ii*16+8).toHexString} $$reg
           |  set_property size           [expr {8*8}]   $$reg
           |  set regparam [::ipx::add_register_parameter -quiet {ASSOCIATED_BUSIF} $$reg]
           |  set_property value req_axi_${ii} $$regparam
           |""".stripMargin)
    }
  }
}
