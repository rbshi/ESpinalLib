// This test can work in local, but hang in CI, so depcrated.


package tm

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4CrossbarFactory}
import util.{AxiMemorySim, AxiMemorySimConfig}

import scala.collection._
import scala.util.Random
import scala.math._
import util._
import spinal.lib.{master, slave}
import spinal.sim.SimThread

import scala.util.control.Breaks._

import scala.collection.mutable.ArrayBuffer


// top level: TxnManGrp <> LockTableCh
class TxnManGrpTop(conf: LockTableConfig, numTxnMan: Int) extends Component{

  val axiConfig = Axi4Config(
    addressWidth = 64,
    dataWidth    = 64,
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
    val axi = master(Axi4(axiConfig))
    val op_req = Vec(slave Stream OpReq(conf), numTxnMan)
    val op_resp = Vec(master Stream OpResp(conf), numTxnMan)
    val sig_txn_abort = Vec(out Bool(), numTxnMan)
    val sig_txn_end = Vec(out Bool(), numTxnMan)
  }

  val txnManGrp = new TxnManGrp(conf, numTxnMan, axiConfig)
  val lt = new LockTableCh(conf, numTxnMan)

  io.axi <> txnManGrp.io.axi
  io.op_req <> txnManGrp.io.op_req
  io.op_resp <> txnManGrp.io.op_resp

  txnManGrp.io.lt_req <> lt.io.lock_req
  txnManGrp.io.lt_resp <> lt.io.lock_resp

  txnManGrp.io.sig_txn_abort <> io.sig_txn_abort
  txnManGrp.io.sig_txn_end <> io.sig_txn_end

}


class TxnManGrpTest extends AnyFunSuite {

  val LTConfig = LockTableConfig(8, 64, 8, 10, 10, 8) // txnIDWidth, unitAddrWidth, htBucketWidth, htTableWidth, llTableWidth, queueCntWidth

  def sendReq(dut: TxnManGrpTop, opIdx: Int, opReq: OpReqSim): Unit ={
    dut.io.op_req(opIdx).addr #= opReq.addr
    dut.io.op_req(opIdx).data #= opReq.data
    dut.io.op_req(opIdx).mode #= opReq.mode
    dut.io.op_req(opIdx).upgrade #= opReq.upgrade
    dut.io.op_req(opIdx).txn_sig #= opReq.txn_sig
    dut.io.op_req(opIdx).valid #= true

    dut.clockDomain.waitSamplingWhere(dut.io.op_req(opIdx).valid.toBoolean && dut.io.op_req(opIdx).ready.toBoolean)
//    opReq.txn_sig.toInt match{
//      case 0 => println(s"[Op:$opIdx\tSend] Addr: ${opReq.addr}\t Mode: ${opReq.mode}")
//      case 1 => println(s"[Op:$opIdx\tTxnStart]")
//      case 2 => println(s"[Op:$opIdx\tTxnEnd]")
//    }
    dut.io.op_req(opIdx).valid #= false
  }

  def recResp(dut: TxnManGrpTop, opIdx: Int): Unit ={
    dut.io.op_resp(opIdx).ready #= true
    dut.clockDomain.waitSamplingWhere(dut.io.op_resp(opIdx).valid.toBoolean && dut.io.op_resp(opIdx).ready.toBoolean)
    println(s"[Op:$opIdx\tResp] Data: ${dut.io.op_resp(opIdx).data.toBigInt}")
  }

  def axiMonitor(dut: TxnManGrpTop): Unit = {
    fork{while(true){
      dut.clockDomain.waitSamplingWhere(dut.io.axi.readCmd.valid.toBoolean && dut.io.axi.readCmd.ready.toBoolean)
      println(s"[AXI RdCmd]: ReadAddr: ${dut.io.axi.readCmd.addr.toBigInt}")}}

    fork{while(true){
      dut.clockDomain.waitSamplingWhere(dut.io.axi.readRsp.valid.toBoolean && dut.io.axi.readRsp.ready.toBoolean)
      println(s"[AXI RdResp]: ReadData: ${dut.io.axi.readRsp.data.toBigInt}")}}

    fork{while(true){
      dut.clockDomain.waitSamplingWhere(dut.io.axi.writeCmd.valid.toBoolean && dut.io.axi.writeCmd.ready.toBoolean)
      println(s"[AXI WrCmd]: WrAddr: ${dut.io.axi.writeCmd.addr.toBigInt}")}}

    fork{while(true){
      dut.clockDomain.waitSamplingWhere(dut.io.axi.writeData.valid.toBoolean && dut.io.axi.writeData.ready.toBoolean)
      println(s"[AXI WrData]: WrData: ${dut.io.axi.writeData.data.toBigInt}")}}
  }

  def ltMonitor(dut: TxnManTop): Unit = {
    fork{while (true){
      dut.clockDomain.waitSamplingWhere(dut.lt.io.lock_req.valid.toBoolean && dut.lt.io.lock_req.ready.toBoolean)
      println(s"[Lock Req]: addr: ${dut.lt.io.lock_req.lock_addr.toBigInt}\t type: ${dut.lt.io.lock_req.lock_type.toBoolean}\t upgrade: ${dut.lt.io.lock_req.lock_upgrade.toBoolean}\t release: ${dut.lt.io.lock_req.lock_release.toBoolean}\t")
    }}
    fork {while (true) {
      dut.clockDomain.waitSamplingWhere(dut.lt.io.lock_resp.valid.toBoolean && dut.lt.io.lock_resp.ready.toBoolean)
      println(s"[Lock Resp]: addr: ${dut.lt.io.lock_resp.lock_addr.toBigInt}\t type: ${dut.lt.io.lock_resp.lock_type.toBoolean}\t upgrade: ${dut.lt.io.lock_resp.lock_upgrade.toBoolean}\t resp: ${dut.lt.io.lock_resp.resp_type.toBigInt}\t")
    }}
  }


  case class OpReqSim(addr:BigInt, data:BigInt, mode:Boolean, upgrade: Boolean, txn_sig:BigInt)


  def multi_op(dut: TxnManGrpTop): Unit ={
    dut.clockDomain.forkStimulus(period = 10)
    // an axi simulation model
    val axi_mem = AxiMemorySim(dut.io.axi, dut.clockDomain, AxiMemorySimConfig(
      maxOutstandingReads=128,
      maxOutstandingWrites=128,
      readResponseDelay=10,
      writeResponseDelay=10,
      useCustom = true
    ))

    axi_mem.start()
    // init data in axi mem
    val mem_init = Array.fill[Byte](1024)(0.toByte)
    axi_mem.memory.writeArray(0, mem_init)

    // init some signal
    dut.io.op_req(0).valid #= false
    dut.io.op_req(1).valid #= false

    // init txn array, each element in array is a txn with multi r/w operations
    var arrayTxn = new ArrayBuffer[mutable.ListBuffer[OpReqSim]]()
    for (i <- 0 until 2){

      var reqQueue = new mutable.ListBuffer[OpReqSim]()
      for (k <- 0 until 4){
        reqQueue += OpReqSim(k, 0, false, false, 0) //read
        reqQueue += OpReqSim(4+k, 0, true, false, 0) //write
      }
      arrayTxn += reqQueue
    }


    def sendTxn(txn : mutable.ListBuffer[OpReqSim], i : Int): Boolean ={
      sendReq(dut, i, OpReqSim(0, 0, false, false, 1)) // txn_start

      breakable{ for (ii <- txn.indices){
        if(dut.io.sig_txn_abort(i).toBoolean) {
          sendReq(dut, i, OpReqSim(0, 0, false, false, 2)) // txn_end
          break()
        } else
          sendReq(dut, i, txn(ii))
      }}
      if(!dut.io.sig_txn_abort(i).toBoolean){
        sendReq(dut, i, OpReqSim(0, 0, false, false, 2)) // txn_end
      }
      dut.clockDomain.waitSamplingWhere(dut.io.sig_txn_end(i).toBoolean) // wait for sig_txn_end
      !dut.io.sig_txn_abort(i).toBoolean // ret true if success
    }


    var arraySend = new ArrayBuffer[SimThread]()
    for (i <- 0 until 2) {
      arraySend += fork {
        var iTxn = 0
        dut.clockDomain.waitSampling(10)
        while (iTxn < 1){
          if(sendTxn(arrayTxn(iTxn * 2 + i), i)){
            println(s"[Op: $i] Txn: ${iTxn * 2 + i} success!")
            iTxn += 1
          } else{
            println(s"[Op: $i] Txn: ${iTxn * 2 + i} aborted!")
            dut.clockDomain.waitSampling(10) // wait for cycles if txn is aborted
          }
        }
//        dut.clockDomain.waitSampling(1000)
      }
    }



//    for (i <- 0 until 2) {
//      fork {
//        while (true)
//          recResp(dut, i)
//      }
//    }

    for (sendTh <- arraySend){
      sendTh.join()
    }

  }


  test("multi_op") {
    SimConfig.withWave.compile {
      val dut = new TxnManGrpTop(LTConfig, 2)
      dut
    }.doSim("multi_op", 99)(multi_op)
  }

}