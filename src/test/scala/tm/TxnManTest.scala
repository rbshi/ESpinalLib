package tm

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config}

import scala.collection._
import scala.util.Random
import scala.math._
import spinal.lib.bus.amba4.axi.sim._
import spinal.lib.{master, slave}


// a top level must be made for simulation, fixme: axi4 config
class TxnManTop(conf: LockTableConfig) extends Component{
  val txn_man = new TxnMan(conf)
  val lt = new LockTable(conf)

  val io = new Bundle{
    val axi = master(Axi4(Axi4Config(
      addressWidth = 32,
      dataWidth    = 64,
      idWidth = 1,
      useStrb = false,
      useBurst = false,
      useId = true,
      useLock      = false,
      useRegion    = false,
      useCache     = false,
      useProt      = false,
      useQos       = false,
      useLen       = true
    )))
    val op_req = slave Stream(OpReq(conf))
    val op_resp = master Stream(OpResp(conf))
  }
  io.axi <> txn_man.io.axi
  txn_man.io.lt_req <> lt.io.lock_req
  txn_man.io.lt_resp <> lt.io.lock_resp
  txn_man.io.op_req <> io.op_req
  txn_man.io.op_resp <> io.op_resp
}


class TxnManTest extends AnyFunSuite {

  val LTConfig = LockTableConfig(8, 32, 8, 10, 10, 8) // txnIDWidth, unitAddrWidth, htBucketWidth, htTableWidth, llTableWidth, queueCntWidth

  def sendReq(dut: TxnManTop, opReq: OpReqSim): Unit ={
    dut.io.op_req.addr #= opReq.addr
    dut.io.op_req.data #= opReq.data
    dut.io.op_req.mode #= opReq.mode
    dut.io.op_req.upgrade #= opReq.upgrade
    dut.io.op_req.txn_sig #= opReq.txn_sig
    dut.io.op_req.valid #= true

    dut.clockDomain.waitSamplingWhere(dut.io.op_req.valid.toBoolean && dut.io.op_req.ready.toBoolean)
    opReq.txn_sig.toInt match{
      case 0 => println(s"[Send] Addr: ${opReq.addr}\t Mode: ${opReq.mode}")
      case 1 => println("[TxnStart]")
      case 2 => println("[TxnEnd]")
    }
    dut.io.op_req.valid #= false
  }

  def recResp(dut: TxnManTop): Unit ={
    dut.io.op_resp.ready #= true
    dut.clockDomain.waitSamplingWhere(dut.io.op_resp.valid.toBoolean && dut.io.op_resp.ready.toBoolean)
    println(s"[Resp] Data: ${dut.io.op_resp.data.toBigInt}\t Mode: ${dut.io.op_resp.mode.toBoolean}\t Status: ${dut.io.op_resp.status.toBoolean}")
  }

  def axiMonitor(dut: TxnManTop): Unit = {
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


  def one_operator(dut: TxnManTop): Unit ={
    dut.clockDomain.forkStimulus(period = 10)
    // an axi simulation model
    val axi_mem = AxiMemorySim(dut.io.axi, dut.clockDomain, AxiMemorySimConfig(
      maxOutstandingReads=128,
      maxOutstandingWrites=128,
      readResponseDelay=30,
      writeResponseDelay=30,
      useCustom = true
    ))
    axi_mem.start()
    // init data in axi mem
    val mem_init = Array.fill[Byte](1024)(0.toByte)
    axi_mem.memory.writeArray(0, mem_init)

    // init one txn
    var oneTxn = mutable.Queue.empty[OpReqSim]

    oneTxn.enqueue(OpReqSim(0, 0, mode = false, false, 1))  // txn_start

//    val rand = Random
//    for (k <- 0 until 16){
//      oneTxn.enqueue(OpReqSim(k, k, rand.nextBoolean(), false, 0))
//    }

    for (k <- 0 until 4){
      oneTxn.enqueue(OpReqSim(k, k, false, false, 0)) // read,
    }

    for (k <- 0 until 4){
      oneTxn.enqueue(OpReqSim(k, k, true, true, 0)) // write, upgrade
    }

    oneTxn.enqueue(OpReqSim(0, 0, false, false, 2)) // txn_end


    val send = fork {
      while(oneTxn.nonEmpty){
        sendReq(dut, oneTxn.dequeue())
      }
      dut.clockDomain.waitSampling(1000)
    }

    val rec = fork {
      while(true){recResp(dut)}
    }

    val axiMon = fork {axiMonitor(dut)}
    val ltMon = fork {ltMonitor(dut)}

    val waitEnd = fork {
      while (!dut.txn_man.io.txn_end_test.toBoolean)
        dut.clockDomain.waitSampling()
      println("[TxnEnd] cleanup finished")
    }

    send.join()
    waitEnd.join()
  }


  test("one_operator") {
    SimConfig.withWave.compile {
      val dut = new TxnManTop(LTConfig)
      dut.txn_man.io.txn_end_test.simPublic()
      dut.lt.io.simPublic()
      dut
    }.doSim("one_operator", 99)(one_operator)
  }

}