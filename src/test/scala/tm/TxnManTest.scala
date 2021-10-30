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

  val LTConfig = LockTableConfig(8, 32, 8, 10, 10, 4) // txnIDWidth, unitAddrWidth, htBucketWidth, htTableWidth, llTableWidth, queueCntWidth

  def sendReq(dut: TxnManTop, opReq: OpReqSim): Unit ={
    dut.io.op_req.addr #= opReq.addr
    dut.io.op_req.data #= opReq.data
    dut.io.op_req.mode #= opReq.mode
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

  def axiMonitorRdCmd(dut: TxnManTop): Unit = {
    dut.clockDomain.waitSamplingWhere(dut.io.axi.readCmd.valid.toBoolean && dut.io.axi.readCmd.ready.toBoolean)
    println(s"[AXI RdCmd]: ReadAddr: ${dut.io.axi.readCmd.addr.toBigInt}")
  }

  def axiMonitorRdResp(dut: TxnManTop): Unit = {
    dut.clockDomain.waitSamplingWhere(dut.io.axi.readRsp.valid.toBoolean && dut.io.axi.readRsp.ready.toBoolean)
    println(s"[AXI RdResp]: ReadData: ${dut.io.axi.readRsp.data.toBigInt}")
  }

  case class OpReqSim(addr:BigInt, data:BigInt, mode:Boolean, txn_sig:BigInt)


  def one_operator(dut: TxnManTop): Unit ={
    dut.clockDomain.forkStimulus(period = 10)
    // an axi simulation model
    val axi_mem = AxiMemorySim(dut.io.axi, dut.clockDomain, AxiMemorySimConfig())
    axi_mem.start()
    // init data in axi mem
    val mem_init = Array.fill[Byte](1024)(255.toByte)
    axi_mem.memory.writeArray(0, mem_init)

    // init one txn
    var oneTxn = mutable.Queue.empty[OpReqSim]
    val op_start = OpReqSim(0, 0, mode = false, 1)
    oneTxn.enqueue(op_start)

    for (k <- 0 until 16){
      oneTxn.enqueue(OpReqSim(k, k, false, 0))
    }
    oneTxn.enqueue(OpReqSim(0, 0, false, 2))


    val send = fork {
      while(oneTxn.nonEmpty){
        sendReq(dut, oneTxn.dequeue())
      }
      dut.clockDomain.waitActiveEdge(100)
    }

    val rec = fork {
      while(true){recResp(dut)}
    }

    val axiRdCmd = fork {
      while(true){axiMonitorRdCmd(dut)}
    }

    val axiRdResp = fork {
      while(true){axiMonitorRdResp(dut)}
    }

    send.join()
  }


  test("one_operator") {
    SimConfig.withWave.compile {
      val dut = new TxnManTop(LTConfig)
      dut
    }.doSim("one_operator", 99)(one_operator)
  }

}