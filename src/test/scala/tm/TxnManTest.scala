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
      addressWidth = 64,
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
      useLast      = false,
      useLen       = false
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

  val LTConfig = LockTableConfig(8, 64, 8, 10, 10, 4) // txnIDWidth, unitAddrWidth, htBucketWidth, htTableWidth, llTableWidth, queueCntWidth

  def sendReq(dut: TxnManTop, opReq: OpReqSim): Unit ={
    dut.io.op_req.addr #= opReq.addr
    dut.io.op_req.data #= opReq.data
    dut.io.op_req.mode #= opReq.mode
    dut.io.op_req.txn_sig #= opReq.txn_sig
    dut.io.op_req.valid #= true

    dut.clockDomain.waitSamplingWhere(dut.io.op_req.valid.toBoolean && dut.io.op_req.ready.toBoolean)
    opReq.txn_sig.toInt match{
      case 0 => println("[SendReq] addr: " + opReq.addr + "mode: " + opReq.mode)
      case 1 => println("[TxnStart]")
      case 2 => println("[TxnEnd]")
    }
    dut.io.op_req.valid #= false
  }

  def recResp(dut: TxnManTop): Unit ={
    dut.io.op_resp.ready #= true
    dut.clockDomain.waitSamplingWhere(dut.io.op_resp.valid.toBoolean && dut.io.op_resp.ready.toBoolean)
    println(s"Data: ${dut.io.op_resp.data.toBigInt}\t Mode: ${dut.io.op_resp.mode.toBoolean}\t Status: ${dut.io.op_resp.status.toBoolean}")
  }


  case class OpReqSim(addr:BigInt, data:BigInt, mode:Boolean, txn_sig:BigInt)


  def one_operator(dut: TxnManTop): Unit ={
    dut.clockDomain.forkStimulus(period = 10)
    // an axi simulation model
    val axi_mem = AxiMemorySim(dut.io.axi, dut.clockDomain, AxiMemorySimConfig())

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
    }

    val rec = fork {
      while(true){recResp(dut)}
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