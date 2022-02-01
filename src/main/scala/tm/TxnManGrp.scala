package tm

import mylib.EciPChaseMT.numPE
import spinal.core.{UInt, _}
import spinal.core
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm.StateMachine

import scala.language.postfixOps
import scala.collection.mutable.ArrayBuffer


class TxnManGrp(conf: LockTableConfig, numTxnMan: Int, outAxiConf: Axi4Config, grpID: Int=0) extends Component {


  val io = new Bundle{
    // to operator
    val op_req = Vec(slave Stream OpReq(conf), numTxnMan)
    val op_resp = Vec(master Stream OpResp(conf), numTxnMan)
    // to lock table
    val lt_req = master Stream LockReq(conf)
    val lt_resp = slave Stream LockResp(conf)
    // out axi
    val axi = master(Axi4(outAxiConf))

    val sig_txn_abort = Vec(out Bool(), numTxnMan)
    val sig_txn_end = Vec(out Bool(), numTxnMan)
  }

  val arrayTxnMan = ArrayBuffer[TxnMan]()
  for (i <- 0 until numTxnMan){
    arrayTxnMan += new TxnMan(conf, outAxiConf.copy(idWidth = outAxiConf.idWidth - log2Up(numTxnMan)), i + grpID * numTxnMan) // i: txnManID
  }

  val axiRdArb = Axi4ReadOnlyArbiter(outAxiConf, numTxnMan)
  val axiWrArb = Axi4WriteOnlyArbiter(outAxiConf, numTxnMan, 1) // 1: routeBufferSize Specify how many write cmd could be schedule before any write data transaction is transmitted
  axiWrArb.io.noCombLoopCheck // FIXME
  axiRdArb.io.noCombLoopCheck // FIXME


  // arbiter lt_req
  val lockReqArb = StreamArbiterFactory.roundRobin.noLock.build(LockReq(conf), numTxnMan) // roundRobin will hangup...
  for (i <- 0 until numTxnMan){
    arrayTxnMan(i).io.lt_req >> lockReqArb.io.inputs(i)
    arrayTxnMan(i).io.lt_req.noCombLoopCheck // FIXME
  }
  // timing
  lockReqArb.io.output >/-> io.lt_req


  // dispatch lt_resp
  io.lt_resp >> arrayTxnMan(0).io.lt_resp // default
  for (i <- 0 until numTxnMan){
    // low bits interleave
    when(io.lt_resp.txn_id(log2Up(numTxnMan)-1 downto 0) === i){
      io.lt_resp >> arrayTxnMan(i).io.lt_resp
    } otherwise {
      arrayTxnMan(i).io.lt_resp.setDefault()
      arrayTxnMan(i).io.lt_resp.valid := False
    }
  }


  for (i <- 0 until numTxnMan){
    arrayTxnMan(i).io.op_req <> io.op_req(i)
    arrayTxnMan(i).io.op_resp <> io.op_resp(i)

    arrayTxnMan(i).io.sig_txn_abort <> io.sig_txn_abort(i)
    arrayTxnMan(i).io.sig_txn_end <> io.sig_txn_end(i)

    // separate RdArb/WrArb, it works.
    arrayTxnMan(i).io.axi.ar <> axiRdArb.io.inputs(i).ar
    arrayTxnMan(i).io.axi.r <> axiRdArb.io.inputs(i).r
    arrayTxnMan(i).io.axi.aw <> axiWrArb.io.inputs(i).aw
    arrayTxnMan(i).io.axi.w <> axiWrArb.io.inputs(i).w
    arrayTxnMan(i).io.axi.b <> axiWrArb.io.inputs(i).b
  }

  // pipe the axi interface
  io.axi.ar <-/< axiRdArb.io.output.ar
  io.axi.r >/-> axiRdArb.io.output.r
  io.axi.aw <-/< axiWrArb.io.output.aw
  io.axi.w <-/< axiWrArb.io.output.w
  io.axi.b >/-> axiWrArb.io.output.b
}
