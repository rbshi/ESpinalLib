package tm

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.Queue

class LockTableTest extends AnyFunSuite {

  val LTConfig = LockTableConfig(8, 32, 8, 10, 10, 4) // txnIDWidth, unitAddrWidth, htBucketWidth, htTableWidth, llTableWidth, queueCntWidth

  def sendReq(dut: LockTable, reqQueue: Queue[(Int, Int, Boolean, Boolean)]): Unit = {
    while(reqQueue.nonEmpty){
      val (txn_id, lock_addr, lock_type, lock_release) = reqQueue.front
      dut.io.lock_req.valid #= true
      dut.io.lock_req.txn_id #= txn_id
      dut.io.lock_req.lock_addr #= lock_addr
      dut.io.lock_req.lock_type #= lock_type
      dut.io.lock_req.lock_release #= lock_release

      dut.clockDomain.waitSampling()

      if (dut.io.lock_req.valid.toBoolean & dut.io.lock_req.ready.toBoolean){
        println("======================[SEND] req:" + txn_id + "========================")
        reqQueue.dequeue()
      }
    }
    dut.io.lock_req.valid #= false
  }

  def getResp(dut: LockTable): Unit ={
    dut.io.lock_resp.ready #= true
    while (true){
      //        dut.ht.io.printCmd()
      dut.ht.io.printResp()
      //        dut.ll_owner.io.printCmd()
      dut.ll_owner.io.printResp()

      dut.clockDomain.waitSampling()

      if (dut.io.lock_resp.valid.toBoolean && dut.io.lock_resp.ready.toBoolean) {
        println("[RECE] lock_addr:\t" + dut.io.lock_resp.txn_id.toBigInt + "\tresp_type:" + dut.io.lock_resp.resp_type.toBigInt)
      }
    }
  }

  def lock_then_release(dut: LockTable): Unit = {
    dut.clockDomain.forkStimulus(period = 10)

    val send = fork {
      var reqQueue = scala.collection.mutable.Queue.empty[(Int, Int, Boolean, Boolean)] // txn_id, lock_addr, lock_type, lock_release
      for ( k <- 0 to 9) {
        reqQueue += ((k, k, false, false))
      }
      for ( k <- 0 to 9) {
        reqQueue += ((k, k, false, true))
      }
      sendReq(dut, reqQueue)
    }

    val rec = fork{
      getResp(dut)
    }
    send.join()
  }


  def lock_conflict(dut: LockTable): Unit = {
    dut.clockDomain.forkStimulus(period = 10)

    val send = fork {
      var reqQueue = scala.collection.mutable.Queue.empty[(Int, Int, Boolean, Boolean)] // txn_id, lock_addr, lock_type, lock_release
      for ( k <- 0 to 9) {
        reqQueue += ((k, k, true, false))
      }
      for ( k <- 0 to 19) {
        reqQueue += ((2 * k, k, false, false))
      }
      sendReq(dut, reqQueue)
    }

    val rec = fork{
      getResp(dut)
    }
    send.join()
  }

  def lock_share_conflict(dut: LockTable): Unit = {
    dut.clockDomain.forkStimulus(period = 10)

    val send = fork {
      var reqQueue = scala.collection.mutable.Queue.empty[(Int, Int, Boolean, Boolean)] // txn_id, lock_addr, lock_type, lock_release
      for ( k <- 0 to 3) {
        reqQueue += ((k, k, false, false))
      }
      for ( k <- 0 to 3) {
        reqQueue += ((2 * k, k, false, false))
      }
      for ( k <- 0 to 3) {
        reqQueue += ((3 * k, k, true, false))
      }
      for ( k <- 0 to 3) {
        reqQueue += ((k, k, false, true))
      }
      for ( k <- 0 to 3) {
        reqQueue += ((2 * k, k, false, true))
      }
      for ( k <- 0 to 3) {
        reqQueue += ((3 * k, k, true, false))
      }
      sendReq(dut, reqQueue)
    }

    val rec = fork{
      getResp(dut)
    }
    send.join()
  }


  test("lock_then_release") {
    SimConfig.withWave.compile {
      val dut = new LockTable(LTConfig)
      dut.ll_owner.io.simPublic()
      dut.ht.io.simPublic()
      dut
    }.doSim("test", 99)(lock_then_release)
  }

  test("lock_conflict") {
    SimConfig.withWave.compile {
      val dut = new LockTable(LTConfig)
      dut.ll_owner.io.simPublic()
      dut.ht.io.simPublic()
      dut
    }.doSim("test", 99)(lock_conflict)
  }

  test("lock_share_conflict") {
    SimConfig.withWave.compile {
      val dut = new LockTable(LTConfig)
      dut.ll_owner.io.simPublic()
      dut.ht.io.simPublic()
      dut
    }.doSim("test", 99)(lock_share_conflict)
  }

}