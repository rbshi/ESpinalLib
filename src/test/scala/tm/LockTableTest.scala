package tm

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable._
import scala.util.Random
import scala.math._


class LockTableTest extends AnyFunSuite {

  val LTConfig = LockTableConfig(8, 64, 8, 10, 10, 8) // txnIDWidth, unitAddrWidth, htBucketWidth, htTableWidth, llTableWidth, queueCntWidth

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

  def bulk(dut: LockTable) = {
    dut.clockDomain.forkStimulus(period = 10)
    var lockMap = scala.collection.mutable.Map.empty[BigInt, (Int, Boolean, Boolean)] // lock_addr, txn_id, lock_type, lock_release

    def sendCmd(lock_addr: BigInt, txn_id: Int, lock_type: Boolean, lock_release: Boolean): Unit = {
      dut.io.lock_req.valid #= true
      dut.io.lock_req.lock_addr #= lock_addr
      dut.io.lock_req.txn_id #= txn_id
      dut.io.lock_req.lock_type #= lock_type
      dut.io.lock_req.lock_release #= lock_release
      while (!(dut.io.lock_req.valid.toBoolean && dut.io.lock_req.ready.toBoolean)) {
        dut.clockDomain.waitSampling()
      }
      dut.clockDomain.waitSampling()
      dut.io.lock_req.valid #= false
    }

    val send = fork {
      // init locks
      val r = new Random()
      for ( k <- 0 until 100) {
        val (addr, id, lock_type, lock_release) = (r.nextInt(pow(2, 32).toInt), r.nextInt(pow(2, 8).toInt), r.nextBoolean(), false)
        lockMap += BigInt(addr) -> (id, lock_type, lock_release) // get_lock
        sendCmd(addr, id, lock_type, lock_release)
      }

      // conflict
      for ( k <- 0 until 10) {
        val (addr, _) = lockMap.toSeq(r.nextInt(lockMap.size))
        sendCmd(addr, r.nextInt(pow(2, 8).toInt), r.nextBoolean(), false)
      }

      // release some
      for ( k <- 0 until 10) {
        val (addr, v) = lockMap.toSeq(r.nextInt(lockMap.size))
        sendCmd(addr, v._1, v._2, true) // release
        lockMap.-(addr)
      }

    }


    val rec = fork {
      dut.io.lock_resp.ready #= true
      while (true) {

        dut.clockDomain.waitSampling()

        if (dut.io.lock_resp.valid.toBoolean && dut.io.lock_resp.ready.toBoolean) {
          if (dut.io.lock_resp.resp_type.toBigInt == 1) {
            assert(lockMap.contains(dut.io.lock_resp.lock_addr.toBigInt))
            assert(lockMap(dut.io.lock_resp.lock_addr.toBigInt)._2 || dut.io.lock_resp.lock_type.toBoolean)
            println("[Abort] addr: " + dut.io.lock_resp.lock_addr.toBigInt)
          } // abort
          if (dut.io.lock_resp.resp_type.toBigInt == 0) {
            assert(lockMap.contains(dut.io.lock_resp.lock_addr.toBigInt) || !(lockMap(dut.io.lock_resp.lock_addr.toBigInt)._2 || dut.io.lock_resp.lock_type.toBoolean))
//            assert(lockMap.contains(dut.io.lock_resp.lock_addr.toBigInt))
          }
        }
      }
    }

    send.join()

  }

//  test("lock_then_release") {
//    SimConfig.withWave.compile {
//      val dut = new LockTable(LTConfig)
//      dut.ll_owner.io.simPublic()
//      dut.ht.io.simPublic()
//      dut
//    }.doSim("test", 99)(lock_then_release)
//  }
//
//  test("lock_conflict") {
//    SimConfig.withWave.compile {
//      val dut = new LockTable(LTConfig)
//      dut.ll_owner.io.simPublic()
//      dut.ht.io.simPublic()
//      dut
//    }.doSim("test", 99)(lock_conflict)
//  }
//
//  test("lock_share_conflict") {
//    SimConfig.withWave.compile {
//      val dut = new LockTable(LTConfig)
//      dut.ll_owner.io.simPublic()
//      dut.ht.io.simPublic()
//      dut
//    }.doSim("test", 99)(lock_share_conflict)
//  }

  test("bulk") {
    SimConfig.withWave.compile {
      val dut = new LockTable(LTConfig)
      dut.ht.io.simPublic()
      dut
    }.doSim("test", 99)(bulk)
  }


}