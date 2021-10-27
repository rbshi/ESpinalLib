package tm

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite

class LockTableTest extends AnyFunSuite {

  val LTConfig = LockTableConfig(8, 32, 8, 10, 10, 4) // txnIDWidth, unitAddrWidth, htBucketWidth, htTableWidth, llTableWidth, queueCntWidth

  def lock_get(dut: LockTable): Unit = {
    dut.clockDomain.forkStimulus(period = 10)

    val prepCmd = fork {
      var reqQueue = scala.collection.mutable.Queue.empty[(Int, Int, Boolean, Boolean)]

      for ( k <- 1 to 10) {
        reqQueue += ((k, k, false, false))
      }
      sendReq()

      def sendReq(): Unit = {
        while(reqQueue.nonEmpty){
          val (txn_id, lock_addr, lock_type, lock_release) = reqQueue.front
          dut.io.lock_req.valid #= true
          dut.io.lock_req.txn_id #= txn_id
          dut.io.lock_req.lock_addr #= lock_addr
          dut.io.lock_req.lock_type #= lock_type
          dut.io.lock_req.lock_release #= lock_release

          dut.clockDomain.waitSampling()

          if (dut.io.lock_req.valid.toBoolean & dut.io.lock_req.ready.toBoolean){
            println("[SEND] req\t" + txn_id)
            reqQueue.dequeue()
          }
        }
        dut.io.lock_req.valid #= false
      }
    }


    val recRes = fork{
      dut.io.lock_resp.ready #= true
      while (true){
        dut.ht.io.printCmd()
        dut.ht.io.printResp()
        dut.ll_owner.io.printCmd()
        dut.ll_owner.io.printResp()
        dut.clockDomain.waitSampling()
        if (dut.io.lock_resp.valid.toBoolean && dut.io.lock_resp.ready.toBoolean) {
          println("[RECE] resp\t" + dut.io.lock_resp.txn_id.toInt)
        }
      }
    }

    prepCmd.join()
  }

  test("lock_get") {
    SimConfig.withWave.compile {
      val dut = new LockTable(LTConfig)
      dut.ll_owner.io.simPublic()
      dut.ht.io.simPublic()
      dut
    }.doSim("test", 99)(lock_get)
  }

}