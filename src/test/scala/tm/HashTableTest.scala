package tm

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import scala.util.Random
import scala.math._

class HashTableTest extends AnyFunSuite {

  def insert_search_delete(dut: HashTableDUT): Unit = {
    dut.clockDomain.forkStimulus(period = 10)

    dut.io.ht_clear_ram_run #= false
    dut.io.dt_clear_ram_run #= false


    var simHT = scala.collection.mutable.Map.empty[Int, Int]
    val prepCmd = fork {
      // init hash table for sim
      val r = new Random()
      val sizeHT = 128
      for (a <- 1 to sizeHT) {
        simHT += r.nextInt(pow(2, 32).toInt) -> a
      }

      //
      var cmdQueue = scala.collection.mutable.Queue.empty[(SpinalEnumElement[HashTableOpCode.type], Int, Int)] // opcode, key, value
      for ((k, v) <- simHT) {
        cmdQueue += ((HashTableOpCode.ins, k, v)) // insert
      }
      sendCmd()

      for ((k, v) <- simHT) {
        cmdQueue += ((HashTableOpCode.sea, k, v)) // search
      }
      sendCmd()

      for ((k, v) <- simHT) {
        if (k % 4 == 0) {
          cmdQueue += ((HashTableOpCode.del, k, v)) // delete
        }
      }
      sendCmd()

      for ((k, v) <- simHT) {
        cmdQueue += ((HashTableOpCode.sea, k, v)) // search, k%16==0 should not exist
      }
      sendCmd()


      def sendCmd(): Unit = {
        while (cmdQueue.nonEmpty) {
          val (opcode, key, value) = cmdQueue.front
          dut.io.ht_cmd_if.valid #= true
          dut.io.ht_cmd_if.opcode #= opcode
          dut.io.ht_cmd_if.key #= key
          dut.io.ht_cmd_if.value #= value
          dut.clockDomain.waitSampling()
          if (dut.io.ht_cmd_if.valid.toBoolean && dut.io.ht_cmd_if.ready.toBoolean) {
            println("[SEND] opcode:" + opcode + "\tkey:" + key + "\tvalue:" + value)
            cmdQueue.dequeue()
          }
        }
        dut.io.ht_cmd_if.valid #= false
      }
    }

    val recRes = fork{
      dut.io.ht_res_if.ready #= true
      while (true){
        dut.clockDomain.waitSampling()
        if (dut.io.ht_res_if.valid.toBoolean) {
          println("[RECE] key:" + dut.io.ht_res_if.key.toBigInt + "\tvalue:" + dut.io.ht_res_if.value.toBigInt + "\tresponse:" + dut.io.ht_res_if.rescode.toBigInt + "\tfound_val:" + dut.io.ht_res_if.found_value.toBigInt)
          // compare if search found
          if (dut.io.ht_res_if.rescode.toBigInt == 0){
            assert(dut.io.ht_res_if.found_value.toBigInt == simHT(dut.io.ht_res_if.key.toBigInt.toInt))
          }
          //
          if (dut.io.ht_res_if.rescode.toBigInt == 1){
            assert(dut.io.ht_res_if.key.toBigInt%4==0)
          }
        }
      }
    }

    prepCmd.join()
  }

  test("insert_search_delete") {
    SimConfig.compile(new HashTableDUT(32, 16, 8, 10)).doSim("test", 99)(insert_search_delete)
  }
}