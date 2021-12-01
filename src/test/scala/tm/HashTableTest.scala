package tm

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import scala.util.Random
import scala.math._
import scala.collection._

class HashTableTest extends AnyFunSuite {

  def sendCmd(dut: HashTableDUT, cmdQueue: mutable.Queue[(SpinalEnumElement[HashTableOpCode.type], Int, Int)]): Unit = {
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


  def insert_search_delete(dut: HashTableDUT): Unit = {
    dut.clockDomain.forkStimulus(period = 10)

    dut.io.ht_clear_ram_run #= false
    dut.io.dt_clear_ram_run #= false
    dut.io.update_en #= false
    dut.io.update_addr #= 0
    dut.io.update_data #= 0

    var simHT = mutable.Map.empty[Int, Int]

    val prepCmd = fork {
      // init hash table for sim
      val r = new Random()
      val sizeHT = 1024 // 10-bit tableAddr
      for (a <- 1 to sizeHT) {
        simHT += a * 8 -> (a % 256)
      }

      //
      var cmdQueue = mutable.Queue.empty[(SpinalEnumElement[HashTableOpCode.type], Int, Int)] // opcode, key, value
      for ((k, v) <- simHT) {
        cmdQueue += ((HashTableOpCode.ins2, k, v)) // insert
      }
      sendCmd(dut, cmdQueue)

      for ((k, v) <- simHT) {
        cmdQueue += ((HashTableOpCode.ins2, k, 88)) // insert2, change a value, should not be updated, value data will be update via quick path in recRes
      }
      sendCmd(dut, cmdQueue)


      for ((k, v) <- simHT) {
        cmdQueue += ((HashTableOpCode.sea, k, v)) // search
      }
      sendCmd(dut, cmdQueue)

      for ((k, v) <- simHT) {
        if (k % 4 == 0) {
          cmdQueue += ((HashTableOpCode.del, k, v)) // delete
        }
      }
      sendCmd(dut, cmdQueue)

      for ((k, v) <- simHT) {
        cmdQueue += ((HashTableOpCode.sea, k, v)) // search, k%16==0 should not exist
      }
      sendCmd(dut, cmdQueue)

      dut.clockDomain.waitSampling(100)

    }

    val recRes = fork{
      dut.io.ht_res_if.ready #= true
      while (true){
        dut.clockDomain.waitSampling()
        dut.io.update_en #= false
        if (dut.io.ht_res_if.valid.toBoolean) {
          println("[RECE] key:" + dut.io.ht_res_if.key.toBigInt + "\tvalue:" + dut.io.ht_res_if.value.toBigInt + "\tresponse:" + dut.io.ht_res_if.rescode.toBigInt + "\tfound_val:" + dut.io.ht_res_if.found_value.toBigInt)

          // if ins_exist
          if (dut.io.ht_res_if.rescode.toBigInt == 3){
            println(s"[Info] findAddr: ${dut.io.ht_res_if.find_addr.toBigInt}\t ramData: ${dut.io.ht_res_if.ram_data.toBigInt}")
            // do update (key:val:tableAddr:chainBit)
            dut.io.update_data #= (dut.io.ht_res_if.ram_data.toBigInt % 2048 + 88 * 2048 + (dut.io.ht_res_if.key.toBigInt << (11+9)))
            dut.io.update_addr #= dut.io.ht_res_if.find_addr.toBigInt
            dut.io.update_en #= true
          }

          // compare if search found
          if (dut.io.ht_res_if.rescode.toBigInt == 0){
            // assert(dut.io.ht_res_if.found_value.toBigInt == simHT(dut.io.ht_res_if.key.toBigInt.toInt))
            assert(dut.io.ht_res_if.found_value.toBigInt == 88)
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
    SimConfig.withWave.compile(new HashTableDUT(64, 9, 8, 10)).doSim("test", 99)(insert_search_delete)
  }
}