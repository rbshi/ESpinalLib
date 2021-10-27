package tm

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite

class LinkedListTest extends AnyFunSuite {

  def insert_dequeue(dut: LinkedListDut): Unit = {
    dut.clockDomain.forkStimulus(period = 10)
    var simHT = scala.collection.mutable.Map.empty[Int, Int]

    val prepCmd = fork {
      // init hash table for sim
      val sizeHT = 128
      var cmdQueue = scala.collection.mutable.Queue.empty[(SpinalEnumElement[LinkedListOpCode.type], Int, Int, Boolean)] // opcode, key, head_ptr, head_val, head_ptr_val

      for ( k <- 1 to 10) {
        if (k==1) {
          cmdQueue += ((LinkedListOpCode.ins, k, 0, false)) // insert
        } else{
          cmdQueue += ((LinkedListOpCode.ins, k, 0, true)) // insert
        }
      }
      sendCmd()

      for ( k <- 1 to 10) {
        cmdQueue += ((LinkedListOpCode.deq, 0, k-1, true)) // dequeue
      }
      sendCmd()

      def sendCmd(): Unit = {
        while (cmdQueue.nonEmpty) {
          val (opcode, key, head_ptr, head_ptr_val) = cmdQueue.front
          dut.io.ll_cmd_if.valid #= true
          dut.io.ll_cmd_if.opcode #= opcode
          dut.io.ll_cmd_if.key #= key
          dut.io.ll_cmd_if.head_ptr #= head_ptr
          dut.io.ll_cmd_if.head_ptr_val #= head_ptr_val
          dut.clockDomain.waitSampling()
          if (dut.io.ll_cmd_if.valid.toBoolean && dut.io.ll_cmd_if.ready.toBoolean) {
            println("[SEND] opcode:" + opcode + "\tkey:" + key)
            cmdQueue.dequeue()
          }
        }
        dut.io.ll_cmd_if.valid #= false
      }
    }

    val recRes = fork{
      dut.io.ll_res_if.ready #= true
      while (true){
        dut.clockDomain.waitSampling()
        if (dut.io.ll_res_if.valid.toBoolean) {
          println("[RECE] key:" + dut.io.ll_res_if.key.toBigInt + "\tresponse:" + dut.io.ll_res_if.rescode.toBigInt)
        }
      }
    }

    val updateHeadTable = fork{
      while (true){
        dut.clockDomain.waitSampling()
        if (dut.io.head_table_if.wr_en.toBoolean){
          println("[HeadTable] ptr:" + dut.io.head_table_if.wr_data_ptr.toBigInt + "\tptr_val:" + dut.io.head_table_if.wr_en.toBoolean)
        }
      }
    }

    prepCmd.join()
  }

  test("insert_dequeue") {
    SimConfig.withWave.compile(new LinkedListDut(32, 10)).doSim("test", 99)(insert_dequeue)
  }
}