package tm

import spinal.core.{UInt, _}
import spinal.core.sim._
import spinal.lib._
import util.RenameIO

import scala.language.postfixOps
import scala.util.Random
import scala.math._

object LinkedListOpCode extends SpinalEnum {
  val ins, del, deq = newElement()
}

object LinkedListRetCode extends SpinalEnum {
  val ins_success, ins_exist, ins_fail, del_success, del_fail, deq_success, deq_fail = newElement()

}

// io
class LinkedListIO(keyWidth:Int, tableAddrWidth:Int) extends Bundle{
  val clk_i = in Bool()
  val rst_i = in Bool()

  val ll_cmd_if = slave Stream(new Bundle{
    val key = UInt(keyWidth bits)
    val opcode = LinkedListOpCode() // opcode: OP_INSERT, OP_DELETE, OP_DEQUEUE
    val head_ptr = UInt(tableAddrWidth bits)
    val head_ptr_val = Bool()
  })

  val ll_res_if = master Stream(new Bundle{
    // cmd
    val key = UInt(keyWidth bits)
    val opcode = UInt(2 bits)

    val rescode = LinkedListRetCode() // INSERT_SUCCESS, INSERT_SUCCESS_SAME_KEY, INSERT_NOT_SUCCESS_TABLE_IS_FULL, DELETE_SUCCESS, DELETE_NOT_SUCCESS_NO_ENTRY, DEQUEUE_SUCCESS, DEQUEUE_NOT_SUCCESS_NO_ENTRY
    val chain_state = UInt(3 bits) // NO_CHAIN, IN_HEAD, IN_MIDDLE, IN_TAIL, IN_TAIL_NO_MATCH
  })

  val head_table_if = new Bundle{
    val wr_data_ptr = out UInt(tableAddrWidth bits)
    val wr_data_ptr_val = out Bool()
    val wr_en = out Bool()
  }

  val clear_ram_run_i = in Bool()
  val clear_ram_done_o = out Bool()


  def sendCmd(key:UInt, opcode:SpinalEnumElement[LinkedListOpCode.type], head_ptr:UInt, head_ptr_val:Bool): Unit ={
    ll_cmd_if.valid := True
    ll_cmd_if.key := key
    ll_cmd_if.opcode := opcode
    ll_cmd_if.head_ptr := head_ptr
    ll_cmd_if.head_ptr_val := head_ptr_val
  }


}

// parameters of blockbox is in sv package
class linked_list_top(keyWidth:Int, tableAddrWidth:Int) extends BlackBox with RenameIO{

  val io = new LinkedListIO(keyWidth, tableAddrWidth)

  mapCurrentClockDomain(io.clk_i, io.rst_i)

  noIoPrefix()
  addPrePopTask(renameIO)

  addRTLPath("rtl_src/LinkedList/linked_list_pkg.sv")
  addRTLPath("rtl_src/LinkedList/data_table_delete.sv")
  addRTLPath("rtl_src/LinkedList/data_table_insert.sv")
  addRTLPath("rtl_src/LinkedList/data_table_dequeue.sv")
  addRTLPath("rtl_src/LinkedList/empty_ptr_storage.sv")
  addRTLPath("rtl_src/LinkedList/head_table_if.sv")
  addRTLPath("rtl_src/LinkedList/ht_res_if.sv")
  addRTLPath("rtl_src/LinkedList/ht_res_mux.sv")
  addRTLPath("rtl_src/LinkedList/linked_list.sv")
  addRTLPath("rtl_src/LinkedList/rd_data_val_helper.sv")
  addRTLPath("rtl_src/LinkedList/true_dual_port_ram_single_clock.sv")
}

// blackbox needs a wrapper before being tested
class LinkedListDut(keyWidth:Int, tableAddrWidth:Int) extends Component {
  val io = new LinkedListIO(keyWidth, tableAddrWidth)
  val ll = new linked_list_top(keyWidth, tableAddrWidth)
  io.ll_cmd_if <> ll.io.ll_cmd_if
  io.ll_res_if <> ll.io.ll_res_if
  io.head_table_if <> ll.io.head_table_if
  io.clear_ram_done_o <> ll.io.clear_ram_done_o
  // FIXME
  ll.io.clear_ram_run_i := False
}