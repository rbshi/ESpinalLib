package tm

import spinal.core.{UInt, _}
import spinal.core.sim._
import spinal.lib._
import scala.language.postfixOps
import scala.util.Random
import scala.math._

object HashTableRetCode extends SpinalEnum {
  val sea_success, sea_fail, ins_success, ins_exist, ins_fail, del_success, del_fail = newElement()
}

object HashTableOpCode extends SpinalEnum {
  val sea, ins, del = newElement()
}

class HashTableIO(keyWidth:Int, valWidth:Int, bucketWidth:Int, tableAddrWidth:Int) extends Bundle{
    val clk_i = in Bool()
    val rst_i = in Bool()

    val ht_cmd_if = new Bundle{
      val valid = in Bool()
      val ready = out Bool()
      val key = in UInt(keyWidth bits)
      val value = in UInt(valWidth bits)
      val opcode = in UInt(2 bits) // opcode: OP_SEARCH, OP_INSERT, OP_DELETE
    }

    val ht_res_if = new Bundle{
      val valid = out Bool()
      val ready = in Bool()
      // cmd
      val key = out UInt(keyWidth bits)
      val value = out UInt(valWidth bits)
      val opcode = out UInt(2 bits)

      val rescode = out UInt(3 bits) // SEARCH_FOUND, SEARCH_NOT_SUCCESS_NO_ENTRY, INSERT_SUCCESS, INSERT_SUCCESS_SAME_KEY, INSERT_NOT_SUCCESS_TABLE_IS_FULL, DELETE_SUCCESS, DELETE_NOT_SUCCESS_NO_ENTRY
      val bucket = out UInt(bucketWidth bits)
      val found_value = out UInt(valWidth bits)
      val chain_state = out UInt(3 bits) // NO_CHAIN, IN_HEAD, IN_MIDDLE, IN_TAIL, IN_TAIL_NO_MATCH
    }
    val ht_clear_ram_run = in Bool() // head table
    val ht_clear_ram_done = out Bool()
    val dt_clear_ram_run = in Bool() // data table
    val dt_clear_ram_done = out Bool()
}

// parameters of blockbox is in sv package FIXME: MUST be modified manually
class hash_table_top(keyWidth:Int, valWidth:Int, bucketWidth:Int, tableAddrWidth:Int) extends BlackBox{

  val io = new HashTableIO(keyWidth, valWidth, bucketWidth, tableAddrWidth)
  mapCurrentClockDomain(io.clk_i, io.rst_i)

  noIoPrefix()

  addRTLPath("rtl_src/HashTable/hash_table_pkg.sv")
  addRTLPath("rtl_src/HashTable/CRC32_D32.sv")
  addRTLPath("rtl_src/HashTable/altera_avalon_st_pipeline_base.v")
  addRTLPath("rtl_src/HashTable/calc_hash.sv")
  addRTLPath("rtl_src/HashTable/data_table.sv")
  addRTLPath("rtl_src/HashTable/data_table_delete.sv")
  addRTLPath("rtl_src/HashTable/data_table_insert.sv")
  addRTLPath("rtl_src/HashTable/data_table_search.sv")
  addRTLPath("rtl_src/HashTable/data_table_search_wrapper.sv")
  addRTLPath("rtl_src/HashTable/empty_ptr_storage.sv")
  addRTLPath("rtl_src/HashTable/hash_table_top.sv")
  addRTLPath("rtl_src/HashTable/head_table.sv")
  addRTLPath("rtl_src/HashTable/head_table_if.sv")
  addRTLPath("rtl_src/HashTable/ht_cmd_if.sv")
  addRTLPath("rtl_src/HashTable/ht_delay.sv")
  addRTLPath("rtl_src/HashTable/ht_res_if.sv")
  addRTLPath("rtl_src/HashTable/ht_res_mux.sv")
  addRTLPath("rtl_src/HashTable/rd_data_val_helper.sv")
  addRTLPath("rtl_src/HashTable/true_dual_port_ram_single_clock.sv")
}

class HashTableDUT(keyWidth:Int, valWidth:Int, bucketWidth:Int, tableAddrWidth:Int) extends Component {
  val io = new HashTableIO(keyWidth, valWidth, bucketWidth, tableAddrWidth)
  val ht = new hash_table_top(keyWidth, valWidth, bucketWidth, tableAddrWidth)
  io.ht_cmd_if <> ht.io.ht_cmd_if
  io.ht_res_if <> ht.io.ht_res_if
  ht.io.ht_clear_ram_run <> io.ht_clear_ram_run
  ht.io.dt_clear_ram_run <> io.dt_clear_ram_run
  ht.io.ht_clear_ram_done <> io.ht_clear_ram_done
  ht.io.dt_clear_ram_done <> io.dt_clear_ram_done
}
