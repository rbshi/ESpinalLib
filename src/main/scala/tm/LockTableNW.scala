package tm

import spinal.core.{UInt, _}
import spinal.core
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm.StateMachine

import scala.language.postfixOps

object LockRespType extends SpinalEnum{
  val grant, abort, waiting, release = newElement()
}


case class LockTableConfig(txnIDWidth:Int, unitAddrWidth:Int, htBucketWidth:Int, htTableWidth:Int, llTableWidth:Int, queueCntWidth:Int){
  // value of ht: lock_status ()
  def htValWidth = 1 + queueCntWidth
}

// value of ht
case class LockEntry(conf: LockTableConfig) extends Bundle{
  val lock_status = Bool() // sh, ex
  val owner_cnt = UInt(conf.queueCntWidth bits)
  def toUInt : UInt = {
    this.asBits.asUInt
  }
}

case class RamEntry(conf: LockTableConfig) extends Bundle{
  val key = UInt(conf.unitAddrWidth bits)
  val lock_status = Bool() // sh, ex
  val owner_cnt = UInt(conf.queueCntWidth bits)
  val next_ptr = UInt(conf.htTableWidth bits)
  val net_ptr_val = Bool()

  def toUInt : UInt = {
    this.asBits.asUInt
  }
}

case class LockReq(conf: LockTableConfig) extends Bundle{
  val txn_id = UInt(conf.txnIDWidth bits)
  val lock_addr = UInt(conf.unitAddrWidth bits)
  val lock_type = Bool() // sh, ex
  val lock_upgrade = Bool() // normal, upgrade
  val lock_release = Bool() // get, release
  val lock_idx = UInt(8 bits) // address index to txn manager (out of order resp)
  //  val txn_ts
  def setDefault() = {
    txn_id := 0
    lock_addr := 0
    lock_type := False
    lock_upgrade := False
    lock_release := False
    lock_idx := 0
  }
}

case class LockResp(conf: LockTableConfig) extends Bundle{
  val txn_id = UInt(conf.txnIDWidth bits)
  val resp_type = LockRespType() // grant, abort, waiting, release
  val lock_addr = UInt(conf.unitAddrWidth bits) // for test
  val lock_type = Bool() // for test
  val lock_upgrade = Bool() // normal, upgrade
  val lock_idx = UInt(8 bits)

  def setDefault() = {
    txn_id := 0
    lock_addr := 0
    lock_type := False
    lock_upgrade := False
    resp_type := LockRespType.abort
    lock_idx := 0
  }
}

class LockTableIO(conf: LockTableConfig) extends Bundle{
  val lock_req = slave Stream(LockReq(conf))
  val lock_resp = master Stream(LockResp(conf))

  def setDefault() = {
    lock_req.ready := False
    lock_resp.valid := False
    lock_resp.setDefault()
  }
}


class LockTable(conf: LockTableConfig) extends Component {

  val io = new LockTableIO(conf)
  // hash table & two linked list inside locktable
  val ht = new HashTableDUT(conf.unitAddrWidth, conf.htValWidth, conf.htBucketWidth, conf.htTableWidth)

  io.setDefault()
  ht.io.setDefault()

  val fsm = new StateMachine {
    // stage lock_req
    val req = RegNextWhen(io.lock_req.payload, io.lock_req.fire)
    // stage ht out
    val ht_lock_entry_cast = LockEntry(conf)
    ht_lock_entry_cast.assignFromBits(ht.io.ht_res_if.found_value.asBits) // wire: cast the value of ht to lock_entry

    val ht_ram_entry_cast = RamEntry(conf)
    ht_ram_entry_cast.assignFromBits(ht.io.ht_res_if.ram_data.asBits)

    val r_lock_resp = Reg(LockRespType())

    val INSERT_TRY = new State with EntryPoint
    val INSET_RESP, LK_RESP = new State

    INSERT_TRY
      .whenIsActive{
        val try_onwer_cnt = UInt(conf.queueCntWidth bits)
        try_onwer_cnt := 1
        ht.io.ht_res_if.ready := True

        io.lock_req.ready := ht.io.ht_cmd_if.ready

        when(io.lock_req.valid){
          ht.io.sendCmd(req.lock_addr, (io.lock_req.lock_type ## try_onwer_cnt).asUInt, HashTableOpCode.ins2)
        }

        when(io.lock_req.fire){
          goto(INSET_RESP)
        }
      }

    INSET_RESP
      .whenIsActive {
        ht.io.ht_res_if.ready := True
        ht.io.update_addr := ht.io.ht_res_if.find_addr
        when(ht.io.ht_res_if.fire) {
          when(!req.lock_release) {

            ht.io.update_data := (ht_ram_entry_cast.key ## req.lock_type ## (ht_ram_entry_cast.owner_cnt+1) ## ht_ram_entry_cast.next_ptr ## ht_ram_entry_cast.net_ptr_val).asUInt

            when(ht.io.ht_res_if.rescode === HashTableRetCode.ins_exist) {
              // lock exist
              when((!req.lock_upgrade && (ht_lock_entry_cast.lock_status | req.lock_type)) || (req.lock_upgrade && ht_lock_entry_cast.owner_cnt > 1)) {
                r_lock_resp := LockRespType.abort // no wait
                goto(LK_RESP)
              } otherwise {
                r_lock_resp := LockRespType.grant
                // write back to ht data ram
                ht.io.update_en := True
                goto(LK_RESP)
              }
            } otherwise {
              // insert_success
              r_lock_resp := LockRespType.grant
              goto(LK_RESP)
            }
          } otherwise {

            ht.io.update_data := (ht_ram_entry_cast.key ## req.lock_type ## (ht_ram_entry_cast.owner_cnt-1) ## ht_ram_entry_cast.next_ptr ## ht_ram_entry_cast.net_ptr_val).asUInt

            // lock release, ht.io.ht_res_if.rescode must be ins_exist. 2 cases: cnt-- or del entry (cost a few cycles)
            when(ht_ram_entry_cast.owner_cnt===1){
              // ht must be ready, del the entry
              ht.io.sendCmd(req.lock_addr, 0, HashTableOpCode.del)
            } otherwise {
              ht.io.update_en := True
            }
            r_lock_resp := LockRespType.release
            goto(LK_RESP)
          }
        }
      }

    LK_RESP
      .whenIsActive{
        ht.io.ht_res_if.ready := True

        io.lock_resp.valid := True
        io.lock_resp.txn_id := req.txn_id
        io.lock_resp.lock_addr := req.lock_addr
        io.lock_resp.lock_type := req.lock_type
        io.lock_resp.lock_upgrade := req.lock_upgrade
        io.lock_resp.resp_type := r_lock_resp
        io.lock_resp.lock_idx := req.lock_idx
        when(io.lock_resp.fire){goto(INSERT_TRY)}
      }
  }

}
