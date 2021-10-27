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
  def htValWidth = 1 + queueCntWidth + queueCntWidth + llTableWidth + llTableWidth + 1 + 1 // see LockEntry
}

// value of ht
case class LockEntry(conf: LockTableConfig) extends Bundle{
  val lock_status = Bool() // sh, ex
  val owner_cnt = UInt(conf.queueCntWidth bits)
  val waiter_cnt = UInt(conf.queueCntWidth bits)
  val owner_ptr = UInt(conf.llTableWidth bits)
  val waiter_ptr = UInt(conf.llTableWidth bits)
  val owner_ptr_val = Bool()
  val waiter_ptr_val = Bool()
  def toUInt : UInt = {
    this.asBits.asUInt
  }
}

case class LockReq(conf: LockTableConfig) extends Bundle{
  val txn_id = UInt(conf.txnIDWidth bits)
  val lock_addr = UInt(conf.unitAddrWidth bits)
  val lock_type = Bool() // sh, ex
  val lock_release = Bool() // get, release
//  val txn_ts
}

case class LockResp(conf: LockTableConfig) extends Bundle{
  val txn_id = UInt(conf.txnIDWidth bits)
  val resp_type = LockRespType() // grant, abort, waiting, release
}

class LockTableIO(conf: LockTableConfig) extends Bundle{
  val lock_req = slave Stream(LockReq(conf))
  val lock_resp = master Stream(LockResp(conf))

  def setDefault() = {
    lock_req.ready := False
    lock_resp.valid := False
    lock_resp.txn_id := 0
    lock_resp.resp_type := LockRespType.abort
  }

}


class LockTable(conf: LockTableConfig) extends Component {

  val io = new LockTableIO(conf)
  // hash table & two linked list inside locktable
  val ht = new HashTableDUT(conf.unitAddrWidth, conf.htValWidth, conf.htBucketWidth, conf.htTableWidth)
  val ll_owner = new LinkedListDut(conf.txnIDWidth + 1, conf.llTableWidth)
//  val ll_waiter = new LinkedListDut(conf.txnIDWidth + 1, conf.llTableWidth) // FIXME: width(lock_type)=1

  io.setDefault()
  ht.io.setDefault()
  ll_owner.io.setDefault()

  val fsm = new StateMachine {

    val IDLE, SEA_HT_CMD, SEA_HT_RESP, OP_LL_CMD, OP_LL_RESP, OP_HT_CMD, OP_HT_RESP, LOCK_RESP = new State
    setEntry(IDLE)

    // stage lock_req
    val req = RegNextWhen(io.lock_req.payload, io.lock_req.fire)
    // stage ht out
    val ht_rescode = RegNextWhen(ht.io.ht_res_if.rescode, ht.io.ht_res_if.fire)
    val ht_lock_entry_cast = LockEntry(conf)
    ht_lock_entry_cast.assignFromBits(ht.io.ht_res_if.found_value.asBits) // wire: cast the value of ht to lock_entry
    val ht_lock_entry = RegNextWhen(ht_lock_entry_cast, ht.io.ht_res_if.fire)

    val r_lock_resp = Reg(LockRespType())

    IDLE
      .whenIsActive {
        io.lock_req.ready := True
        when(io.lock_req.fire) {
          goto(SEA_HT_CMD)
        }
      }

    SEA_HT_CMD
      .whenIsActive {
        // issue ht search command
        ht.io.sendCmd(req.lock_addr, 0, HashTableOpCode.sea)
        when(ht.io.ht_cmd_if.fire) {goto(SEA_HT_RESP)}
      }

    SEA_HT_RESP
      .whenIsActive{
        ht.io.ht_res_if.ready := True
        when(ht.io.ht_res_if.fire) {goto(OP_LL_CMD)}
      }
      .onExit{
        // parse the search results to lock resp
        when(!req.lock_release){
          when(ht_rescode === HashTableRetCode.sea_success) {
            when(ht_lock_entry.lock_status | req.lock_type) {
              r_lock_resp := LockRespType.abort // no wait
            }
          }otherwise{r_lock_resp := LockRespType.grant}
        }otherwise{r_lock_resp := LockRespType.release}
      }

    OP_LL_CMD
      .whenIsActive {
        when(req.lock_release) {
          //FIXME: remove from both owner & waiter queue (now only no-wait is implemented)
          ll_owner.io.sendCmd((req.lock_type ## req.txn_id).asUInt, LinkedListOpCode.del, ht_lock_entry.owner_ptr, ht_lock_entry.owner_ptr_val)
        } otherwise {
          //FIXME: only owner queue is inserted
          ll_owner.io.sendCmd((req.lock_type ## req.txn_id).asUInt, LinkedListOpCode.ins, ht_lock_entry.owner_ptr, ht_lock_entry.owner_ptr_val)
        }
        when(ll_owner.io.ll_cmd_if.fire){goto(OP_LL_RESP)}
      }

    OP_LL_RESP
      .whenIsActive {
        // save the owner_ptr from ll to lock_entry
        when(ll_owner.io.head_table_if.wr_en){
          ht_lock_entry.owner_ptr := ll_owner.io.head_table_if.wr_data_ptr
          ht_lock_entry.owner_ptr_val := ll_owner.io.head_table_if.wr_data_ptr_val
        }
        ll_owner.io.ll_res_if.ready := True
        when(ll_owner.io.ll_res_if.fire){goto(OP_HT_CMD)}
      }

    OP_HT_CMD
      // update the lock_entry
      .onEntry {
        switch(r_lock_resp) {
          is(LockRespType.grant) {
            ht_lock_entry.owner_cnt := ht_lock_entry.owner_cnt + 1
            ht_lock_entry.lock_status := req.lock_type
          }
          is(LockRespType.abort) {} // do nothing
          is(LockRespType.release) {
            ht_lock_entry.owner_cnt := ht_lock_entry.owner_cnt - 1
          }
        }
      }
      .whenIsActive{
        ll_owner.io.ll_res_if.ready := True
        switch(r_lock_resp) {
          is(LockRespType.grant) {
            ht.io.sendCmd(req.lock_addr, ht_lock_entry.toUInt, HashTableOpCode.ins)
          }
          is(LockRespType.abort) {} // do nothing
          is(LockRespType.release) {
            when(ht_lock_entry.owner_cnt === 0) {
              ht.io.sendCmd(req.lock_addr, ht_lock_entry.toUInt, HashTableOpCode.del)
            } otherwise ht.io.sendCmd(req.lock_addr, ht_lock_entry.asBits.asUInt, HashTableOpCode.ins)
          }
        }

        when(r_lock_resp === LockRespType.abort){
          goto(LOCK_RESP)
        }otherwise{
          when(ht.io.ht_cmd_if.fire){goto(OP_HT_RESP)}
        }
      }

    OP_HT_RESP
      .whenIsActive{
        ht.io.ht_res_if.ready := True
        when(ht.io.ht_res_if.fire){goto(LOCK_RESP)}
      }

    LOCK_RESP
      .whenIsActive{
        io.lock_resp.valid := True
        io.lock_resp.txn_id := req.txn_id
        io.lock_resp.resp_type := r_lock_resp
        when(io.lock_resp.fire){goto(IDLE)}
      }
  }

}
















































