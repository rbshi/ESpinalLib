package tm

import spinal.core.{UInt, _}
import spinal.core
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm.StateMachine

import scala.language.postfixOps

case class LockTableConfig(txnIDWidth:Int, htBucketWidth:Int, htTableWidth:Int, llTableWidth:Int, unitAddrWidth:Int, queueCntWidth:Int){
  // value of ht: lock_status ()

  def htValWidth = 1 + queueCntWidth + queueCntWidth + llTableWidth + llTableWidth + 1 + 1

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
  val resp_type = UInt(3 bits) // grant, abort, wait
}

class LockTableIO(conf: LockTableConfig) extends Bundle{
  val lock_req = slave Stream(LockReq(conf))
  val lock_resp = master Stream(LockResp(conf))
}


object LockTablePhase extends SpinalEnum{
  val IDLE, SEA_HT_CMD, SEA_HT_RESP, OP_LL_CMD, OP_LL_RESP, OP_HT_CMD, OP_HT_RESP = newElement
}

class LockTable(conf: LockTableConfig) extends Component {
  val io = new LockTableIO(conf)

  val ht = new HashTableDUT(conf.unitAddrWidth, conf.htValWidth, conf.htBucketWidth, conf.htTableWidth)
  val ll_owner = new LinkedListDut(conf.txnIDWidth, conf.llTableWidth)
  val ll_waiter = new LinkedListDut(conf.txnIDWidth + 1, conf.llTableWidth) // FIXME: width(lock_type)=1

  // ht takes cmd in a blocking manner
  io.lock_req.ready <> ht.io.ht_cmd_if.ready
  io.lock_req.valid <> ht.io.ht_cmd_if.valid
  io.lock_resp.ready <> ht.io.ht_res_if.ready // usually ready is high

  val phase = RegInit(LockTablePhase.IDLE)

  val fsm = new StateMachine {

    // stage lock_req
    val req = RegNextWhen(io.lock_req.payload, io.lock_req.fire)
    // stage ht out
    val ht_rescode = RegNextWhen(ht.io.ht_res_if.rescode, ht.io.ht_res_if.fire)
    val ht_lock_entry_cast = LockEntry(conf)
    ht_lock_entry_cast.assignFromBits(ht.io.ht_res_if.found_value.asBits) // wire: cast the value of ht to lock_entry
    val ht_lock_entry = RegNextWhen(ht_lock_entry_cast, ht.io.ht_res_if.fire)


    val IDLE, SEA_HT_CMD, SEA_HT_RESP, OP_LL_CMD, OP_LL_RESP, OP_HT_CMD, OP_HT_RESP = new State
    setEntry(IDLE)

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
      .whenIsActive(when(ht.io.ht_res_if.fire) {goto(OP_LL_CMD)})

    OP_LL_CMD
      .whenIsActive {
        when(req.lock_release) {
          //FIXME: remove from both owner & waiter queue (now only no-wait is implemented)
          ll_owner.io.sendCmd((req.txn_id ## req.lock_type).asUInt, LinkedListOpCode.del, ht_lock_entry.owner_ptr, ht_lock_entry.owner_ptr_val)
        } otherwise {
          //FIXME: only owner queue is inserted
          ll_owner.io.sendCmd((req.txn_id ## req.lock_type).asUInt, LinkedListOpCode.ins, ht_lock_entry.owner_ptr, ht_lock_entry.owner_ptr_val)
        }
        when(ll_owner.io.ll_cmd_if.fire){goto(OP_LL_RESP)}
      }

    OP_LL_RESP
      .whenIsActive{
        // save the owner_ptr from ll to lock_entry
        when(ll_owner.io.head_table_if.wr_en){
          ht_lock_entry.owner_ptr := ll_owner.io.head_table_if.wr_data_ptr
          ht_lock_entry.owner_ptr_val := ll_owner.io.head_table_if.wr_data_ptr_val
        }
        when(ll_owner.io.ll_res_if.fire){goto(OP_HT_CMD)}
      }

    OP_HT_CMD
      // update the lock_entry
      .onEntry {
        // request a lock
        when(!req.lock_release) {
          when(ht_rescode === HashTableRetCode.sea_success) {
            // no conflict
            when(!(ht_lock_entry.lock_status | req.lock_type)) {
              ht_lock_entry.owner_cnt := ht_lock_entry.owner_cnt + 1
            }
            // conflict: do nothing and send abort
          } otherwise {
            // not exist
            ht_lock_entry.owner_cnt := 1
            ht_lock_entry.lock_status := req.lock_type
          }
        }otherwise {
          // release
          ht_lock_entry.owner_cnt := ht_lock_entry.owner_cnt - 1
        }
      }

      .whenIsActive{
        // request a lock
        when(!req.lock_release) {
          when(ht_rescode === HashTableRetCode.sea_success) {
            // no conflict
            when(!(ht_lock_entry.lock_status | req.lock_type)) {
              ht.io.sendCmd(req.lock_addr, ht_lock_entry.asBits.asUInt, HashTableOpCode.ins)
            }
            // conflict: do nothing and send abort

          } otherwise {
            // not exist
            ht.io.sendCmd(req.lock_addr, ht_lock_entry.asBits.asUInt, HashTableOpCode.ins)
          }
        }otherwise {
          // release
          when(ht_lock_entry.owner_cnt===0){
            // delete the lock entry in ht
            ht.io.sendCmd(req.lock_addr, ht_lock_entry.asBits.asUInt, HashTableOpCode.del)
          } otherwise(ht.io.sendCmd(req.lock_addr, ht_lock_entry.asBits.asUInt, HashTableOpCode.ins))
        }
      }

    OP_HT_RESP
      .whenIsActive{
        when(ht.io.ht_res_if.fire){goto(IDLE)}
      }
  }

}
















































