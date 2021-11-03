//package tm
//
//import spinal.core.{UInt, _}
//import spinal.core
//import spinal.lib._
//import spinal.lib.fsm._
//import spinal.lib.bus.amba4.axi._
//import spinal.lib.fsm.StateMachine
//
//import scala.language.postfixOps
//
//object LockRespType extends SpinalEnum{
//  val grant, abort, waiting, release = newElement()
//}
//
//
//case class LockTableConfig(txnIDWidth:Int, unitAddrWidth:Int, htBucketWidth:Int, htTableWidth:Int, llTableWidth:Int, queueCntWidth:Int){
//  // value of ht: lock_status ()
//  def htValWidth = 1 + queueCntWidth
//}
//
//// value of ht
//case class LockEntry(conf: LockTableConfig) extends Bundle{
//  val lock_status = Bool() // sh, ex
//  val owner_cnt = UInt(conf.queueCntWidth bits)
//  def toUInt : UInt = {
//    this.asBits.asUInt
//  }
//}
//
//case class LockReq(conf: LockTableConfig) extends Bundle{
//  val txn_id = UInt(conf.txnIDWidth bits)
//  val lock_addr = UInt(conf.unitAddrWidth bits)
//  val lock_type = Bool() // sh, ex
//  val lock_upgrade = Bool() // normal, upgrade
//  val lock_release = Bool() // get, release
//  val lock_idx = UInt(8 bits) // address index to txn manager (out of order resp)
//  //  val txn_ts
//}
//
//case class LockResp(conf: LockTableConfig) extends Bundle{
//  val txn_id = UInt(conf.txnIDWidth bits)
//  val resp_type = LockRespType() // grant, abort, waiting, release
//  val lock_addr = UInt(conf.unitAddrWidth bits) // for test
//  val lock_type = Bool() // for test
//  val lock_upgrade = Bool() // normal, upgrade
//  val lock_idx = UInt(8 bits)
//}
//
//class LockTableIO(conf: LockTableConfig) extends Bundle{
//  val lock_req = slave Stream(LockReq(conf))
//  val lock_resp = master Stream(LockResp(conf))
//
//  def setDefault() = {
//    lock_req.ready := False
//    lock_resp.valid := False
//    lock_resp.txn_id := 0
//    lock_resp.lock_addr := 0
//    lock_resp.lock_type := False
//    lock_resp.lock_upgrade := False
//    lock_resp.resp_type := LockRespType.abort
//    lock_resp.lock_idx := 0
//  }
//}
//
//
//class LockTable(conf: LockTableConfig) extends Component {
//
//  val io = new LockTableIO(conf)
//  // hash table & two linked list inside locktable
//  val ht = new HashTableDUT(conf.unitAddrWidth, conf.htValWidth, conf.htBucketWidth, conf.htTableWidth)
//
//  io.setDefault()
//  ht.io.setDefault()
//
//
//  val fsm = new StateMachine {
//    val IDLE, SEA_HT_CMD, SEA_HT_RESP, OP_LL_CMD, OP_LL_RESP, OP_HT_CMD, OP_HT_RESP, LOCK_RESP = new State
//    setEntry(IDLE)
//
//    // stage lock_req
//    val req = RegNextWhen(io.lock_req.payload, io.lock_req.fire)
//    // stage ht out
//    val ht_lock_entry_cast = LockEntry(conf)
//    ht_lock_entry_cast.assignFromBits(ht.io.ht_res_if.found_value.asBits) // wire: cast the value of ht to lock_entry
//    val r_ht_lock_entry = RegNextWhen(ht_lock_entry_cast, ht.io.ht_res_if.fire)
//
//    val r_lock_resp = Reg(LockRespType())
//
//    IDLE
//      .whenIsActive {
//        io.lock_req.ready := True
//        when(io.lock_req.fire) {
//          goto(SEA_HT_CMD)
//        }
//      }
//
//    SEA_HT_CMD
//      .whenIsActive {
//        // issue ht search command
//        ht.io.sendCmd(req.lock_addr, 0, HashTableOpCode.sea)
//        when(ht.io.ht_cmd_if.fire) {goto(SEA_HT_RESP)}
//      }
//
//    SEA_HT_RESP
//      .whenIsActive{
//        ht.io.ht_res_if.ready := True
//        when(ht.io.ht_res_if.fire) {
//          // parse the search results to lock resp
//          when(!req.lock_release){
//            when(ht.io.ht_res_if.rescode === HashTableRetCode.sea_success) {
//              when((!req.lock_upgrade && (ht_lock_entry_cast.lock_status | req.lock_type)) || (req.lock_upgrade && ht_lock_entry_cast.owner_cnt > 1)){
//                //              when(ht_lock_entry_cast.lock_status | req.lock_type) {
//                r_lock_resp := LockRespType.abort // no wait
//                goto(LOCK_RESP)
//              }otherwise{
//                r_lock_resp := LockRespType.grant
//                goto(OP_LL_CMD)
//              }
//            } otherwise{
//              r_lock_resp := LockRespType.grant
//              goto(OP_LL_CMD)
//            }
//          } otherwise{
//            r_lock_resp := LockRespType.release
//            goto(OP_LL_CMD)
//          }
//        }
//      }
//
//    OP_HT_CMD
//      // update the lock_entry
//      .onEntry {
//        switch(r_lock_resp) {
//          is(LockRespType.grant) {
//            r_ht_lock_entry.owner_cnt := r_ht_lock_entry.owner_cnt + 1
//            r_ht_lock_entry.lock_status := req.lock_type
//          }
//          is(LockRespType.abort) {} // do nothing, bypass now
//          is(LockRespType.release) {
//            r_ht_lock_entry.owner_cnt := r_ht_lock_entry.owner_cnt - 1
//          }
//        }
//      }
//      .whenIsActive{
//        switch(r_lock_resp) {
//          is(LockRespType.grant) {
//            ht.io.sendCmd(req.lock_addr, r_ht_lock_entry.toUInt, HashTableOpCode.ins)
//          }
//          is(LockRespType.abort) {} // do nothing, bypass now
//          is(LockRespType.release) {
//            when(r_ht_lock_entry.owner_cnt === 0) {
//              ht.io.sendCmd(req.lock_addr, r_ht_lock_entry.toUInt, HashTableOpCode.del)
//            } otherwise ht.io.sendCmd(req.lock_addr, r_ht_lock_entry.asBits.asUInt, HashTableOpCode.ins)
//          }
//        }
//        when(ht.io.ht_cmd_if.fire){goto(OP_HT_RESP)}
//      }
//
//    OP_HT_RESP
//      .whenIsActive{
//        ht.io.ht_res_if.ready := True
//        when(ht.io.ht_res_if.fire){goto(LOCK_RESP)}
//      }
//
//    LOCK_RESP
//      .whenIsActive{
//        io.lock_resp.valid := True
//        io.lock_resp.txn_id := req.txn_id
//        io.lock_resp.lock_addr := req.lock_addr
//        io.lock_resp.lock_type := req.lock_type
//        io.lock_resp.lock_upgrade := req.lock_upgrade
//        io.lock_resp.resp_type := r_lock_resp
//        io.lock_resp.lock_idx := req.lock_idx
//        when(io.lock_resp.fire){goto(IDLE)}
//      }
//  }
//
//}
