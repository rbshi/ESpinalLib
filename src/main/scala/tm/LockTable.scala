package tm

import spinal.core.{UInt, _}
import spinal.lib._
import spinal.lib.bus.amba4.axi._
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

class LockTable(conf: LockTableConfig) extends Component{
  val io = new LockTableIO(conf)
  val ht = new HashTableDUT(conf.unitAddrWidth, conf.htValWidth, conf.htBucketWidth, conf.htTableWidth)
  val ll_owner = new LinkedListDut(conf.txnIDWidth, conf.llTableWidth)
  val ll_waiter = new LinkedListDut(conf.txnIDWidth, conf.llTableWidth)

  // ht takes cmd in a blocking manner
  io.lock_req.ready <> ht.io.ht_cmd_if.ready
  io.lock_req.valid <> ht.io.ht_cmd_if.valid
  io.lock_resp.ready <> ht.io.ht_res_if.ready // usually ready is high
  
}










































