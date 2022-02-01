package tm

import spinal.core.{UInt, _}
import spinal.lib._
import spinal.lib.StreamArbiterFactory
import spinal.lib.StreamDispatcherSequencial
import spinal.lib.slave
import spinal.lib.master
import spinal.lib.bus.amba4.axi._

// dispatch lock_req to multiple LTs
class LockTableCh(conf: LockTableConfig, numLT: Int) extends Component{
  val io = new LockTableIO(conf)

  // change the unitAddr of each LT
  val ltConf = LockTableConfig(conf.txnIDWidth, conf.unitAddrWidth-log2Up(numLT), conf.htBucketWidth, conf.htTableWidth, conf.llTableWidth, conf.queueCntWidth, conf.key2AddrShift)

  val arrayLT = Array.fill(numLT)(new LockTable(ltConf))

  // dispatch lock_req
  io.lock_req.ready := False
  for (i <- 0 until numLT){
    // low bits interleave
    when(io.lock_req.lock_addr(log2Up(numLT)-1 downto 0) === i){
      // use MSB as the lock_addr to distributed LTs
        arrayLT(i).io.lock_req.lock_addr <> io.lock_req.lock_addr(conf.unitAddrWidth-1 downto log2Up(numLT)).resized
      io.lock_req.txn_id <> arrayLT(i).io.lock_req.txn_id
      io.lock_req.lock_type <> arrayLT(i).io.lock_req.lock_type
      io.lock_req.lock_upgrade <> arrayLT(i).io.lock_req.lock_upgrade
      io.lock_req.lock_release <> arrayLT(i).io.lock_req.lock_release
      io.lock_req.lock_idx <> arrayLT(i).io.lock_req.lock_idx
      io.lock_req.ready <> arrayLT(i).io.lock_req.ready
      io.lock_req.valid <> arrayLT(i).io.lock_req.valid
    } otherwise {
      // should not happen, but behavior should be defined
      arrayLT(i).io.lock_req.setDefault()
      arrayLT(i).io.lock_req.valid := False
    }
  }

  // arbiter lock_resp
  val lockRespArb = StreamArbiterFactory.roundRobin.noLock.build(LockResp(conf), numLT)
  for (i <- 0 until numLT){
    // redefine addr
    val addrLowBit = UInt(log2Up(numLT) bits)
    addrLowBit := i
    // add a pipe stage for retiming
    val lkRespPipe = Stream(LockResp(ltConf))
    arrayLT(i).io.lock_resp >/-> lkRespPipe

    lockRespArb.io.inputs(i).lock_addr := (lkRespPipe.lock_addr ## addrLowBit).asUInt.resized
    lkRespPipe.txn_id <> lockRespArb.io.inputs(i).txn_id
    lkRespPipe.resp_type <> lockRespArb.io.inputs(i).resp_type
    lkRespPipe.lock_type <> lockRespArb.io.inputs(i).lock_type
    lkRespPipe.lock_upgrade <> lockRespArb.io.inputs(i).lock_upgrade
    lkRespPipe.lock_idx <> lockRespArb.io.inputs(i).lock_idx
    lkRespPipe.ready <> lockRespArb.io.inputs(i).ready
    lkRespPipe.valid <> lockRespArb.io.inputs(i).valid

//    lockRespArb.io.inputs(i).lock_addr := (arrayLT(i).io.lock_resp.lock_addr ## addrLowBit).asUInt.resized
//    arrayLT(i).io.lock_resp.txn_id <> lockRespArb.io.inputs(i).txn_id
//    arrayLT(i).io.lock_resp.resp_type <> lockRespArb.io.inputs(i).resp_type
//    arrayLT(i).io.lock_resp.lock_type <> lockRespArb.io.inputs(i).lock_type
//    arrayLT(i).io.lock_resp.lock_upgrade <> lockRespArb.io.inputs(i).lock_upgrade
//    arrayLT(i).io.lock_resp.lock_idx <> lockRespArb.io.inputs(i).lock_idx
//    arrayLT(i).io.lock_resp.ready <> lockRespArb.io.inputs(i).ready
//    arrayLT(i).io.lock_resp.valid <> lockRespArb.io.inputs(i).valid
  }
  // timing
  lockRespArb.io.output >/-> io.lock_resp

}
