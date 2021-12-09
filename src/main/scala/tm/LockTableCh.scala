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

  //
  val arrayLT = Array.fill(numLT)(new LockTable(conf))

  // dispatch lock_req
  io.lock_req.ready := False
  for (i <- 0 until numLT){
    // low bits interleave
    when(io.lock_req.lock_addr(log2Up(numLT)-1 downto 0) === i){
      // redefine lock_addr
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
    (arrayLT(i).io.lock_resp.lock_addr(conf.unitAddrWidth-1 downto log2Up(numLT)) ## addrLowBit).asUInt <> lockRespArb.io.inputs(i).lock_addr
    arrayLT(i).io.lock_resp.txn_id <> lockRespArb.io.inputs(i).txn_id
    arrayLT(i).io.lock_resp.resp_type <> lockRespArb.io.inputs(i).resp_type
    arrayLT(i).io.lock_resp.lock_type <> lockRespArb.io.inputs(i).lock_type
    arrayLT(i).io.lock_resp.lock_upgrade <> lockRespArb.io.inputs(i).lock_upgrade
    arrayLT(i).io.lock_resp.lock_idx <> lockRespArb.io.inputs(i).lock_idx
    arrayLT(i).io.lock_resp.ready <> lockRespArb.io.inputs(i).ready
    arrayLT(i).io.lock_resp.valid <> lockRespArb.io.inputs(i).valid
  }

  lockRespArb.io.output >> io.lock_resp

}
