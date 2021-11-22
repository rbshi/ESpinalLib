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
      io.lock_req >> arrayLT(i).io.lock_req
    } otherwise {
      // should not happen, but behavior should be defined
      arrayLT(i).io.lock_req.setDefault()
      arrayLT(i).io.lock_req.valid := False
    }
  }

  // arbiter lock_resp
  val lockRespArb = StreamArbiterFactory.roundRobin.noLock.build(LockResp(conf), numLT)
  for (i <- 0 until numLT){
    arrayLT(i).io.lock_resp >> lockRespArb.io.inputs(i)
  }

  lockRespArb.io.output >> io.lock_resp

}
