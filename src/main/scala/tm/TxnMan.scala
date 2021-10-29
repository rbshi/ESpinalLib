package tm

import spinal.core.{UInt, _}
import spinal.core
import spinal.core.Mem
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm.StateMachine

import scala.language.postfixOps

case class OpReq(conf: LockTableConfig) extends Bundle{
  val addr = UInt(conf.unitAddrWidth bits)
  val data = UInt(64 bits)
  val mode = Bool() // r/w
  val txn_sig = UInt(2 bits) // 0: addr mode, 1: txn_start, 2: txn_end
}

case class OpResp(conf: LockTableConfig) extends Bundle{
  val data = UInt(64 bits) // FIXME word size
  val mode = Bool() // r/w
  val status = Bool() // 0: continue 1: restart
}

case class TxnManIO(conf: LockTableConfig) extends Bundle{
  // to operator
  val op_req = slave Stream(OpReq(conf))
  val op_resp = master Stream(OpResp(conf))

  // to lock table
  val lt_req = master Stream(LockReq(conf))
  val lt_resp = slave Stream(LockResp(conf))

  // to axi
  val axi = master(Axi4(Axi4Config(
    addressWidth = 64,
    dataWidth    = 64,
    idWidth = 1,
    useStrb = true,
    useBurst = true,
    useId = true,
    useLock      = false,
    useRegion    = false,
    useCache     = false,
    useProt      = false,
    useQos       = false
  )))

  def setDefault() = {
    lt_req.valid := False
    lt_req.txn_id := 0
    lt_req.lock_addr := 0
    lt_req.lock_type := False
    lt_req.lock_release := False
    lt_req.lock_idx := 0
    lt_resp.ready := False

    axi.readCmd.size := log2Up(64 / 8)
    axi.readCmd.id := 0
    axi.readCmd.len := 0
    axi.readCmd.setBurstINCR()
  }
}


class TxnMan(conf: LockTableConfig) extends Bundle{
  val io = new TxnManIO(conf)
  io.setDefault()

  val op_req = RegNextWhen(io.op_req.payload, io.op_req.fire)
  val lk_req_cnt, lk_resp_cnt, lk_req_wr_cnt, lk_resp_wr_cnt = Reg(UInt(8 bits)).init(0)
  val tab_iter_cnt = Reg(UInt(8 bits)).init(0)
//  val r_abort = RegNextWhen(io.lt_resp.lock_type === LockRespType.abort, io.lt_resp.fire)
  val r_abort = Reg(Bool())

  val mem = Mem(OpReq(conf), 32)

  val sm_req = new StateMachine {
    val IDLE, WAIT_ADDR_OR_END, AXI_RD_REQ, LOCK_REQ, LOCK_WAIT, WRITE_BACK, LOCK_RELEASE, TXN_END = new State
    setEntry(IDLE)

    IDLE
      .whenIsActive{
        // init and reset

        // wait for the start signal
        io.op_req.ready := True
        when(io.op_req.fire){goto(WAIT_ADDR_OR_END)}
      }

    WAIT_ADDR_OR_END
      .whenIsActive{
        io.op_req.ready := True
        when(r_abort){goto(LOCK_RELEASE)} otherwise{
          when(io.op_req.fire){
            when(io.op_req.txn_sig===2){
              // txn_end
              goto(LOCK_WAIT)
            } otherwise {
              switch(io.op_req.mode){
                is(False){goto(AXI_RD_REQ)} // read
                is(True){goto(LOCK_REQ)}
              }
            }
          }
        }
      }

    AXI_RD_REQ
      .whenIsActive{
        io.axi.readCmd.valid := True
        io.axi.readCmd.addr := op_req.addr
        when(io.axi.readCmd.fire){goto(LOCK_REQ)}
      }

    LOCK_REQ
      .whenIsActive{
        io.lt_req.valid := True
        io.lt_req.txn_id := 0 // FIXME
        io.lt_req.lock_addr := op_req.addr
        io.lt_req.lock_type := op_req.mode
        io.lt_req.lock_release := False
        io.lt_req.lock_idx := lk_req_cnt

        when(io.lt_req.fire){
          mem.write(lk_resp_cnt, op_req)
          lk_req_cnt := lk_req_cnt + 1
          goto(WAIT_ADDR_OR_END)
        }
      }

    // op sends txn_end signal (wait for the remaining lock resp)
    LOCK_WAIT
      .whenIsActive{
        // get all lock_resp
        when(lk_req_cnt === lk_resp_cnt){
          when(~r_abort){goto(WRITE_BACK)} otherwise{goto(LOCK_RELEASE)}
        }
      }



    val mem_op_req = OpReq(conf)

    WRITE_BACK
      .onEntry{
        lk_req_wr_cnt := 0
        tab_iter_cnt := 0
      }
      .whenIsActive{

        // assume wr bus is always ready
        io.axi.writeCmd.valid := mem_op_req.mode // if wr req
        io.axi.writeData.valid := mem_op_req.mode
        io.axi.writeCmd.addr := mem_op_req.addr
        io.axi.writeData.data := mem_op_req.data

        // axi write back
        mem_op_req := mem.readSync(tab_iter_cnt) // one clock latency
        tab_iter_cnt := tab_iter_cnt + 1

        when(mem_op_req.mode){lk_req_wr_cnt := lk_req_wr_cnt + 1}

        when(tab_iter_cnt === lk_req_cnt){goto(LOCK_RELEASE)}
      }

    LOCK_RELEASE
      .onEntry{
        tab_iter_cnt := 0
      }
      .whenIsActive{
        //TODO: release the lock only when get the `corresponding` axi_response
        // now release lock after get all wr resp
        when(lk_resp_wr_cnt===lk_req_wr_cnt){
          mem_op_req := mem.readSync(tab_iter_cnt) // one clock latency
          tab_iter_cnt := tab_iter_cnt + 1

          io.lt_req.valid := mem_op_req.mode // if wr
          io.lt_req.lock_addr := mem_op_req.addr
          io.lt_req.lock_release := True

          when(tab_iter_cnt === lk_req_cnt){goto(TXN_END)}
        }
      }

    TXN_END
      .whenIsActive{
        io.op_resp.status := r_abort
        io.op_resp.valid := True
        when(io.op_resp.fire){goto(IDLE)}
      }
  }


  val op_rd_resp = new Area {
    // set axi resp ready
    io.axi.readRsp.ready := True
    when(io.axi.readRsp.fire){
      // assume op_resp is always ready
      io.op_resp.valid := True
      io.op_resp.data := io.axi.readRsp.data
      io.op_resp.mode := False // read
      io.op_resp.status := r_abort // restart
    }
  }

  // do not need op_wr_resp

  val axi_wr_resp = new Area {
    // set axi resp ready
    io.axi.writeRsp.ready := True
    when(io.axi.writeRsp.fire){lk_resp_wr_cnt := lk_resp_wr_cnt + 1}
  }

  val lt_resp = new Area {
    val lt_resp_get_fire = io.lt_resp.fire && (io.lt_resp.resp_type=/=LockRespType.release)
    // set resp ready
    io.lt_resp.ready := True
    when(lt_resp_get_fire){
      lk_resp_cnt := lk_resp_cnt + 1
      r_abort := io.lt_resp.lock_type === LockRespType.abort
    }
  }

}



































