package tm

import spinal.core.{UInt, _}
import spinal.core.Mem
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm.StateMachine

import scala.language.postfixOps

case class OpReq(conf: LockTableConfig) extends Bundle {
  val addr = UInt(conf.unitAddrWidth bits)
  val data = UInt(64 bits)
  val mode = Bool() // r/w
  val upgrade = Bool() // normal / upgrade lock (sh -> ex)
  val txn_sig = UInt(2 bits) // 0: addr mode, 1: txn_start, 2: txn_end

  def setDefault(): Unit = {
    addr := 0; data := 0;    mode := False
    txn_sig := 0
  }
}

case class OpResp(conf: LockTableConfig) extends Bundle {
  val data = UInt(64 bits) // FIXME word size

  def setDefault(): Unit = {
    data := 0
  }
}

case class TxnManIO(conf: LockTableConfig, axiConf: Axi4Config) extends Bundle {
  // to operator
  val op_req = slave Stream OpReq(conf)
  val op_resp = master Stream OpResp(conf)

  // to lock table
  val lt_req = master Stream LockReq(conf)
  val lt_resp = slave Stream LockResp(conf)

  // to axi
  val axi = master(Axi4(axiConf))

  val txn_abort = out Bool()
  val txn_end_test = out Bool()

  def setDefault() = {
    lt_req.valid := False
//    lt_req.txn_id := 0
    lt_resp.ready := False

    op_req.ready := False
    op_resp.valid := False

    axi.readCmd.size := log2Up(64 / 8)
    //    axi.readCmd.addr := 0
    axi.readCmd.id := 0
    axi.readCmd.valid := False
    axi.readCmd.len := 0
//    axi.writeCmd.valid := False
    axi.writeCmd.size := 0
//    axi.writeCmd.addr := 0
    axi.writeCmd.id := 0
    axi.writeCmd.len := 0
    //    axi.readRsp.ready := True
    //    axi.writeRsp.ready := True
//    axi.writeData.data := 0
    axi.writeData.last := False
//    axi.writeData.valid := False

    txn_end_test := False
  }
}

class TxnMan(conf: LockTableConfig, axiConf: Axi4Config, txnManID: Int) extends Component {
  val io = TxnManIO(conf, axiConf)
  io.setDefault()

  val lk_req_cnt, lk_resp_cnt, lk_req_wr_cnt, lk_resp_wr_cnt, cmt_req_cnt, cmt_resp_cnt, clean_req_cnt, clean_req_wr_cnt = Reg(UInt(8 bits)).init(0)
  val r_abort, r_to_commit, r_to_cleanup = RegInit(False)
  io.txn_abort := r_abort

  val txn_wr_mem = Mem(OpReq(conf), 256)
  // local lock record (for release) word: addr + mode + t/f
  val txn_lt = Mem(Bits(conf.unitAddrWidth + 1 + 1 bits), 256)


  val req_rec = new StateMachine {
    val TXN_START = new State with EntryPoint
    val NORMAL, TXN_END = new State

    TXN_START.whenIsActive {
      io.op_req.ready := True

      // init reg

      when(io.op_req.fire && io.op_req.txn_sig === 1) {
        goto(NORMAL)
      } // receive the txn_start
    }

    NORMAL.whenIsActive {
      when(io.op_req.txn_sig === 2){ // txn_end signal
        io.op_req.ready := True
      } otherwise{
        io.op_req.ready := io.lt_req.ready // op_req fire till lt_req.ready by round-robin
      }

      // receive txn_end signal
      when(io.op_req.fire && io.op_req.txn_sig === 2) {
        // set flag to txn_commit & txn_cleanup
        r_to_commit := True
        r_to_cleanup := True
        goto(TXN_END)
      } // receive the txn_end
    }

    TXN_END.whenIsActive {
      io.op_req.ready := False
      when(!r_to_commit && !r_to_cleanup) {
        io.txn_end_test := True
        goto(TXN_START)
      } // finish txn commit / abort
    }
  }

  val req_act = new Area {
    // lt
    io.lt_req.lock_addr := io.op_req.addr
    io.lt_req.lock_type := io.op_req.mode
    io.lt_req.lock_upgrade := io.op_req.upgrade
    io.lt_req.lock_release := False
    io.lt_req.lock_idx := lk_req_cnt
    io.lt_req.txn_id := txnManID


    when(io.op_req.txn_sig===0){
      // bypass the valid
      io.lt_req.valid := io.op_req.valid
    }

    // axi
    io.axi.ar.addr := io.op_req.addr

    when(io.op_req.fire && io.op_req.txn_sig === 0) { // normal mode
      lk_req_cnt := lk_req_cnt + 1
      when(io.op_req.mode) {
        // write op: issue txn_wr_mem & lt_req_wr_cnt++
        txn_wr_mem.write(lk_req_wr_cnt, io.op_req)
        lk_req_wr_cnt := lk_req_wr_cnt + 1
      }
    }
  }

  val lt_resp = new StateMachine {

    val r_lock_addr = RegNextWhen(io.lt_resp.lock_addr, io.lt_resp.fire)

    val WAIT_RESP = new State with EntryPoint
    val AXI_RD_REQ = new State

    WAIT_RESP.whenIsActive {
      io.lt_resp.ready := True
      when(io.lt_resp.fire) {
        when(io.lt_resp.resp_type === LockRespType.grant) {
          // write to local lock record (lt_resp may arrive out of order)
          txn_lt.write(io.lt_resp.lock_idx, io.lt_resp.lock_addr ## io.lt_resp.lock_type ## True)
          lk_resp_cnt := lk_resp_cnt + 1
          // when the ex lock is obtained
          when(io.lt_resp.lock_type) {
            lk_resp_wr_cnt := lk_resp_wr_cnt + 1
          } otherwise {

            goto(AXI_RD_REQ)
          }
        }

        when(io.lt_resp.resp_type === LockRespType.abort) {
          r_abort := True
          // fixme: simplify
          lk_resp_cnt := lk_resp_cnt + 1
          when(io.lt_resp.lock_type) {
            lk_resp_wr_cnt := lk_resp_wr_cnt + 1
          }
        }
      }
    }

    AXI_RD_REQ.whenIsActive {
      // send read cmd
      // FIXME: lt_resp may be ooo, so is the rd address
      io.axi.ar.addr := r_lock_addr
      io.axi.ar.valid := True
      when(io.axi.ar.fire)(goto(WAIT_RESP))
    }

  }

  val axi_resp = new Area {
    io.axi.r.ready := True
    io.op_resp.data := io.axi.r.data.asUInt
    when(io.axi.r.fire) {
      // assume op_resp is always ready
      io.op_resp.valid := True
    }
    // write resp
    io.axi.b.ready := True
    when(io.axi.b.fire) {
      cmt_resp_cnt := cmt_resp_cnt + 1
    }
  }


  val txn_commit = new Area {

    val mem_rdcmd = Stream(UInt(8 bits))
    mem_rdcmd.payload := cmt_req_cnt
    mem_rdcmd.valid := False
    val mem_rddata = txn_wr_mem.streamReadSync(mem_rdcmd)
    // fork mem_rddata to two stream (axi.aw, axi.w)
    val (mem1, mem2) = StreamFork2(mem_rddata, synchronous = true)
    // mem_rddata.ready := io.axi.aw.ready && io.axi.w.ready
    io.axi.aw.addr := mem1.addr
    io.axi.aw.valid := mem1.valid
    io.axi.w.data := mem2.data.asBits
    io.axi.w.valid := mem2.valid
    io.axi.aw.ready <> mem1.ready
    io.axi.w.ready <> mem2.ready

    when(req_rec.isActive(req_rec.TXN_END)) {

      when(lk_req_cnt === lk_resp_cnt && !r_abort && r_to_commit && cmt_req_cnt < lk_req_wr_cnt) {
        mem_rdcmd.valid := True
      }
      when(mem_rdcmd.fire) {
        cmt_req_cnt := cmt_req_cnt + 1
      }

      when(cmt_req_cnt === lk_req_wr_cnt) {
        r_to_commit := False
      }
    }
  }

  // release locks
  val txn_cleanup = new Area {
    val mem_rdcmd = Stream(UInt(8 bits))
    //    val mem_rddata = Stream(Bits(conf.unitAddrWidth+1+1 bits))
    mem_rdcmd.payload := clean_req_cnt
    mem_rdcmd.valid := False

    val mem_rddata = txn_lt.streamReadSync(mem_rdcmd)
    mem_rddata.ready := io.lt_req.ready

    when(req_rec.isActive(req_rec.TXN_END)) {
      // lt
      io.lt_req.lock_addr := mem_rddata.payload(conf.unitAddrWidth + 1 downto 2).asUInt
      io.lt_req.lock_type := mem_rddata.payload(1)
      io.lt_req.lock_release := True
      io.lt_req.lock_idx := clean_req_cnt
      io.lt_req.valid := mem_rddata.valid && mem_rddata.payload(0) // all txn_lt entries should be valid

      when(lk_req_cnt === lk_resp_cnt && r_to_cleanup && (clean_req_wr_cnt < cmt_resp_cnt || cmt_resp_cnt === 0 || clean_req_wr_cnt === lk_req_wr_cnt) && clean_req_cnt < lk_req_cnt) {
        //      when(lk_req_cnt === lk_resp_cnt && r_to_cleanup && (clean_req_wr_cnt <= cmt_resp_cnt)  && clean_req_cnt < lk_req_cnt){
        mem_rdcmd.valid := True
      }

      when(mem_rdcmd.fire) {
        clean_req_cnt := clean_req_cnt + 1
      }

      when(mem_rddata.fire){
        when(mem_rddata.payload(1)){clean_req_wr_cnt := clean_req_wr_cnt + 1} // wr lock
      }

      // clean_req_cnt is one-cycle ahead of lt_req fire
      when(clean_req_cnt === lk_req_cnt &  mem_rddata.fire) {
        r_to_cleanup := False
      }
    }
  }

}



































