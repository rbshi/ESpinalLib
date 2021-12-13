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
  val data = Bits(64 bits)
  val mode = Bool() // r/w
  val upgrade = Bool() // normal / upgrade lock (sh -> ex)
  val txn_sig = UInt(2 bits) // 0: addr mode, 1: txn_start, 2: txn_end

  def setDefault(): Unit = {
    addr := 0; data := 0;    mode := False
    txn_sig := 0
  }

  def sendReq(addr: UInt, data: UInt, mode: Bool, upgrade: Bool, txn_sig: UInt): Unit = {
    this.addr := addr
    this.data := data.resize(this.data.getBitsWidth).asBits
    this.mode := mode
    this.upgrade := upgrade
    this.txn_sig := txn_sig
  }

}

case class OpResp(conf: LockTableConfig) extends Bundle {
  val data = Bits(64 bits) // FIXME word size

  def setDefault(): Unit = {
    data := 0
  }
}

case class TxnManIO(conf: LockTableConfig, axiConf: Axi4Config) extends Bundle {
  // to operator
  val op_req = slave Stream OpReq(conf)
  val op_resp = master Stream OpResp(conf) // read data only

  // to lock table
  val lt_req = master Stream LockReq(conf)
  val lt_resp = slave Stream LockResp(conf)

  // to axi
  val axi = master(Axi4(axiConf))

  val sig_txn_abort = out Bool()
  val sig_txn_end = out Bool()

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
    axi.writeData.last := True
//    axi.writeData.valid := False

    sig_txn_end := False
  }
}

class TxnMan(conf: LockTableConfig, axiConf: Axi4Config, txnManID: Int) extends Component {
  val io = TxnManIO(conf, axiConf)
  io.setDefault()

  val lk_req_cnt, lk_resp_cnt, lk_hold_cnt, lk_req_wr_cnt, lk_hold_wr_cnt, cmt_req_cnt, cmt_resp_cnt, clean_req_cnt, clean_req_wr_cnt, clean_resp_cnt = Reg(UInt(8 bits)).init(0)
  val cntList = List(lk_req_cnt, lk_resp_cnt, lk_hold_cnt, lk_req_wr_cnt, lk_hold_wr_cnt, cmt_req_cnt, cmt_resp_cnt, clean_req_cnt, clean_req_wr_cnt, clean_resp_cnt)


  val r_abort, r_to_commit, r_to_cleanup = RegInit(False)
  io.sig_txn_abort := r_abort

  val txn_wr_mem = Mem(OpReq(conf), 256)
  // local lock record (for release) word: addr + mode + t/f
  val txn_lt = Mem(Bits(conf.unitAddrWidth + 1 + 1 bits), 256)


  val req_rec = new StateMachine {
    val TXN_START = new State with EntryPoint
    val NORMAL, TXN_END = new State

    TXN_START.whenIsActive {
      io.op_req.ready := True

      // init reg
      cntList.map(_.clearAll())
      r_abort.clear()

      when(io.op_req.fire && io.op_req.txn_sig === 1) {
        goto(NORMAL)
      } // receive the txn_start
    }

    NORMAL.whenIsActive {
      when(io.op_req.txn_sig === 2 || r_abort){ // txn_end signal, if r_abort, op_req will be ignored
        io.op_req.ready := True
      } otherwise{
        io.op_req.ready := io.lt_req.ready // op_req fire till lt_req.ready by round-robin
      }

      // receive txn_end signal
      when(io.op_req.fire && io.op_req.txn_sig === 2) {
        // set flag to txn_commit & txn_cleanup
        when(~r_abort){r_to_commit := True}
        r_to_cleanup := True
        goto(TXN_END)
      } // receive the txn_end
    }

    TXN_END.whenIsActive {
      io.op_req.ready := False
      when(!r_to_commit && !r_to_cleanup) {
        io.sig_txn_end := True
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


    // if abort, ignore the op_req
    when(io.op_req.txn_sig===0 && ~r_abort){
      // bypass the valid
      io.lt_req.valid := io.op_req.valid
    }

    // axi
    io.axi.ar.addr := io.op_req.addr

    when(io.lt_req.fire && ~io.lt_req.lock_release && io.op_req.txn_sig === 0) { // normal mode
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
          // txn_lt.write(io.lt_resp.lock_idx, io.lt_resp.lock_addr ## io.lt_resp.lock_type ## True)
          // ooo arrive
          txn_lt.write(lk_hold_cnt, io.lt_resp.lock_addr ## io.lt_resp.lock_type ## True)
          lk_resp_cnt := lk_resp_cnt + 1
          lk_hold_cnt := lk_hold_cnt + 1
          when(io.lt_resp.lock_type)(lk_hold_wr_cnt := lk_hold_wr_cnt + 1)
          // when the sh lock is obtained, send axi read cmd
          when(~io.lt_resp.lock_type) {goto(AXI_RD_REQ)} // FIXME: the later coming lock_resp may be granted!
        }

        when(io.lt_resp.resp_type === LockRespType.abort) {
          r_abort := True
          lk_resp_cnt := lk_resp_cnt + 1
        }

        when(io.lt_resp.resp_type === LockRespType.release) {
          clean_resp_cnt := clean_resp_cnt + 1
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
    io.op_resp.data := io.axi.r.data(io.op_resp.data.getBitsWidth-1 downto 0)
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

    val rAwFire, rWFire = Reg(Bool()).init(False)

    when(io.axi.aw.fire)(rAwFire.set())
    when(io.axi.w.fire)(rWFire.set())

    mem_rddata.ready := False
    when(rAwFire && rWFire){
      mem_rddata.ready := True
      rAwFire.clear()
      rWFire.clear()
    }


    io.axi.aw.valid := mem_rddata.valid && ~rAwFire
    io.axi.w.valid := mem_rddata.valid && ~rWFire
    io.axi.aw.addr := mem_rddata.addr
    io.axi.w.data := mem_rddata.data.resize(axiConf.dataWidth)

    when(req_rec.isActive(req_rec.TXN_END)) {

      when(lk_req_cnt === lk_resp_cnt && ~r_abort && r_to_commit && cmt_req_cnt < lk_hold_wr_cnt) {
        mem_rdcmd.valid := True
      }
      when(mem_rdcmd.fire) {
        cmt_req_cnt := cmt_req_cnt + 1
      }

      when(lk_req_cnt === lk_resp_cnt && (cmt_resp_cnt === lk_hold_wr_cnt || r_abort)) { // bug fix: r_to_commit should be set to f after all lk_resp
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

      // 1. after receive the resp of all lk_req
      // 2. r_to_cleanup
      // 3. (~r_to_commit, no commit) || clean_req_wr_cnt < cmt_resp_cnt || clean_req_wr_cnt === lk_req_wr_cnt (all req_wr has been released)
      // 4. clean_req_cnt < lk_hold_cnt
      // FIXME: mem_rdcmd is 1 clcyle ahead of mem_rddata, so may issue one more wr_lock release.
      when(lk_req_cnt === lk_resp_cnt && r_to_cleanup && ( ~r_to_commit || clean_req_wr_cnt < cmt_resp_cnt || clean_req_wr_cnt === lk_req_wr_cnt) && clean_req_cnt < lk_hold_cnt) {
        mem_rdcmd.valid := True
      }

      when(mem_rdcmd.fire) {
        clean_req_cnt := clean_req_cnt + 1
      }

      when(mem_rddata.fire){
        when(mem_rddata.payload(1)){clean_req_wr_cnt := clean_req_wr_cnt + 1} // wr lock
      }

      when(clean_resp_cnt === lk_hold_cnt && lk_req_cnt === lk_resp_cnt){ // bug fix
        r_to_cleanup := False
      }
    }
  }

}



































