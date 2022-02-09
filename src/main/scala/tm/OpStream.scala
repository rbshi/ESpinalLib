package tm

import spinal.core.{UInt, Vec, _}
import spinal.core
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm.StateMachine
import util.SetDefaultIO

import scala.language.postfixOps


class OpStream(conf: LockTableConfig, axiConfig: Axi4Config) extends Component with SetDefaultIO{

  val io = new Bundle {
    val axi = master(Axi4ReadOnly(axiConfig))
    val op_req = master Stream OpReq(conf)
    val op_resp = slave Stream OpResp(conf)
    val sig_txn_abort = in Bool()
    val sig_txn_end = in Bool()
    val txn_len = in UInt(8 bits)
    val txn_cnt = in UInt(16 bits)
    val done = out Bool()
    val start = in Bool()
    val addr_offset = in UInt(axiConfig.addressWidth bits)
    val txn_exe_cnt = out(Reg(UInt(16 bits)).init(0))
    val txn_abort_cnt = out(Reg(UInt(16 bits)).init(0))
  }

  io.axi.ar.valid := False
  io.axi.ar.id := 0
  io.axi.ar.len := 0
  io.axi.ar.size := log2Up(512/8)
  io.axi.r.ready := True

  if(axiConfig.useBurst) {
    io.axi.ar.setBurstINCR()
  }

  setDefStream(io.op_req)

  val txn_mem = Mem(OpReq(conf), 512)
  val txn_mem_wr_addr = Reg(UInt(9 bits)).init(0)
  val txn_mem_wr_data = io.axi.r.data(io.op_req.getBitsWidth-1 downto 0).toDataType(OpReq(conf))
  val txn_mem_rd_addr = Reg(UInt(9 bits)).init(0)

  val txn_loaded_cnt = Reg(UInt(16 bits)).init(0)

  io.done := (io.txn_exe_cnt === io.txn_cnt)
  io.op_resp.ready := True // bypass the op_resp


  val load_txn = new StateMachine {
    val txn_load_cnt = Reg(UInt(16 bits)).init(0)
    val load_addr = Reg(axiConfig.addressType).init(0)
    val req_load_cnt = Reg(UInt(8 bits)).init(0)

    io.axi.ar.addr := load_addr + io.addr_offset

    val IDLE = new State with EntryPoint
    val LOAD = new State

    IDLE.whenIsActive{
      when(io.start){
        load_addr.clearAll()
        txn_load_cnt.clearAll()
        txn_loaded_cnt.clearAll()
        io.txn_exe_cnt.clearAll()
        io.txn_abort_cnt.clearAll()
        goto(LOAD)
      }
    }

    LOAD.whenIsActive{
      when((txn_load_cnt - io.txn_exe_cnt)<2 && txn_load_cnt < io.txn_cnt){
        io.axi.ar.valid := True
      }
      when(io.axi.ar.fire){
        load_addr := load_addr + 64
        req_load_cnt := req_load_cnt + 1
        when(req_load_cnt === (io.txn_len-1)){
          txn_load_cnt := txn_load_cnt + 1 // fixme: here txn_load_cnt does not mean the data are all ready
          req_load_cnt := 0
        }
      }
      // axi rd resp: write the req with double buffering
      when(io.axi.r.fire){
        txn_mem.write(txn_mem_wr_addr, txn_mem_wr_data)

        when(txn_mem_wr_addr === io.txn_len-1){txn_loaded_cnt := txn_loaded_cnt + 1}

        when(txn_mem_wr_addr === io.txn_len*2-1){
          txn_loaded_cnt := txn_loaded_cnt + 1
          txn_mem_wr_addr := 0
          when(txn_loaded_cnt === (io.txn_cnt-1)){goto(IDLE)}
        } otherwise {txn_mem_wr_addr := txn_mem_wr_addr + 1}
      }
    }
  }


  val sendTxn = new StateMachine {

    val mem_rdcmd = Stream(UInt(9 bits))
    mem_rdcmd.payload := txn_mem_rd_addr
    mem_rdcmd.valid := False
    mem_rdcmd.ready := False

    val flagPingPong = Reg(Bool()).init(False)

    val TxnStart = new State with EntryPoint
    val Normal, TxnEnd, WaitClean = new State

    TxnStart.whenIsActive{
      // wait for txn_load
      when(txn_loaded_cnt > io.txn_exe_cnt){
        io.op_req.valid := True
        io.op_req.sendReq(0, 0, False, False, 1) // txn_start
      }
      when(io.op_req.fire){goto(Normal)}
    }

    Normal.onEntry{
      // setup the start address
      when(~flagPingPong){txn_mem_rd_addr := 0} otherwise {txn_mem_rd_addr := io.txn_len.resized}
    }

    Normal.whenIsActive{
      val mem_rddata = txn_mem.streamReadSync(mem_rdcmd)
      mem_rddata.ready := io.op_req.ready
      io.op_req.payload := mem_rddata.payload
      io.op_req.valid := mem_rddata.valid

      when(~io.sig_txn_abort && ((~flagPingPong && txn_mem_rd_addr<io.txn_len) || (flagPingPong && txn_mem_rd_addr<io.txn_len*2))){mem_rdcmd.valid := True}
      when(mem_rdcmd.fire){txn_mem_rd_addr := txn_mem_rd_addr + 1}

      // stop txn when abort
      when(io.sig_txn_abort){
        io.txn_abort_cnt := io.txn_abort_cnt + 1
        goto(TxnEnd)
      }
      // all txn reqs have been sent
      when(io.op_req.fire && ((~flagPingPong && txn_mem_rd_addr===io.txn_len) || (flagPingPong && txn_mem_rd_addr===io.txn_len*2))){goto(TxnEnd)}
    }

    TxnEnd.whenIsActive{
      io.op_req.valid := True
      io.op_req.sendReq(0, 0, False, False, 2) // txn_end
      when(io.op_req.fire){goto(WaitClean)}
    }

    WaitClean.whenIsActive{
      when(io.sig_txn_end){
        when(~io.sig_txn_abort){
          flagPingPong := ~flagPingPong
          io.txn_exe_cnt := io.txn_exe_cnt + 1
        } // flip if commit
        goto(TxnStart)
      }

      // bug fix: may send req very quickly, and wait for abort sig
      when(io.sig_txn_abort){
        io.txn_abort_cnt := io.txn_abort_cnt + 1
        goto(TxnStart)
      }

    }
  }

}
