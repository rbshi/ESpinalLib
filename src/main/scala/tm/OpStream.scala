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
    val txn_cnt = in UInt(8 bits)
    val done = out Bool()
    val start = in Bool()
    val addr_offset = in UInt(axiConfig.addressWidth bits)
  }

  io.axi.ar.valid := False
  io.axi.ar.id := 0
  io.axi.ar.len := 0
  io.axi.ar.size := 0
  io.axi.r.ready := True

  setDefStream(io.op_req)
//  io.op_req.valid := False
//  io.op_req.addr := 0
//  io.op_req.data := 0
//  io.op_req.mode := False
//  io.op_req.upgrade := False
//  io.op_req.txn_sig := 0



  val txn_mem = Mem(OpReq(conf), 256)
  val txn_mem_wr_addr = Reg(UInt(8 bits)).init(0)
  val txn_mem_wr_data = io.axi.r.data(io.op_req.getBitsWidth-1 downto 0).toDataType(OpReq(conf))
  val txn_mem_rd_addr = Reg(UInt(8 bits)).init(0)


  val txn_load_cnt = Reg(UInt(8 bits)).init(0)
  val txn_loaded_cnt = Reg(UInt(8 bits)).init(0)
  val req_load_cnt = Reg(UInt(8 bits)).init(0)
  val txn_exe_cnt = Reg(UInt(8 bits)).init(0)

  io.done := (txn_exe_cnt === io.txn_cnt)

  val load_txn = new Area {
    val load_addr = Reg(axiConfig.addressType).init(0)
    io.axi.ar.addr := load_addr + io.addr_offset


    when((txn_load_cnt - txn_exe_cnt)<2 && txn_load_cnt < io.txn_cnt && io.start){
      io.axi.ar.valid := True
    }

    when(io.axi.ar.fire){
      load_addr := load_addr + 64
      req_load_cnt := req_load_cnt + 1
      when(req_load_cnt === io.txn_len){
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
      } otherwise {txn_mem_wr_addr := txn_mem_wr_addr + 1}
    }
  }

  io.op_resp.ready := True // bypass the op_resp

  val sendTxn = new StateMachine {

    val mem_rdcmd = Stream(UInt(8 bits))
    mem_rdcmd.payload := txn_mem_rd_addr
    mem_rdcmd.valid := False
    mem_rdcmd.ready := False

    val flagPingPong = Reg(Bool()).init(False)

    val TxnStart = new State with EntryPoint
    val Normal, TxnEnd, WaitClean = new State

    TxnStart.whenIsActive{
      // wait for txn_load
      when(txn_loaded_cnt > txn_exe_cnt){
        io.op_req.valid := True
        io.op_req.sendReq(0, 0, False, False, 1) // txn_start
      }
      when(io.op_req.fire){goto(Normal)}
    }

    Normal.onEntry{
      // setup the start address
      when(~flagPingPong){txn_mem_rd_addr := 0} otherwise {txn_mem_rd_addr := io.txn_len}
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
        goto(TxnEnd)
      }
      // all txn reqs have been sent
      when(io.op_req.fire && ((~flagPingPong && txn_mem_rd_addr===io.txn_len) || (flagPingPong && txn_mem_rd_addr===io.txn_len*2))){goto(TxnEnd)}
    }

    TxnEnd.whenIsActive{
      io.op_req.valid := True
      io.op_req.sendReq(0, 0, False, False, 2) // txn_start
      when(io.op_req.fire){goto(WaitClean)}
    }

    WaitClean.whenIsActive{
      when(io.sig_txn_end){
        when(~io.sig_txn_abort){
          flagPingPong := ~flagPingPong
          txn_exe_cnt := txn_exe_cnt + 1
        } // flip if commit
        goto(TxnStart)
      }
    }
  }

}
