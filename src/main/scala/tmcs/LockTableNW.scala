package tmcs

import spinal.core.{UInt, _}
import spinal.core
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm.StateMachine

import scala.language.postfixOps

object LockRespType extends SpinalEnum{
  val grant, abort, waiting, release = newElement()
}

// value of ht
case class LockEntry(conf: SysConfig) extends Bundle{
  val lock_status = Bool() // sh, ex
  val owner_cnt = UInt(conf.wOwnerCnt bits)
  def toUInt : UInt = {
    this.asBits.asUInt
  }
}

case class RamEntry(conf: SysConfig) extends Bundle{

  val net_ptr_val = Bool()
  val next_ptr = UInt(conf.wHtTable bits)
  val owner_cnt = UInt(conf.wOwnerCnt bits)
  val lock_status = Bool() // sh, ex
  val key = UInt(conf.wTId bits)

  def toUInt : UInt = {
    this.asBits.asUInt
  }
}

case class LkReq(conf: SysConfig) extends Bundle{
  val nId = UInt(conf.wNId bits)
  val cId = UInt(conf.wCId bits)
  val tId = UInt(conf.wTId bits)
  val txnManId = UInt(conf.wTxnManId bits)
  val txnId = UInt(conf.wTxnId bits)
  val lkType = Bool()
  val lkUpgrade = Bool()
  val lkRelease = Bool()
  val lkIdx = UInt(conf.wLkIdx bits)
  val wLen = UInt(3 bits) // len(tuple)=2^wLen
  // val vld = Bool() // for network usage

  def setDefault() = {
    this.nId := 0
    this.cId := 0
    this.txnManId := 0
    this.txnId := 0
    this.tId := 0
    this.lkType := False
    this.lkUpgrade := False
    this.lkIdx := 0
    this.wLen := 0
  }

}

// TODO: now LkResp bypass all info in LkReq
case class LkResp(conf: SysConfig) extends Bundle{
  val nId = UInt(conf.wNId bits)
  val cId = UInt(conf.wCId bits)
  val tId = UInt(conf.wTId bits)
  val txnManId = UInt(conf.wTxnManId bits)
  val txnId = UInt(conf.wTxnId bits)
  val lkType = Bool()
  val lkUpgrade = Bool()
  val lkRelease = Bool()
  val lkIdx = UInt(conf.wLkIdx bits)
  val wLen = UInt(3 bits) // len(tuple)=2^wLen
  val respType = LockRespType()


  def setDefault() = {
    this.nId := 0
    this.cId := 0
    this.txnManId := 0
    this.txnId := 0
    this.tId := 0
    this.lkType := False
    this.lkUpgrade := False
    this.lkRelease := False
    this.lkIdx := 0
    this.wLen := 0
    this.respType := LockRespType.abort
  }

  def toLkReq(release: Bool, lkIdx: UInt): LkReq = {
    val ret = LkReq(conf)
    ret.nId := this.nId
    ret.cId := this.cId
    ret.tId := this.tId
    ret.txnManId := this.txnManId
    ret.txnId:= this.txnId
    ret.lkType := this.lkType
    ret.lkUpgrade := this.lkUpgrade
    ret.lkRelease := release
    ret.lkIdx := lkIdx
    ret.wLen := this.wLen
    ret
  }

}

class LockTableIO(conf: SysConfig) extends Bundle{
  val lkReq = slave Stream(LkReq(conf))
  val lkResp = master Stream(LkResp(conf))

  def setDefault() = {
    this.lkReq.ready := False
    this.lkResp.valid := False
    this.lkResp.setDefault()
  }
}


class LockTable(conf: SysConfig) extends Component {

  val io = new LockTableIO(conf)
  // hash table
  val ht = new HashTableDUT(conf.wTId-log2Up(conf.nLtPart), conf.wHtValNW, conf.wHtBucket, conf.wHtTable)

  io.setDefault()
  ht.io.setDefault()

  val fsm = new StateMachine {
    // stage lock_req
    val req = RegNextWhen(io.lkReq.payload, io.lkReq.fire)
    // stage ht out
    val ht_lock_entry_cast = LockEntry(conf)
    ht_lock_entry_cast.assignFromBits(ht.io.ht_res_if.found_value.asBits) // wire: cast the value of ht to lock_entry

    val ht_ram_entry_cast = RamEntry(conf)

    ht_ram_entry_cast.assignFromBits(ht.io.ht_res_if.ram_data.asBits) // BUG, MSB order

    val r_lock_resp = Reg(LockRespType())

    val INSERT_TRY = new State with EntryPoint
    val INSET_RESP, DEL_CMD, DEL_RESP, LK_RESP = new State

    INSERT_TRY
      .whenIsActive{
        val try_onwer_cnt = UInt(conf.wOwnerCnt bits)
        try_onwer_cnt := 1
        ht.io.ht_res_if.ready := True

        io.lkReq.ready := ht.io.ht_cmd_if.ready

        when(io.lkReq.valid){
          ht.io.sendCmd(io.lkReq.tId, (io.lkReq.lkType ## try_onwer_cnt).asUInt, HashTableOpCode.ins2)
        }

        when(io.lkReq.fire){
          goto(INSET_RESP)
        }
      }

    INSET_RESP
      .whenIsActive {
        ht.io.ht_res_if.ready := True
        ht.io.update_addr := ht.io.ht_res_if.find_addr
        when(ht.io.ht_res_if.fire) {
          when(!req.lkRelease) {

            ht.io.update_data := (ht_ram_entry_cast.key ## req.lkType ## (ht_ram_entry_cast.owner_cnt+1) ## ht_ram_entry_cast.next_ptr ## ht_ram_entry_cast.net_ptr_val).asUInt

            when(ht.io.ht_res_if.rescode === HashTableRetCode.ins_exist) {
              // lock exist
              when((!req.lkUpgrade && (ht_lock_entry_cast.lock_status | req.lkType)) || (req.lkUpgrade && ht_lock_entry_cast.owner_cnt > 1)) {
                r_lock_resp := LockRespType.abort // no wait
                goto(LK_RESP)
              } otherwise {
                r_lock_resp := LockRespType.grant
                // write back to ht data ram
                ht.io.update_en := True
                goto(LK_RESP)
              }
            } otherwise {
              // insert_success
              r_lock_resp := LockRespType.grant
              goto(LK_RESP)
            }
          } otherwise {

            ht.io.update_data := (ht_ram_entry_cast.key ## req.lkType ## (ht_ram_entry_cast.owner_cnt-1) ## ht_ram_entry_cast.next_ptr ## ht_ram_entry_cast.net_ptr_val).asUInt

            // lock release, ht.io.ht_res_if.rescode must be ins_exist. 2 cases: cnt-- or del entry (cost a few cycles)
            when(ht_ram_entry_cast.owner_cnt===1){
              // ht must be ready, del the entry: BUG
              goto(DEL_CMD)
            } otherwise {
              ht.io.update_en := True
              goto(LK_RESP)
            }
            r_lock_resp := LockRespType.release
          }
        }
      }


    DEL_CMD
      .whenIsActive{
        ht.io.ht_res_if.ready := True
        ht.io.sendCmd(req.tId, 0, HashTableOpCode.del)
        when(ht.io.ht_cmd_if.fire){goto(DEL_RESP)}
      }

    DEL_RESP
      .whenIsActive{
        ht.io.ht_res_if.ready := True
        when(ht.io.ht_res_if.fire){goto(LK_RESP)}
      }


    LK_RESP
      .whenIsActive{
        ht.io.ht_res_if.ready := True

        io.lkResp.valid := True
        io.lkResp.txnId := req.txnId
        io.lkResp.tId := req.tId
        io.lkResp.lkType := req.lkType
        io.lkResp.lkUpgrade := req.lkUpgrade
        io.lkResp.lkRelease := req.lkRelease
        io.lkResp.respType := r_lock_resp
        io.lkResp.lkIdx := req.lkIdx
        when(io.lkResp.fire){goto(INSERT_TRY)}
      }
  }

}
