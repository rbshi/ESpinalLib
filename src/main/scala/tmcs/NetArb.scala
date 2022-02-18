package tmcs

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

//TODO: for flow control at local node, should expose lk_batch or data_batch

// local
class SendArbiter(cntTxnMan: Int, sysConf: SysConfig) extends Component {
  val io = new Bundle {
    val lkReqV = Vec(slave Stream LkReq(sysConf), cntTxnMan)
    val wrDataV = Vec(slave Stream Bits(512 bits), cntTxnMan)
    val sendQ = master Stream Bits(512 bits)
  }

  // #txnMan <= 8
  val lkReqJoin = Stream(Bits(512 bits))
  if(cntTxnMan<=8){
    lkReqJoin.arbitrationFrom(StreamJoin.vec(io.lkReqV))
    lkReqJoin.payload := io.lkReqV.asBits.resized
  } else {SpinalError("Only support #txnMan <= 8 now!")}

  val rWrLen = Vec(Reg(UInt(3 bits)), cntTxnMan) // maxLen = 64B << 7 = 8192 B
  val cntBeat = Reg(UInt(8 bits)).init(0)

  val mskWr = Bits(cntTxnMan bits)
  for (i <- mskWr.bitsRange){
    mskWr(i) := io.lkReqV(i).lkRelease && io.lkReqV(i).lkType
  }

  val fsm = new StateMachine {
    val LKREQ = new State with EntryPoint
    val WRDATA = new State

    val rMskWr = Reg(Bits(cntTxnMan bits)).init(0)

    LKREQ.whenIsActive{
      io.sendQ << lkReqJoin
      when(io.sendQ.fire){
        rMskWr := mskWr
        (rWrLen, io.lkReqV).zipped.foreach(_ := _.wLen)
      }
      when(mskWr.orR) (goto(WRDATA))
    }

    WRDATA.whenIsActive{
      // find the first txnMan that needs wr
      val ohTxnMan = OHMasking.first(rMskWr)
      val idTxnMan = OHToUInt(ohTxnMan)
      val nBeat: UInt = U(1) << rWrLen(idTxnMan)

      // connect wrData to the target txnMan and cnt
      io.sendQ << io.wrDataV(idTxnMan)
      when(io.sendQ.fire){
        cntBeat := cntBeat + 1
        when(cntBeat === (nBeat-1)) {
          // clear the bit
          rMskWr(idTxnMan).clear()
          cntBeat := 0
        }
      }

      // no more write
      when(~rMskWr.orR) (goto(LKREQ))
    }

  }
}

class RecvDispatcher(cntTxnMan: Int, sysConf: SysConfig) extends Component {

  val io = new Bundle {
    val recvQ = master Stream Bits(512 bits)
    val lkRespV = Vec(master Stream LkResp(sysConf), cntTxnMan)
    val rdDataV = Vec(master Stream Bits(512 bits), cntTxnMan)
  }

  val cntBeat = Reg(UInt(8 bits)).init(0)

  val fsm = new StateMachine {
    val LKRESP = new State with EntryPoint
    val LKDISPATCH, RDDATA = new State

    val cntDisp = Counter(cntTxnMan)

    // cast to bit vectors
    val rLkResp = Vec(Reg(LkResp(sysConf)), cntTxnMan)
    val lkRespBitV = io.recvQ.payload(widthOf(rLkResp) - 1 downto 0).subdivideIn(SlicesCount(cntTxnMan))

    val rMskRd = Reg(Bits(cntTxnMan bits)).init(0)
    val mskRd = Bits(cntTxnMan bits)
    for (i <- mskRd.bitsRange) {
      // if lockReq of Rd is granted, consume the followup read data
      mskRd(i) := ~rLkResp(i).lkRelease && ~rLkResp(i).lkType && (rLkResp(i).respType === LockRespType.grant)
    }

    LKRESP.whenIsActive {
      io.recvQ.ready.set()
      when(io.recvQ.fire) {
        rMskRd := mskRd
        // cast to LkResp entry
        (rLkResp, lkRespBitV).zipped.foreach(_.assignFromBits(_))
      }
    }

    LKDISPATCH.whenIsActive {
      val idTxnMan = rLkResp(cntDisp).txnManId
      io.lkRespV(idTxnMan).valid.set()
      when(io.lkRespV(idTxnMan).fire) {
        cntDisp.increment()

        when(cntDisp.willOverflow) {
          switch(rMskRd.orR) {
            is(True)(goto(RDDATA))
            is(False)(goto(LKRESP))
          }
        }
      }
    }

    RDDATA.whenIsActive {

      // find the first txnMan that needs wr
      val ohTxnMan = OHMasking.first(rMskRd)
      val idTxnMan = OHToUInt(ohTxnMan)
      val nBeat: UInt = U(1) << rLkResp(idTxnMan).wLen
      val wireMskRd = cloneOf(rMskRd)

      io.rdDataV(idTxnMan) << io.recvQ

      when(io.recvQ.fire) {
        cntBeat := cntBeat + 1
        when(cntBeat === (nBeat - 1)) {
          // clear the bit
          rMskRd(idTxnMan).clear()
          wireMskRd(idTxnMan) := False // one cycle earlier, to avoid over write
          cntBeat := 0
        }
      }

      // no more rd
      when(~wireMskRd.orR)(goto(LKRESP))
    }
  }

}

// remote
class ReqDispatcher(cntTxnMan: Int, sysConf: SysConfig) extends Component {

  val io = new Bundle {
    val reqQ = master Stream Bits(512 bits)
    val lkReq = master Stream LkReq(sysConf)
    val wrData = master Stream Bits(512 bits)
  }

  val cntBeat = Reg(UInt(8 bits)).init(0)

  val fsm = new StateMachine {
    val LKREQ = new State with EntryPoint
    val LKREQFIRE, WRDATA = new State
    
    val cntFire = Counter(cntTxnMan)

    // cast to bit vectors
    val rLkReq = Vec(Reg(LkReq(sysConf)), cntTxnMan)
    val lkReqBitV = io.reqQ.payload(widthOf(rLkReq) - 1 downto 0).subdivideIn(SlicesCount(cntTxnMan))

    val rMskWr = Reg(Bits(cntTxnMan bits)).init(0)
    val MskWr = Bits(cntTxnMan bits)
    for (i <- MskWr.bitsRange) {
      // if lockReq of Rd is granted, consume the followup read data
      MskWr(i) := rLkReq(i).lkRelease && rLkReq(i).lkType
    }

    LKREQ.whenIsActive {
      io.reqQ.ready.set()
      when(io.reqQ.fire) {
        rMskWr := MskWr
        // cast to LkResp entry
        (rLkReq, lkReqBitV).zipped.foreach(_.assignFromBits(_))
      }
    }

    LKREQFIRE.whenIsActive {
      io.lkReq.valid.set()
      io.lkReq.payload := rLkReq(cntFire)
      
      when(io.lkReq.fire) {
        cntFire.increment()
        when(cntFire.willOverflow) {
          switch(rMskWr.orR) {
            is(True)(goto(WRDATA))
            is(False)(goto(LKREQ))
          }          
        }
      }
    }

    WRDATA.whenIsActive {

      // find the first txnMan that needs wr
      val ohTxnMan = OHMasking.first(rMskWr)
      val idTxnMan = OHToUInt(ohTxnMan)
      val nBeat: UInt = U(1) << rLkReq(idTxnMan).wLen
      val wireMskWr = cloneOf(rMskWr)

      io.wrData << io.reqQ

      when(io.reqQ.fire) {
        cntBeat := cntBeat + 1
        when(cntBeat === (nBeat - 1)) {
          // clear the bit
          rMskWr(idTxnMan).clear()
          wireMskWr(idTxnMan) := False // one cycle earlier, to avoid over write
          cntBeat := 0
        }
      }

      // no more rd
      when(~wireMskWr.orR)(goto(LKREQ))
    }
  }

}


// TODO: RespArbiter & RecvDispatcher should pack the lock with 8, non-related to cntTxnMan. (Here just in case the #remote_lk is not multiple of 8)
class RespArbiter(cntTxnMan: Int, sysConf: SysConfig) extends Component {
  val io = new Bundle {
    val lkResp = slave Stream LkResp(sysConf)
    val rdData = slave Stream Bits(512 bits)
    val respQ = master Stream Bits(512 bits)
  }

  val cntBeat = Reg(UInt(12 bits)).init(0)

  val fsm = new StateMachine {
    val LKRESP = new State with EntryPoint
    val WRDATA = new State

    LKRESP.whenIsActive {
      val lkRespSlowDown = io.lkResp.slowdown(cntTxnMan)
      io.respQ.arbitrationFrom(lkRespSlowDown)
      io.respQ.payload := lkRespSlowDown.asBits.resized

      // record cntBeat
      when(io.lkResp.fire && io.lkResp.lkRelease && ~io.lkResp.lkType && (io.lkResp.respType === LockRespType.grant)) {
        cntBeat := cntBeat + (U(1) << io.lkResp.wLen)
      }

      when(io.respQ.fire) {
        when(cntBeat =/= 0) {
          cntBeat := 0
          goto(WRDATA)
        }
      }
    }

    WRDATA.whenIsActive {
      io.respQ << io.rdData
      when(io.respQ.fire) {
        cntBeat := cntBeat - 1
        when(cntBeat === 1)(goto(LKRESP))
      }
    }
  }
}














































