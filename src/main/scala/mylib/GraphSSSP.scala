package mylib

import spinal.core
import spinal.core.{UInt, _}
import spinal.lib._
import spinal.lib.bus.amba4.axi._

import scala.language.postfixOps



case class NodePtr(graphConfig : GraphConfig) extends Bundle{
  val adjAddr = UInt(graphConfig.adjAddrWidth bits)
  val adjLen = UInt(16 bits)
}


object GraphSSSP{

  /**
   * State of the state machine of the wrapper
   */
  object PhaseGeneral extends SpinalEnum{
    val IDLE, SETUP, WAIT, DONE= newElement
  }

  object PhaseReadCmd extends SpinalEnum{
    val SRCIDX, PTR, SRCDATA, ADJ, DST_WAIT, DST_LANE, DST_CMD = newElement
  }

  object PhaseReadResp extends SpinalEnum{
    val SRCIDX, PTR, SRCDATA, ADJ, DST = newElement
  }

  object PhaseProcess extends SpinalEnum{
    val PROCESS, WR_DST, WR_QUEUE = newElement
  }

  /**
   * Return the axi and bram configuration
   */
  def getConfigs(addressAxiWidth: Int, dataWidth: Int): Axi4Config =
    Axi4Config(
      addressWidth = addressAxiWidth,
      dataWidth    = dataWidth,
      idWidth = 1,
      useStrb = true,
      useBurst = true,
      useId = true,
      useLock      = false,
      useRegion    = false,
      useCache     = false,
      useProt      = false,
      useQos       = false
    )

  def getGraphConfigs(nodeIdxWidth: Int, nodePtrWidth: Int, adjAddrWidth : Int, adjLenWidth : Int, nodeDataWidth: Int): GraphConfig = GraphConfig(nodeIdxWidth, nodePtrWidth, adjAddrWidth, adjLenWidth, nodeDataWidth)

}



/**
 * Issue Axi4 write transactions in strode memory addresses with burst
 */
case class GraphSSSP(addressAxiWidth: Int, dataWidth: Int) extends Component {

  import GraphSSSP._

  val axiConfig = GraphSSSP.getConfigs(addressAxiWidth, dataWidth)
  val graphConfig = GraphSSSP.getGraphConfigs(nodeIdxWidth = 32, nodePtrWidth = 64, adjAddrWidth = 33, adjLenWidth = 16, nodeDataWidth = 64)

  val io = new Bundle {
    // axi interface
    val axi = master(Axi4(axiConfig))

    // control signal (traslated from AXI-lite)
    val addr_ptr = in UInt (axiConfig.addressWidth bits) // addr_adj is omitted, which is listed by ptr
    val addr_data = in UInt (axiConfig.addressWidth bits)
    val addr_queue = in UInt (axiConfig.addressWidth bits)

    val start_node = in UInt (graphConfig.nodeIdxWidth bits)
    // out
    val cnt_clk = out UInt (32 bits)

    // standard Xilinx accelerator control signals
    val ap = ApIO()
  }


  val nodeDstDataPush = new Stream(graphConfig.nodeDataType)
  val nodeDstDataPop = nodeDstDataPush.queue(size = 64)
  nodeDstDataPush.valid := False
  nodeDstDataPush.payload := 0
  nodeDstDataPop.ready := False


  val nodeDstIdxLanePush = new Stream(axiConfig.dataType)
  val (nodeDstIdxLanePop, occuNodeDstIdxLanePop) = nodeDstIdxLanePush.queueWithOccupancy(size = 4096)
  nodeDstIdxLanePush.valid := False
  nodeDstIdxLanePush.payload := 0
  nodeDstIdxLanePop.ready := False
  val rOccuNodeDstIdxLanePop = RegNext(occuNodeDstIdxLanePop)


  val nodeDstIdxPush1, nodeDstIdxPush2 = new Stream(graphConfig.nodeIdxType)
  val nodeDstIdxPop1 = nodeDstIdxPush1.queue(size = 64)
  val (nodeDstIdxPop2, occunodeDstIdxPop2) = nodeDstIdxPush2.queueWithOccupancy(size = 64)
  val rOccuNodeDstIdxPop2 = RegNext(occunodeDstIdxPop2)

  nodeDstIdxPush1.valid := False
  nodeDstIdxPush1.payload := 0
  nodeDstIdxPop1.ready := False
  nodeDstIdxPush2.valid := False
  nodeDstIdxPush2.payload := 0
  nodeDstIdxPop2.ready := False


  io.ap.setDefault()
  val phase_general = RegInit(PhaseGeneral.IDLE)
  val phase_readcmd = RegInit(PhaseReadCmd.SRCIDX)
  val phase_readresp = RegInit(PhaseReadResp.SRCIDX)
  val phase_process = RegInit(PhaseProcess.PROCESS)

  val rCntClk = Reg(cloneOf(io.cnt_clk)) init (0)
  io.cnt_clk := rCntClk

  val numNodePtrPerLane = axiConfig.dataWidth / graphConfig.nodePrtWidth
  val numNodeIdxPerLane = axiConfig.dataWidth / graphConfig.nodeIdxWidth
  val numNodeDataPerLane = axiConfig.dataWidth / graphConfig.nodeDataWidth



  val rNodeAdjLen, rNodeAdjBurstCnt1, rNodeAdjBurstCnt2 = Reg(UInt(graphConfig.adjLenWidth bits)) init(0)

  val rNodeAdjLenCnt1, rNodeAdjLenCnt2 = Reg(UInt(graphConfig.adjLenWidth bits)) init (0)
  val rNodeAdjAddr = Reg(UInt(graphConfig.adjAddrWidth bits)) init (0)

  val rNodeDstIdxLane = Reg(UInt(axiConfig.dataWidth bits)) init(0)

  val rNodePtrSel = Reg(UInt(log2Up(numNodePtrPerLane) bits)) init (0)
  val rNodeDataSel = Reg(UInt(log2Up(numNodeDataPerLane) bits)) init (0)


  val rNodeSrcIdx = Reg(graphConfig.nodeIdxType) init (0)
  val rNodeSrcData = Reg(graphConfig.nodeDataType) init (0)

  val nodeSrcDataPush = new Stream(graphConfig.nodeDataType)
  val nodeSrcDataPop = nodeSrcDataPush.queue(size = 64)
  nodeSrcDataPush.valid := False
  nodeSrcDataPush.payload := 0
  nodeSrcDataPop.ready := False


  val rNumAdjIdxLane = Reg(UInt(log2Up(numNodeIdxPerLane) bits)) init (0)

  val rLockAxiAw, rLockAxiW = Reg(Bool())
  val rLockReadPtr = Reg(Bool())
  val rLockReadSrcIdx = Reg(Bool())


  val axiRdAddrPush = new Stream(axiConfig.addressType)
  val axiRdAddrPop = axiRdAddrPush.queue(size = 16)
  axiRdAddrPush.valid := False
  axiRdAddrPush.payload := 0

  io.axi.readCmd.size := log2Up(axiConfig.dataWidth / 8) // FIXME: remove later, Vitis has default value.
  io.axi.readCmd.id := 0
  io.axi.readCmd.len := 0
  io.axi.readCmd.setBurstINCR()

  io.axi.readCmd.addr := axiRdAddrPop.payload
  io.axi.readCmd.valid := axiRdAddrPop.valid
  axiRdAddrPop.ready := io.axi.readCmd.ready
  io.axi.readRsp.ready := True // keep high

  io.axi.writeRsp.ready := True


  val axiWrCmdPush = new Stream(cloneOf(io.axi.writeCmd.payload))
  val axiWrDataPush = new Stream(cloneOf(io.axi.writeData.payload))
  axiWrCmdPush.valid := False

  val axiWrCmdPop = axiWrCmdPush.queue(size = 16)
  val axiWrDataPop = axiWrDataPush.queue(size = 16)

  axiWrCmdPop >> io.axi.writeCmd
  axiWrDataPop >> io.axi.writeData

  axiWrCmdPush.addr := 0
  axiWrCmdPush.size := log2Up(axiConfig.dataWidth / 8)
  axiWrCmdPush.id := 0
  axiWrCmdPush.len := 0
  axiWrCmdPush.setBurstINCR()

  axiWrDataPush.data := 0
  axiWrDataPush.valid := False
  axiWrDataPush.strb := 0
  axiWrDataPush.last := True


  val rQueueRdAddr = Reg(UInt(axiConfig.addressWidth bits)) init(0)
  val rQueueWrAddr = Reg(UInt(axiConfig.addressWidth bits)) init(0)
  val queueLen = UInt(32 bits)
  queueLen := ((rQueueWrAddr - rQueueRdAddr) >> log2Up(graphConfig.nodeIdxWidth/8)).resized

  val rCntWrIssue = Reg(UInt(32 bits)) init(0)
  val rCntWrResp = Reg(UInt(32 bits)) init(0)
  val cntWrRemain = UInt(32 bits)
  cntWrRemain := rCntWrIssue - rCntWrResp


  /**
   * Main state machine
   */
  val sm_general = new Area {
    switch(phase_general) {
      is(PhaseGeneral.IDLE) {
        when(io.ap.reqStart()) {
          phase_general := PhaseGeneral.SETUP
          io.ap.setIdle(False)
        }
      }

      is(PhaseGeneral.SETUP) {
        io.ap.setIdle(False)

        rCntClk := 0

        rQueueRdAddr := io.addr_queue
        rQueueWrAddr := io.addr_queue + graphConfig.nodeIdxWidth / 8

        phase_general := PhaseGeneral.WAIT

      }

      is(PhaseGeneral.WAIT) {
        io.ap.setIdle(False)
        rCntClk := rCntClk + 1
        when(rCntClk > 5000 && queueLen === 0) {
          phase_general := PhaseGeneral.DONE // FIXME
        }
      }

      is(PhaseGeneral.DONE) {
        io.ap.setIdle(False)
        io.ap.setReady(True)
        io.ap.setDone(True)
        phase_general := PhaseGeneral.IDLE
      }
    }


    val sm_readcmd = new Area {
      switch(phase_readcmd) {

        is(PhaseReadCmd.SRCIDX) {

          when(~(queueLen === 0) && (cntWrRemain === 0)) {
            axiRdAddrPush.payload := rQueueRdAddr
            axiRdAddrPush.valid := True
          }

          when(axiRdAddrPush.fire) {
//            rQueueRdAddr := rQueueRdAddr + 1 // update in readResp
            rLockReadSrcIdx := True
            phase_readcmd := PhaseReadCmd.PTR
          }
        }


        is(PhaseReadCmd.PTR) {
          when(~rLockReadSrcIdx){

            axiRdAddrPush.payload := io.addr_ptr + (rNodeSrcIdx << log2Up(graphConfig.nodePrtWidth / 8)).resized
            axiRdAddrPush.valid := True

            when(axiRdAddrPush.fire) {
              rLockReadPtr := True
              phase_readcmd := PhaseReadCmd.SRCDATA
            }
          }
        }


        is(PhaseReadCmd.SRCDATA) {
          axiRdAddrPush.payload := io.addr_data + (rNodeSrcIdx << log2Up(graphConfig.nodeDataWidth / 8)).resized
          axiRdAddrPush.valid := True
          when(axiRdAddrPush.fire) {
            phase_readcmd := PhaseReadCmd.ADJ
          }
        }


        is(PhaseReadCmd.ADJ) {

          when(~rLockReadPtr) {
            when(~(rNodeAdjBurstCnt1 === 0)) {
              axiRdAddrPush.payload := rNodeAdjAddr.resized
              axiRdAddrPush.valid := True
            } otherwise {
              // w/o adj node
              when(rNodeAdjLen===0){
                phase_readcmd := PhaseReadCmd.SRCIDX
              } otherwise {
                phase_readcmd := PhaseReadCmd.DST_WAIT
              }
            }
          }
          when(axiRdAddrPush.fire) {
            rNodeAdjAddr := rNodeAdjAddr + (axiConfig.dataWidth / 8)
            rNodeAdjBurstCnt1 := rNodeAdjBurstCnt1 - 1
          }
        }
        
        // avoid access the old data before the write back finish
        is(PhaseReadCmd.DST_WAIT){
          when(rOccuNodeDstIdxPop2===0){
            phase_readcmd := PhaseReadCmd.DST_LANE
          }
        }

        is(PhaseReadCmd.DST_LANE) {
          when(~(rOccuNodeDstIdxLanePop === 0)) {
            rNodeDstIdxLane := nodeDstIdxLanePop.payload.asUInt
            nodeDstIdxLanePop.ready := True // FIXME: stream pop to reg?
          }
          when(nodeDstIdxLanePop.fire) {
            phase_readcmd := PhaseReadCmd.DST_CMD
          }
        }

        is(PhaseReadCmd.DST_CMD) {
          val nodeDstIdx = rNodeDstIdxLane.subdivideIn(graphConfig.nodeIdxWidth bits)(rNodeAdjLenCnt1(log2Up(numNodeIdxPerLane)-1 downto 0))
          nodeDstIdxPush1.payload := nodeDstIdx
          axiRdAddrPush.payload := io.addr_data + (nodeDstIdx << log2Up(graphConfig.nodeDataWidth / 8)).resized // dst node data address
          axiRdAddrPush.valid := True

          when(axiRdAddrPush.fire) {
            nodeDstIdxPush1.valid := True  // FIXME: fifo is full?
            when((rNodeAdjLenCnt1 === (rNodeAdjLen -1))){
              phase_readcmd := PhaseReadCmd.SRCIDX
            } otherwise{
              rNodeAdjLenCnt1 := rNodeAdjLenCnt1 + 1
              when(rNodeAdjLenCnt1(log2Up(numNodeIdxPerLane)-1 downto 0) === ((1 << log2Up(numNodeIdxPerLane)) -1)){
                phase_readcmd := PhaseReadCmd.DST_LANE
              }
            }
          }
        }

      }
    }


    val sm_readresp = new Area {
      switch(phase_readresp) {

        is(PhaseReadResp.SRCIDX){
          when(io.axi.readRsp.fire) {
            val nodeSrcIdx = io.axi.readRsp.data.subdivideIn(graphConfig.nodeIdxWidth bits)(rQueueRdAddr(log2Up(axiConfig.dataWidth/8)-1 downto log2Up(graphConfig.nodeIdxWidth/8))).asUInt
            rNodeSrcIdx := nodeSrcIdx
            rNodePtrSel := nodeSrcIdx(log2Up(numNodePtrPerLane) - 1 downto 0)
            rNodeDataSel := nodeSrcIdx(log2Up(numNodeDataPerLane) - 1 downto 0)
            // for selecting the proper ptr data from burst width
            rQueueRdAddr := rQueueRdAddr + graphConfig.nodeIdxWidth/8
            rLockReadSrcIdx := False
            phase_readresp := PhaseReadResp.PTR
          }
        }

        is(PhaseReadResp.PTR) {
          when(io.axi.readRsp.fire) {

            val nodePtr = io.axi.readRsp.data.subdivideIn(graphConfig.nodePrtWidth bits)(rNodePtrSel).asUInt
            val nodeAdjLen = nodePtr(graphConfig.adjLenWidth + graphConfig.adjAddrWidth - 1 downto graphConfig.adjAddrWidth)

            rNodeAdjAddr := nodePtr(graphConfig.adjAddrWidth -1 downto 0)
            rNodeAdjLenCnt1 := 0 // up counting
            rNodeAdjLenCnt2 := nodeAdjLen // down counting
            rNodeAdjLen := nodeAdjLen
            val numNodeAdjBurst = (nodeAdjLen >> (log2Up(numNodeIdxPerLane))) + Mux((nodeAdjLen(log2Up(numNodeIdxPerLane) -1 downto 0) === 0), 0, 1) // FIXME: width
            rNodeAdjBurstCnt1 := numNodeAdjBurst.resized
            rNodeAdjBurstCnt2 := numNodeAdjBurst.resized

            rLockReadPtr := False
            phase_readresp := PhaseReadResp.SRCDATA
          }
        }

        is(PhaseReadResp.SRCDATA) {
          when(io.axi.readRsp.fire) {
            rNodeSrcData := io.axi.readRsp.data.subdivideIn(graphConfig.nodeDataWidth bits)(rNodeDataSel).asUInt
            when(rNodeAdjLen===0){
              phase_readresp := PhaseReadResp.SRCIDX
            } otherwise{
              phase_readresp := PhaseReadResp.ADJ
            }
          }
        }

        is(PhaseReadResp.ADJ) {

          nodeDstIdxLanePush.payload := io.axi.readRsp.data
          nodeDstIdxLanePush.valid := io.axi.readRsp.valid
          io.axi.readRsp.ready := nodeDstIdxLanePush.ready

          when(io.axi.readRsp.fire) {
            when(~(rNodeAdjBurstCnt2 === 1)){
              rNodeAdjBurstCnt2 := rNodeAdjBurstCnt2 - 1
            } otherwise{
              phase_readresp := PhaseReadResp.DST
            }
          }
        }

        is(PhaseReadResp.DST) {
          // select proper nodeDataWidth in the lane
          nodeDstDataPush.payload := io.axi.readRsp.data.subdivideIn(graphConfig.nodeDataWidth bits)(nodeDstIdxPop1.payload(log2Up(numNodeDataPerLane)-1 downto 0)).asUInt
          nodeDstDataPush.valid := io.axi.readRsp.valid

          nodeSrcDataPush.payload := rNodeSrcData
          nodeSrcDataPush.valid := io.axi.readRsp.valid


          val nodeDstIdxPop1Halt = nodeDstIdxPop1.continueWhen(io.axi.readRsp.fire)
          nodeDstIdxPop1Halt >> nodeDstIdxPush2

          when(io.axi.readRsp.fire){
            when(~(rNodeAdjLenCnt2 === 1)){
              rNodeAdjLenCnt2 := rNodeAdjLenCnt2 - 1
            } otherwise{
              phase_readresp := PhaseReadResp.SRCIDX // back to read the next NodeSrc ptr
            }
          }
        }
      }
    }



    val sm_process = new Area {

      val rAxiDstDataAddr = Reg(cloneOf(io.axi.writeCmd.addr))
      val rAxiDstDataLane = Reg(cloneOf(io.axi.writeData.data))
      val rAxiDstDataStrb = Reg(cloneOf(io.axi.writeData.strb))

      val rAxiDstIdxAddr = Reg(cloneOf(io.axi.writeCmd.addr))
      val rAxiDstIdxLane = Reg(cloneOf(io.axi.writeData.data))
      val rAxiDstIdxStrb = Reg(cloneOf(io.axi.writeData.strb))


      switch(phase_process) {
        is(PhaseProcess.PROCESS){

          when(~(rOccuNodeDstIdxPop2 === 0)){
            // pop the nodeDstData & nodeDstIdx
            nodeDstDataPop.ready := True
            nodeDstIdxPop2.ready := True
            nodeSrcDataPop.ready := True

            rAxiDstDataAddr := io.addr_data + (nodeDstIdxPop2.payload << log2Up(graphConfig.nodeDataWidth / 8)).resized // check the unaligned address is supported
            rAxiDstDataLane := ((nodeSrcDataPop.payload+1) << (nodeDstIdxPop2.payload(log2Up(numNodeDataPerLane)-1 downto 0) << log2Up(graphConfig.nodeDataWidth))).asBits.resized
            rAxiDstDataStrb := (U((1 << (graphConfig.nodeDataWidth/8))-1 , graphConfig.nodeDataWidth/8 bits) << (nodeDstIdxPop2.payload(log2Up(numNodeDataPerLane)-1 downto 0) << log2Up(graphConfig.nodeDataWidth / 8))).asBits.resized

            rAxiDstIdxAddr := rQueueWrAddr
            rAxiDstIdxLane := (nodeDstIdxPop2.payload << (rQueueWrAddr(log2Up(axiConfig.dataWidth / 8)-1 downto 0) << log2Up(8))).asBits.resized
            rAxiDstIdxStrb := (U((1 << (graphConfig.nodeIdxWidth/8))-1 , graphConfig.nodeIdxWidth/8 bits) << rQueueWrAddr(log2Up(axiConfig.dataWidth / 8)-1 downto 0)).asBits.resized

            when((nodeSrcDataPop.payload + 1) < nodeDstDataPop.payload){
              rLockAxiAw := True
              rLockAxiW := True
              phase_process := PhaseProcess.WR_DST
            }            
          }
        }

        is(PhaseProcess.WR_DST) {

          axiWrCmdPush.valid := rLockAxiAw
          axiWrDataPush.valid := rLockAxiW

          axiWrCmdPush.addr := rAxiDstDataAddr
          axiWrDataPush.data := rAxiDstDataLane
          axiWrDataPush.strb := rAxiDstDataStrb

          when(axiWrCmdPush.fire) {
            rLockAxiAw := False
          }
          when(axiWrDataPush.fire) {
            rLockAxiW := False
            rCntWrIssue := rCntWrIssue + 1
          }
          when(~rLockAxiAw && ~rLockAxiW){
            rLockAxiAw := True
            rLockAxiW := True
            phase_process := PhaseProcess.WR_QUEUE
          }
        }

        is(PhaseProcess.WR_QUEUE) {

          axiWrCmdPush.valid := rLockAxiAw
          axiWrDataPush.valid := rLockAxiW

          axiWrCmdPush.addr := rAxiDstIdxAddr
          axiWrDataPush.data := rAxiDstIdxLane
          axiWrDataPush.strb := rAxiDstIdxStrb

          when(axiWrCmdPush.fire) {
            rLockAxiAw := False
          }

          when(axiWrDataPush.fire) {
            rLockAxiW := False
            rCntWrIssue := rCntWrIssue + 1
          }

          when(~rLockAxiAw && ~rLockAxiW) {
            rLockAxiAw := True
            rLockAxiW := True
            phase_process := PhaseProcess.PROCESS

            rQueueWrAddr := rQueueWrAddr + graphConfig.nodeIdxWidth/8
          }
        }

      }
    }


    val sm_wrresp = new Area {
      when(io.axi.writeRsp.fire){
        rCntWrResp := rCntWrResp + 1
      }
    }

  }
}

object GraphSSSPMain {
  def main(args: Array[String]) {
    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC), targetDirectory = "rtl").generateVerilog(new GraphSSSP(64, 512))
  }
}