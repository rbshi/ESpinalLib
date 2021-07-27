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
    val GETNODE, PTR, SRCDATA, WAITPTR, ADJ, DST_LANE, DST_CMD = newElement
  }

  object PhaseReadResp extends SpinalEnum{
    val PTR, SRCDATA, ADJ, DST = newElement
  }

  object PhaseProcess extends SpinalEnum{
    val PROCESS, WRDST = newElement
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

    val start_node = in UInt (graphConfig.nodeIdxWidth bits)
    // out
    val cnt_clk = out UInt (32 bits)

    // standard Xilinx accelerator control signals
    val ap = ApIO()
  }

  val axiRdAddrPush = new Stream(axiConfig.addressType)
  val axiRdAddrPop = axiRdAddrPush.queue(size = 16)
  axiRdAddrPush.valid := False
  axiRdAddrPush.payload := 0


//  val axiWrAddrPush = new Stream(axiConfig.addressType)
//  val axiWrAddrPop = axiWrAddrPush.queue(size = 16)
//  axiWrAddrPush.valid := False
//  axiWrAddrPush.payload := 0

  val nodeSrcIdxPush = new Stream(graphConfig.nodeIdxType)
  val (nodeSrcIdxPop, occuNodeSrcIdxPop) = nodeSrcIdxPush.queueWithOccupancy(size = 1024)
  nodeSrcIdxPush.valid := False
  nodeSrcIdxPush.payload := 0
  nodeSrcIdxPop.ready := False
  val rOccuNodeSrcIdxPop = RegNext(occuNodeSrcIdxPop)



  val nodeDstDataPush = new Stream(graphConfig.nodeDataType)
  val nodeDstDataPop = nodeDstDataPush.queue(size = 1024)
  nodeDstDataPush.valid := False
  nodeDstDataPush.payload := 0
  nodeDstDataPop.ready := False


  val nodeDstIdxLanePush = new Stream(axiConfig.dataType)
  val (nodeDstIdxLanePop, occuNodeDstIdxLanePop) = nodeDstIdxLanePush.queueWithOccupancy(size = 1024)
  nodeDstIdxLanePush.valid := False
  nodeDstIdxLanePush.payload := 0
  nodeDstIdxLanePop.ready := False
  val rOccuNodeDstIdxLanePop = RegNext(occuNodeDstIdxLanePop)


  val nodeDstIdxPush2, nodeDstIdxPush3 = new Stream(graphConfig.nodeIdxType)
  val nodeDstIdxPop2 = nodeDstIdxPush2.queue(size = 1024)
  val (nodeDstIdxPop3, occuNodeDstIdxPop3) = nodeDstIdxPush3.queueWithOccupancy(size = 1024)
  val rOccuNodeDstIdxPop3 = RegNext(occuNodeDstIdxPop3)

  nodeDstIdxPush2.valid := False
  nodeDstIdxPush2.payload := 0
  nodeDstIdxPop2.ready := False
  nodeDstIdxPush3.valid := False
  nodeDstIdxPush3.payload := 0
  nodeDstIdxPop3.ready := False


  io.ap.setDefault()
  val phase_general = RegInit(PhaseGeneral.IDLE)
  val phase_readcmd = RegInit(PhaseReadCmd.GETNODE)
  val phase_readresp = RegInit(PhaseReadResp.PTR)
  val phase_process = RegInit(PhaseProcess.PROCESS)

  val rCntClk = Reg(cloneOf(io.cnt_clk)) init (0)
  io.cnt_clk := rCntClk

  val numNodePtrPerLane = axiConfig.dataWidth / graphConfig.nodePrtWidth
  val numNodeIdxPerLane = axiConfig.dataWidth / graphConfig.nodeIdxWidth
  val numNodeDataPerLane = axiConfig.dataWidth / graphConfig.nodeDataWidth


  val rNodePtr = Reg(UInt(graphConfig.nodePrtWidth bits)) init (0)
  val NodeAdjLen = UInt(graphConfig.adjLenWidth bits)
  NodeAdjLen := rNodePtr(graphConfig.adjLenWidth + graphConfig.adjAddrWidth - 1 downto graphConfig.adjAddrWidth)
  val rNodeAdjBurstCnt1, rNodeAdjBurstCnt2 = Reg(UInt(16 bits)) init (0)

  val rNodeAdjLenCnt1, rNodeAdjLenCnt2 = Reg(UInt(graphConfig.adjLenWidth bits)) init (0)
  val rNodeAdjAddr = Reg(UInt(graphConfig.adjAddrWidth bits)) init (0)

  val rNodeDstIdxLane = Reg(UInt(axiConfig.dataWidth bits)) init(0)

  val rNodePtrSel = Reg(UInt(log2Up(numNodePtrPerLane) bits)) init (0)
  val rNodeDataSel = Reg(UInt(log2Up(numNodeDataPerLane) bits)) init (0)
  val rAxiRdAddr = Reg(UInt(axiConfig.addressWidth bits)) init (0)



  val rNodeSrcIdx = Reg(graphConfig.nodeIdxType) init (0)
  val rNodeSrcData = Reg(graphConfig.nodeDataType) init (0)

  val rNumAdjIdxLane = Reg(UInt(log2Up(numNodeIdxPerLane) bits)) init (0)

  val rAxiAw = Reg(cloneOf(io.axi.writeCmd))
  val rAxiW = Reg(cloneOf(io.axi.writeData))
  val rLockAxiAw, rLockAxiW = Reg(Bool())

  val rLockReadPtr = Reg(Bool())




  io.axi.readCmd.size := log2Up(axiConfig.dataWidth / 8) // FIXME: remove later, Vitis has default value.
  io.axi.readCmd.id := 0
  io.axi.readCmd.len := 0
  io.axi.readCmd.setBurstINCR()

  io.axi.readCmd.addr := axiRdAddrPop.payload
  io.axi.readCmd.valid := axiRdAddrPop.valid
  axiRdAddrPop.ready := io.axi.readCmd.ready
  io.axi.readRsp.ready := True // keep high

  io.axi.writeCmd.size := log2Up(axiConfig.dataWidth / 8)
  io.axi.writeCmd.id := 0
  io.axi.writeCmd.len := 0
  io.axi.writeCmd.setBurstINCR()

  io.axi.writeCmd.addr := rAxiAw.addr
  io.axi.writeCmd.valid := rAxiAw.valid
  rAxiAw.ready := io.axi.writeCmd.ready

  io.axi.writeData.data := rAxiW.data
  io.axi.writeData.valid := rAxiW.valid
  io.axi.writeData.strb := rAxiW.strb
  io.axi.writeData.last := True
  io.axi.writeRsp.ready := True






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

        nodeSrcIdxPush.valid := True
        nodeSrcIdxPush.payload := io.start_node

        when(nodeSrcIdxPush.fire) {
          phase_general := PhaseGeneral.WAIT
        }
      }

      is(PhaseGeneral.WAIT) {
        io.ap.setIdle(False)
        rCntClk := rCntClk + 1
        when(rCntClk > 500 && occuNodeSrcIdxPop === 0) {
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
        is(PhaseReadCmd.GETNODE) {
          when(~(rOccuNodeSrcIdxPop === 0)) {
            nodeSrcIdxPop.ready := True // pop a node
            // for selecting the proper ptr data from burst width
            rNodePtrSel := nodeSrcIdxPop.payload(log2Up(numNodePtrPerLane) - 1 downto 0)
            rNodeDataSel := nodeSrcIdxPop.payload(log2Up(numNodeDataPerLane) - 1 downto 0)
            rNodeSrcIdx := nodeSrcIdxPop.payload

            rAxiRdAddr := io.addr_ptr + (nodeSrcIdxPop.payload << log2Up(graphConfig.nodePrtWidth / 8)).resized
            phase_readcmd := PhaseReadCmd.PTR
          }
        }



        is(PhaseReadCmd.PTR) {
          axiRdAddrPush.payload := rAxiRdAddr
          axiRdAddrPush.valid := True
          when(axiRdAddrPush.fire) {
            // addr of src data
            rAxiRdAddr := io.addr_data + (rNodeSrcIdx << log2Up(graphConfig.nodeDataWidth / 8)).resized
            rLockReadPtr := True

            phase_readcmd := PhaseReadCmd.SRCDATA
          }
        }

        is(PhaseReadCmd.SRCDATA) {
          axiRdAddrPush.payload := rAxiRdAddr
          axiRdAddrPush.valid := True
          when(axiRdAddrPush.fire) {
            phase_readcmd := PhaseReadCmd.WAITPTR
          }
        }

        is(PhaseReadCmd.WAITPTR) {
          // lock with rLockReadPtr
          when(~rLockReadPtr) {
            rNodeAdjAddr := rNodePtr(graphConfig.adjAddrWidth -1 downto 0)
            rNodeAdjLenCnt1 := 0 // up counting
            rNodeAdjLenCnt2 := NodeAdjLen // down counting
            val numNodeAdjBurst = (NodeAdjLen >> (log2Up(numNodeIdxPerLane))) + Mux((NodeAdjLen(log2Up(numNodeIdxPerLane) -1 downto 0) === 0), 0, 1) // FIXME: width
            rNodeAdjBurstCnt1 := numNodeAdjBurst.resized
            rNodeAdjBurstCnt2 := numNodeAdjBurst.resized

            phase_readcmd := PhaseReadCmd.ADJ
          }
        }

        is(PhaseReadCmd.ADJ) {
          when(~(rNodeAdjBurstCnt1 === 0)) {
            axiRdAddrPush.payload := rNodeAdjAddr.resized
            axiRdAddrPush.valid := True
          }otherwise{
            // w/o adj node
            phase_readcmd := PhaseReadCmd.DST_LANE
          }

          when(axiRdAddrPush.fire) {
            rNodeAdjAddr := rNodeAdjAddr + (axiConfig.dataWidth / 8)
            rNodeAdjBurstCnt1 := rNodeAdjBurstCnt1 - 1
          }
        }

        is(PhaseReadCmd.DST_LANE) {
          when(~(rOccuNodeDstIdxLanePop === 0)) {
            rNodeDstIdxLane := nodeDstIdxLanePop.payload.asUInt
            nodeDstIdxLanePop.ready := True
          }
          when(nodeDstIdxLanePop.fire) {
            phase_readcmd := PhaseReadCmd.DST_CMD
          }
        }

        is(PhaseReadCmd.DST_CMD) {
          val nodeDstIdx = rNodeDstIdxLane.subdivideIn(graphConfig.nodeIdxWidth bits)(rNodeAdjLenCnt1(log2Up(numNodeIdxPerLane)-1 downto 0)) // FIXME: correct usage?
          nodeDstIdxPush2.payload := nodeDstIdx
          axiRdAddrPush.payload := io.addr_data + (nodeDstIdx << log2Up(graphConfig.nodeDataWidth / 8)).resized // dst node data address
          axiRdAddrPush.valid := True

          when(axiRdAddrPush.fire) {
            when(~(rNodeAdjLenCnt1 === (NodeAdjLen -1))){
              rNodeAdjLenCnt1 := rNodeAdjLenCnt1 + 1
              nodeDstIdxPush2.valid := True // for selecting the dst data from lane
              when(rNodeAdjLenCnt1(log2Up(numNodeIdxPerLane)-1 downto 0) === ((1 << log2Up(numNodeIdxPerLane)) -1)){
                phase_readcmd := PhaseReadCmd.DST_LANE
              }
            } otherwise{
              nodeDstIdxPush2.valid := True // for selecting the dst data from lane // FIXME
              phase_readcmd := PhaseReadCmd.GETNODE
            }
          }
        }

      }
    }



    val sm_readresp = new Area {
      switch(phase_readresp) {
        is(PhaseReadResp.PTR) {
          when(io.axi.readRsp.fire) {
            rNodePtr := io.axi.readRsp.data.subdivideIn(graphConfig.nodePrtWidth bits)(rNodePtrSel).asUInt
            rLockReadPtr := False
            phase_readresp := PhaseReadResp.SRCDATA
          }
        }

        is(PhaseReadResp.SRCDATA) {
          when(io.axi.readRsp.fire) {
            rNodeSrcData := io.axi.readRsp.data.subdivideIn(graphConfig.nodeDataWidth bits)(rNodeDataSel).asUInt
            phase_readresp := PhaseReadResp.ADJ
          }
        }

        is(PhaseReadResp.ADJ) {

          nodeDstIdxLanePush.payload := io.axi.readRsp.data

          when(~(rNodeAdjBurstCnt2 === 0)){
            when(io.axi.readRsp.fire) {
              nodeDstIdxLanePush.valid := True
              rNodeAdjBurstCnt2 := rNodeAdjBurstCnt2 - 1
            }
          } otherwise {
            phase_readresp := PhaseReadResp.DST
          }
        }

        is(PhaseReadResp.DST) {
          // select proper nodeDataWidth in the lane
          nodeDstDataPush.payload := io.axi.readRsp.data.subdivideIn(graphConfig.nodeDataWidth bits)(nodeDstIdxPop2.payload(log2Up(numNodeDataPerLane)-1 downto 0)).asUInt

          nodeDstIdxPush3.payload := nodeDstIdxPop2.payload

          when(~(rNodeAdjLenCnt2 === 0)){
            when(io.axi.readRsp.fire) {

              nodeDstDataPush.valid := True
              nodeDstIdxPop2.ready := True // pop one nodeDstIdx
              nodeDstIdxPush3.valid := True // push the nodeDstIdx to stream3 (write back purpose)

              rNodeAdjLenCnt2 := rNodeAdjLenCnt2 - 1
            }
          } otherwise {
            phase_readresp := PhaseReadResp.PTR // back to read the next NodeSrc ptr
          }
        }
      }
    }



    val sm_process = new Area {
      switch(phase_process) {
        is(PhaseProcess.PROCESS){
          when(~(rOccuNodeDstIdxPop3 === 0)){
            // pop the nodeDstData & nodeDstIdx
            nodeDstDataPop.ready := True
            nodeDstIdxPop3.ready := True

            rAxiAw.addr := io.addr_data + (nodeDstIdxPop3.payload << log2Up(graphConfig.nodeDataWidth / 8)).resized // check the the unaligned address is supported
            rAxiW.strb := ((U((1 << log2Up(graphConfig.nodeDataWidth/8))-1 , graphConfig.nodeDataWidth/8 bits) << nodeDstIdxPop3.payload(log2Up(numNodeDataPerLane)-1 downto 0)) << log2Up(graphConfig.nodeDataWidth / 8)).asBits.resized

            // push to nodeSrcIdx
            nodeSrcIdxPush.payload := nodeDstIdxPop3.payload

            // behavior (update dstNode)
            rAxiW.data := (((nodeDstDataPop.payload+1) << nodeDstIdxPop3.payload(log2Up(numNodeDataPerLane)-1 downto 0)) << log2Up(graphConfig.nodeDataWidth)).asBits.resized
            when((rNodeSrcData + 1) < nodeDstDataPop.payload){
              rAxiAw.valid := True
              rAxiW.valid := True

              rLockAxiAw := True
              rLockAxiW := True

              nodeSrcIdxPush.valid := True
              phase_process := PhaseProcess.WRDST
            }
          }
        }

        is(PhaseProcess.WRDST) {
          when(io.axi.writeCmd.fire) {
            rAxiAw.valid := False
            rLockAxiAw := False
          }
          when(io.axi.writeData.fire) {
            rAxiW.valid := False
            rLockAxiW := False
          }
          when(~rLockAxiAw && ~rLockAxiW){
            phase_process := PhaseProcess.PROCESS
          }
        }

      }
    }

  }
}

object GraphSSSPMain {
  def main(args: Array[String]) {
    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC), targetDirectory = "rtl").generateVerilog(new GraphSSSP(64, 512))
  }
}