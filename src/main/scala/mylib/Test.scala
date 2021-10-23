package mylib

import spinal.core
import spinal.core.{UInt, _}
import spinal.lib._
import spinal.lib.fsm._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable


object TypeInstruction extends Enumeration {
  type TypeInstruction = Value
  val QueuePop, QueuePush, ListRead, ListWrite = Value
}

object TypeBlock extends Enumeration {
  type TypeBlock = Value
  val Inst, BASIC, LOOP_FOR, LOOP_WHILE, IF = Value
}

import TypeBlock._, TypeInstruction._


// block is the root structure in instruction hierarchy
trait Block{
  def typeBlock : TypeBlock
}

class BasicBlock(val typeBlock: TypeBlock) extends Component with Block{

  // instruction or nested bb list
  val inst = ArrayBuffer[Block]()

  def addInst(instruction: Inst): Unit = {
    inst += instruction
  }

  def addBlock(basicBlock: BasicBlock): Unit = {
    inst += basicBlock
  }
}

case class LoopForBlock(tripCount: Int) extends BasicBlock(LOOP_FOR) {
}

case class LoopWhileBlock(loopCond: Bool) extends BasicBlock(LOOP_WHILE) {
}

case class IfBlock(ifCond: Bool) extends BasicBlock(LOOP_FOR) {
}



class Inst(inst_type: TypeInstruction) extends Component with Block{
  override def typeBlock = Inst
}

case class InstQueuePop[T <: Data](l_exp: T, r_exp: ObjQueue) extends Inst(QueuePop){
}

case class InstQueuePush[T <: Data](l_exp: T, r_exp: ObjQueue) extends Inst(QueuePush){
}

case class InstListRead[TDst <: Data, TAddr <: Data](l_exp: TDst, r_exp1: ObjList, r_exp2: TAddr) extends Inst(ListRead){
}

case class InstListWrite[T <: Data](l_exp: T, r_exp: ObjQueue) extends Inst(ListWrite){
}


// in DRAM data structure
case class ObjMemoryConfig(addrWidth: Int, laneWidth: Int, wordWidth: Int, startAddr: Long, lenByte: Long) {
}


class ObjMemory(objMemConfig: ObjMemoryConfig) extends Component{
}

case class ObjQueue(objMemConfig: ObjMemoryConfig) extends ObjMemory(objMemConfig) {
  val rQueueWrAddr = Reg(UInt(objMemConfig.addrWidth bits)) init(0)
  val rQueueRdAddr = Reg(UInt(objMemConfig.addrWidth bits)) init(0)
  val lenQueue : UInt = ((rQueueWrAddr - rQueueRdAddr) >> log2Up(objMemConfig.wordWidth/8)).resized
}

case class ObjList(objMemConfig: ObjMemoryConfig) extends ObjMemory(objMemConfig) {
}



// hardware class
case class ObjFifo[T <: Data](val payloadType :  HardType[T], size : Int) extends Component {

  val push = new Stream(payloadType)
  val (pop, occuPop) = push.queueWithOccupancy(size = this.size)

  def init() = {
    push.valid := False
//    push.payload := 0 // FIXME
    push.ready := False
  }
}






// hardware
class Test extends Component {

  val io = new Bundle {
    val output = out UInt(32 bits)
  }

  val cnt = Reg(UInt(32 bits)) init(0)
  io.output := cnt

  val queueMemConfig = ObjMemoryConfig(addrWidth = 64, laneWidth = 512, wordWidth = 32, startAddr = 0, lenByte = 100)
  val listMemConfig = ObjMemoryConfig(addrWidth = 64, laneWidth = 512, wordWidth = 32, startAddr = 1024, lenByte = 100)

  // global hardware objects
  val queue = ObjQueue(queueMemConfig)
  val list = ObjList(listMemConfig)

  val a = Reg(UInt(32 bits))
  val b = UInt(32 bits)

  val fifoMap = mutable.Map[Data, Any]()

  // fixed hardware interface
  val read_cmd = new StateMachine
//  val read_resp = new StateMachine


  /* manually dataflow construction
  * Loop1 (20) {
  *   a << queue
  *   b = list[a] (a is the address with type(list))
  *   If (b > 1) {
  *     b >> queue
  *   }
  * }
  * */

  val glbBB = new BasicBlock(BASIC)

  val loop1 = LoopForBlock(20) // loop-20
  glbBB.addBlock(loop1)

  loop1.addInst(InstQueuePop(a, queue))  // a << queue
  loop1.addInst(InstListRead(b, list, a)) //

  val if1 = IfBlock(b > 1)
  loop1.addBlock(if1)
  if1.addInst(InstQueuePush(b, queue))

  println("[INFO] Instruction list done.")

  // Hardware generation
  for (block <- glbBB.inst) {
    GenHw(block)
  }

//  read_cmd.add(new State()(read_cmd) with EntryPoint {
//    whenIsActive(
//      when(cnt < 10) {
//        cnt := cnt + 1
//      }
//    )
//  })

//  val fsm = new StateMachine {}
//    val st = new State()(fsm) with EntryPoint {
//      whenIsActive(
//        when(cnt < 10) {
//          cnt := cnt + 1
//        }
//      )
//    }




  def GenHw(block: Block): Unit ={

    block match {

      case _: Inst => {
        println("[INFO] I'm inst.")
        val inst = block.asInstanceOf[Inst]

        inst match {

          case InstQueuePop(l_exp, r_exp) => {
            println("[INFO] I'm inst InstQueuePop.")


            fifoMap += (l_exp -> ObjFifo(UInt(32 bits), 16))

            val st = new State()(read_cmd) with EntryPoint {
              whenIsActive(
                when(cnt < 10) {
                  cnt := cnt + 1
                }
              )
            }

          }

          case _ => {}
        }
      }


      case LoopForBlock(tripCount) => {
        println("[INFO] I'm loop.")
        for (inner <- block.asInstanceOf[LoopForBlock].inst) {
          GenHw(inner)
        }
      }

      case IfBlock(cond) => {
        println("[INFO] I'm if.")
        for (inner <- block.asInstanceOf[IfBlock].inst) {
          GenHw(inner)
        }
      }
    }




  }


}







object TestMain {
  def main(args: Array[String]) {
    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC), targetDirectory = "rtl").generateVerilog(new Test)
  }
}
