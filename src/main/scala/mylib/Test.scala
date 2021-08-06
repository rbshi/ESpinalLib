package mylib

import spinal.core
import spinal.core.{UInt, _}
import spinal.lib._

class Test extends Component {

  val io = new Bundle {
    val output = out UInt(32 bits)
  }

  val cnt = Reg(UInt(32 bits)) init(0)

  io.output := cnt

  val inst = Inst(cnt, 5)

  inst.l_exp := inst.l_exp + inst.r_exp2

}


case class Inst[T <: Data](l_exp: T, r_exp: UInt){
  val r_exp2 = this.r_exp + 1
}

object TestMain {
  def main(args: Array[String]) {
    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC), targetDirectory = "rtl").generateVerilog(new Test)
  }
}
