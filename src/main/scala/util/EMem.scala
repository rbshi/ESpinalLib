package util

import spinal.core.{UInt, _}
import spinal.core.Mem
import spinal.lib._

import scala.language.postfixOps

/*
* Extended memory
* */


class EMem(wordType: UInt, wordCount: Int) extends Component {

  val io = new Bundle {
    val addr = in UInt(log2Up(wordCount) bits)
    val addVal = in cloneOf(wordType)
    val enAdd = in Bool()
    val enRd = in Bool()
    val data = out cloneOf(wordType)

    def setAdd(addr: UInt, addVal: UInt): Unit = {
      this.addr := addr
      this.addVal := addVal
      this.enAdd := True
      this.enRd := False
    }

    def setRead(addr: UInt): Unit = {
      this.enAdd := False
      this.enRd := True
      this.addr := addr
    }
  }

  var initContent: Seq[UInt] = Seq()
  for (i <- 0 until wordCount){
    val zero: UInt = 0
    initContent = initContent :+ zero
  }

  val mem = Mem(wordType, wordCount).init(initContent)

  val rRdAddr = RegNext(io.addr)
  val rAddVal = RegNext(io.addVal)
  val wrEn = RegNext(io.enAdd)

  val rdDataMux, wrData = cloneOf(wordType)
  val wrDataD = RegNext(wrData)

  val rdData = mem.readSync(io.addr, io.enRd || io.enAdd)

  val rdSel = RegNext(io.addr === rRdAddr && wrEn)

  when(rdSel)(rdDataMux:=wrDataD) otherwise(rdDataMux:=rdData)
  io.data := rdDataMux

  wrData := rdDataMux + rAddVal
  when(wrEn)(mem.write(rRdAddr, wrData))

}
