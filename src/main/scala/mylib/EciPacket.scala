package mylib

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class TMreq0to10() extends Bundle{
  val opcode = UInt(5 bits) // OPCODE_WIDTH
  val xb4 = UInt(4 bits)
  val rreq_id = UInt(5 bits)
  val dmas = UInt(4 bits)
  val ns = Bool()
  val xb3 = UInt(3 bits)
  val xb2 = UInt(2 bits)
  val address = UInt(40 bits)
}

case class TGenericCmd() extends Bundle{
  val opcode = UInt(5 bits) // OPCODE_WIDTH
  val xb3 = UInt(3 bits)
  val xb1 = Bool()
  val rreq_id = UInt(5 bits)
  val dmas = UInt(4 bits)
  val ns = Bool()
  val xb5 = UInt(5 bits)
  val address = UInt(40 bits)
}