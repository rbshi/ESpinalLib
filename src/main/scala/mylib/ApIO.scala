package mylib

import spinal.core._
import spinal.lib._

case class ApIO() extends Bundle{
  // accelerator control signal
  val start: Bool = in Bool()
  val ready: Bool = out Bool()
  val done: Bool = out Bool()
  val idle: Bool = out Bool()

  def reqStart() : Bool = return start
  def setReady(sigVal : Bool)  = ready := sigVal
  def setDone(sigVal : Bool) = done := sigVal
  def setIdle(sigVal : Bool) = idle := sigVal

  def setDefault() = {
    idle := True
    ready := False
    done := False
  }

}
