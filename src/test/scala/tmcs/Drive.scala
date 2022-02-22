package tmcs
import spinal.core.sim._
import spinal.core._
import spinal.lib.bus.amba4.axi._
import util._

import scala.collection.mutable.ArrayBuffer

object SysSim {

  val axiMemSimConf = AxiMemorySimConfig(
    maxOutstandingReads = 128,
    maxOutstandingWrites = 128,
    readResponseDelay = 3,
    writeResponseDelay = 3
  )

  def instAxiMemSim(axi: Axi4, clockDomain: ClockDomain, memCtx: Option[Array[Byte]]) : AxiMemorySim = {
    val mem = AxiMemorySim(axi, clockDomain, axiMemSimConf)
    mem.start()
    memCtx match {
      case Some(ctx) => {
        mem.memory.writeArray(0, ctx)
      }
      case None => mem.memory.writeArray(0, Array.fill[Byte](1<<22)(0.toByte))
    }
    mem
  }



}