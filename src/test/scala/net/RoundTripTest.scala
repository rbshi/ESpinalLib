package net

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._
import spinal.lib.bus.amba4.axi.sim._
import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.Stream

import scala.collection._


class RoundTripTest extends AnyFunSuite {

  def setAxi4LiteReg(dut: Component, bus: AxiLite4, addr: Int, data: Int): Unit ={
    val awa = fork {
      bus.aw.addr #= addr
      bus.w.data #= data
      bus.w.strb #= 0xF // strb for 4 Bytes
      bus.aw.valid #= true
      bus.w.valid #= true
      while (!bus.aw.ready.toBoolean || !bus.w.ready.toBoolean)
        dut.clockDomain.waitSampling()
      bus.aw.valid #= false
      bus.w.valid #= false
    }

    val b = fork {
      bus.b.ready #= true
      while (!bus.b.valid.toBoolean)
        dut.clockDomain.waitSampling()
      bus.b.ready #= false
    }

    awa.join()
    b.join()

  }

  def genNotif(sid:BigInt, length:BigInt, ipaddr:BigInt, dstport:BigInt, closed:BigInt): BigInt ={
    val data: BigInt = sid + (length << 16) + (ipaddr << (16+16)) + (dstport << (16+16+32)) + (closed << (16+16+32+16))
    data
  }

  def genTxStatus(sid:BigInt, length:BigInt, remaining_space:BigInt, error:BigInt): BigInt ={
    val data : BigInt = sid + (length << 16) + (remaining_space << (16+16)) + (error << (16+16+30))
    data
  }

  def genRxData(data: BigInt, last: Boolean) : BigInt = {
    val rxdata: BigInt = data + ((BigInt(2).pow(64) - 1) << 512) + (BigInt((last).toInt) << (512+64))
    rxdata
  }


  // fixme: currently I cannot use [T], since #= does not support
  def sendOnStream(dut: Component, stream: Stream[Bits], data: BigInt): Unit ={
    stream.valid #= true
    stream.payload #= data
    dut.clockDomain.waitSamplingWhere(stream.valid.toBoolean && stream.ready.toBoolean)
    stream.valid #= false
  }

  def sendOnStream(dut: Component, stream: Stream[Bool], data: Boolean): Unit ={
    stream.valid #= true
    stream.payload #= data
    dut.clockDomain.waitSamplingWhere(stream.valid.toBoolean && stream.ready.toBoolean)
    stream.valid #= false
  }

  def recOnStream[T <: Data](dut: Component, stream: Stream[T]): T = {
    stream.ready #= true
    dut.clockDomain.waitSamplingWhere(stream.valid.toBoolean && stream.ready.toBoolean)
    stream.ready #= false
    stream.payload
  }


  def round_trip(dut: RoundTrip): Unit ={
    dut.clockDomain.forkStimulus(period = 10)

    var f_open_port = false
    var f_open_connect = false
    var f_init = false

    val init = fork {
      dut.io.net.port_status.valid #= false
      dut.io.net.notification.valid #= false
      dut.io.net.rx_meta.valid #= false
      dut.io.net.rx_data.valid #= false
      dut.io.net.open_status.valid #= false
      dut.io.net.tx_status.valid #= false


      dut.clockDomain.waitSampling(10)
      setAxi4LiteReg(dut, dut.io.control, 0x20, 1) // pkgCounter
      setAxi4LiteReg(dut, dut.io.control, 0x18, 110) // connect ip address
      setAxi4LiteReg(dut, dut.io.control, 0x00, 1) // ap_start
      f_init = true
      println(s"[Init] finished.")
    }

    val open_port = fork {
      dut.clockDomain.waitSamplingWhere(f_init)
      val rd_port = recOnStream(dut, dut.io.net.listen_port).toBigInt
      println(s"[ListenPort]: open port @$rd_port")
      sendOnStream(dut, dut.io.net.port_status, true)
      println(s"[PortStatus]: port status sent.")
      f_open_port = true
    }

    val send_notif = fork {
      dut.clockDomain.waitSamplingWhere(f_open_port)
      var n_notif = 1024
      while (n_notif > 0){
        sendOnStream(dut, dut.io.net.notification, genNotif(0, 64, 0, 1, 1)) // 64
        println(s"[Notif] pkg: $n_notif")
        n_notif -= 1
      }
    }

    val rx_process = fork {
      dut.clockDomain.waitSamplingWhere(f_open_port)
      var n = 1
      while(true) {
        // sequential: read_pkg >> rx_meta >> rx_data
        val rx_len = recOnStream(dut, dut.io.net.read_pkg).toBigInt>>16 // high 16 bits
        val rx_len_pgk = rx_len / 64
        println(s"[RxPkg] rx_len: $rx_len_pgk")
        sendOnStream(dut, dut.io.net.rx_meta, rx_len) // send rx_meta
        println(s"[RxMeta] sent.")
        for (a <- 0 until rx_len_pgk.toInt) {
          val rx_data = genRxData(BigInt(a), a === rx_len_pgk.toInt - 1)
          sendOnStream(dut, dut.io.net.rx_data, rx_data)
          println(s"[RxData] pkg $a sent.")
        }
        println(s"[RxProcess] pkg idx: $n")
        n += 1
      }
    }

    val open_connection = fork {
      val ip_tuple = recOnStream(dut, dut.io.net.open_connection).toBigInt
      println(s"[OpenConnect]: open connection ip@${ip_tuple&0xFFFFFFFFL}\t port@${ip_tuple>>32}")
      val sid = 8
      sendOnStream(dut, dut.io.net.open_status, sid + (1<<16))
      println(s"[OpenStatus]: open_status sent.")

      f_open_connect = true

      // $finish
      dut.clockDomain.waitSampling(10000)
    }

    val tx_meta_process = fork {
      dut.clockDomain.waitSamplingWhere(f_open_connect)
      var n = 1
      while(true) {
        val tx_meta = recOnStream(dut, dut.io.net.tx_meta).toBigInt
        println(s"[TxMeta]: txLen: ${tx_meta >> 16}\t sid: ${tx_meta & 0xFFFF}")
        sendOnStream(dut, dut.io.net.tx_status, genTxStatus(8, tx_meta & 0xFFFF, 0xFF, 0))
        println(s"[TxStatus]: tx_status sent, pkg idx: $n.")
        n += 1
      }
    }

    val tx_data = fork {
      dut.clockDomain.waitSamplingWhere(f_open_connect)
      var n = 1
      while(true) {
        val tx_data = recOnStream(dut, dut.io.net.tx_data).toBigInt
        println(s"[TxData]: txData: ${tx_data & 0xFFFF}\t kep: ${(tx_data >> 512) & 0xFFFF}\t last: ${(tx_data >> (512+64)) & 0x01}\t pkg idx: $n")
        n += 1
      }
    }

    open_port.join()
    send_notif.join()
    open_connection.join()
  }


  test("round_trip") {
    SimConfig.withWave.compile {
      val dut = RoundTrip()
      dut.io.net.simPublic()
      dut.io.control.simPublic()
      dut
    }.doSim("round_trip", 99)(round_trip)
  }

}