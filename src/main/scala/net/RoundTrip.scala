package net

import spinal.core.{UInt, _}
import spinal.core.Mem
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4Config, AxiLite4SlaveFactory}
import spinal.lib.fsm.StateMachine
import util._

import scala.language.postfixOps

case class RoundTrip() extends Component with SetDefaultIO {

  val io = new Bundle{
    val net = NetIO()
    val control = slave(AxiLite4(AxiLite4Config(12, 32)))
  }

  io.net.setDefault()

//  io.control.ar.isMasterInterface = false
//  io.control.r.isMasterInterface = true
//  io.control.aw.isMasterInterface = false
//  io.control.w.isMasterInterface = false
//  io.control.b.isMasterInterface = true
//  setDefStream(io.control.ar)
//  setDefStream(io.control.r)
//  setDefStream(io.control.aw)
//  setDefStream(io.control.w)
//  setDefStream(io.control.b)

  // axilite control registers
  val ctlreg = new AxiLite4SlaveFactory(io.control)
  val ap_start = ctlreg.createReadAndWrite(Bool(), 0, 0)
//  val ap_done = ctlreg.createReadOnly(Bool(), 0, 1)
//  val ap_idle = ctlreg.createReadOnly(Bool(), 0, 2)
//  val ap_ready = ctlreg.createReadOnly(Bool(), 0, 3)

  // net config reg
  val useConn = ctlreg.createReadAndWrite(UInt(32 bits), 0x10, 0)
  val useIpAddr = ctlreg.createReadAndWrite(UInt(32 bits), 0x18, 0)
  val pkgWordCount = ctlreg.createReadAndWrite(UInt(16 bits), 0x20, 0)
  val baseIpAddr = ctlreg.createReadAndWrite(UInt(32 bits), 0x30, 0)
  val dualModeEn = ctlreg.createReadAndWrite(UInt(32 bits), 0x38, 0)
  val packetGap = ctlreg.createReadAndWrite(UInt(32 bits), 0x40, 0)
  val swRst = ctlreg.createReadAndWrite(Bool(), 0x48, 0) // soft reset


  // rxData >> this >> txData
  val rxDataFifo = StreamFifo(netData(512).asBits, 512) // several brams

  val server = new Area{

    // type case
    val notification = io.net.notification.payload.toDataType(notif())

    // notification fifo >> read_pkg
    val notifFifo = StreamFifo(readPkg().asBits, 512) // a sdp bram
    notifFifo.io.push.valid <> io.net.notification.valid
    io.net.notification.ready <> (notifFifo.io.push.ready && rxDataFifo.io.push.ready) // ready to get both notif and rxData
    notifFifo.io.push.payload <>  notification.length ## notification.sid
    notifFifo.io.pop >> io.net.read_pkg

    // directly push rx_data to fifo for tx
    io.net.rx_data >> rxDataFifo.io.push

    val rRdCnt, rNotifCnt, rNotif = Reg(UInt(32 bits)).init(0)

    val fsm = new StateMachine{

      val INIT = new State with EntryPoint
      val OPEN_PORT, WAIT_PORT_STATUS, READ_PKG = new State

      INIT
        .whenIsActive{
          when(ap_start){goto{OPEN_PORT}}
        }

      OPEN_PORT
        .whenIsActive{
          io.net.listen_port.valid := True
          io.net.listen_port.payload := 5001
          when(io.net.listen_port.fire){goto(WAIT_PORT_STATUS)}
        }

      WAIT_PORT_STATUS
        .whenIsActive{
          io.net.port_status.ready := True
          when(io.net.port_status.fire){
            when(io.net.port_status.payload){goto(READ_PKG)} otherwise{goto(OPEN_PORT)}
          }
        }

      READ_PKG
        .whenIsActive{
          io.net.rx_meta.ready := True
          io.net.rx_data.ready := True

          when(io.net.notification.fire){rNotif := rNotif + 1}
          when(notifFifo.io.push.fire){rNotifCnt := rNotifCnt + 1}

          when(io.net.rx_data.fire){
            // rd logic here, store in a fifo to send back, drive outside. do nothing here...
            rRdCnt := rRdCnt + 1
          }
        }
    }
  }


  val client = new Area{

    val ip_tuple = new ipTuple
    ip_tuple.port := 5002
    ip_tuple.ipaddr := useIpAddr
    val rSid = Reg(UInt(16 bits)).init(0)
    val pkgCnt = Reg(UInt(16 bits)).init(0)

    // cast
    val open_status = io.net.open_status.payload.toDataType(openStatus())
    val tx_status = io.net.tx_status.payload.toDataType(txStatus())

    // txMetaFifo
    val txMetaFifo = StreamFifo(txMeta().asBits, 512)
    txMetaFifo.io.pop >> io.net.tx_meta

    txMetaFifo.io.push.valid := False
    txMetaFifo.io.push.payload := 0

    val txLen = pkgWordCount * 64
    //
    rxDataFifo.io.pop.continueWhen(io.net.tx_status.fire && tx_status.error===0) >> io.net.tx_data

    val fsm = new StateMachine {

      val INIT = new State with EntryPoint
      val INIT_CON, WAIT_CON, SEND_PKG = new State

      INIT
        .whenIsActive{
          when(ap_start){goto{INIT_CON}}
        }

      // init connection: open_connection,
      INIT_CON
        .whenIsActive{
          // clear the status

          io.net.open_connection.payload := ip_tuple.asBits
          io.net.open_connection.valid := True
          when(io.net.open_connection.fire){goto(WAIT_CON)}
        }

      // wait connection: check open_status, if success, tx_metadata
      WAIT_CON
        .whenIsActive{
          io.net.open_status.ready := True
          when(io.net.open_status.fire){
            when(open_status.success){
              // get sid
              rSid := open_status.sid
              // push the first tx req
//              txMetaFifo.io.push.valid := True
//              txMetaFifo.io.push.payload := (txLen(15 downto 0) ## open_status.sid ) // rSid will be available in the next cycle

              // set pkgCnt
              pkgCnt := 1

              goto(SEND_PKG)
            } otherwise{goto(INIT_CON)}
          }
        }

      // start_pkg: tx_metadata (send sid+pkg_length); tx_data (current word)
      SEND_PKG
        .whenIsActive{
          txMetaFifo.io.push.payload := (txLen(15 downto 0) ## rSid)
          io.net.tx_status.ready := True

          when(io.net.tx_status.fire){
            // lose connection
            when(tx_status.error===1){goto(INIT_CON)} // connection lost
            when(tx_status.error===2){txMetaFifo.io.push.valid := True} // send tx_meta again
          }

          // fixme: now it's the logic ONLY support pkgWordCount=1 (tx_status.fire)

          when(rxDataFifo.io.push.fire){
            // the last pkg
            when(pkgCnt === pkgWordCount){
              pkgCnt := 1
              txMetaFifo.io.push.valid := True // send tx_meta for the next pkg
            } otherwise{pkgCnt := pkgCnt + 1}
          }
        }

    }
  }
}



























