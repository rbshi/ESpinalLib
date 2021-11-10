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

case class RoundTrip() extends Component with SetDefaultIO with RenameIO {

  val io = new Bundle{
    val net = NetIO()
    val control = slave(AxiLite4(AxiLite4Config(12, 32)))
  }

  io.net.setDefault()



  val notifCnt = Counter(32 bits, io.net.notification.fire)
  val rxpkgCnt = Counter(32 bits, io.net.read_pkg.fire)
  val rxmetaCnt = Counter(32 bits, io.net.rx_meta.fire)
  val rxdataCnt = Counter(32 bits, io.net.rx_data.fire)
  val txmetaCnt = Counter(32 bits, io.net.tx_meta.fire)
  val txstatusCnt = Counter(32 bits, io.net.tx_status.fire)
  val txdataCnt = Counter(32 bits, io.net.tx_data.fire)

  // axilite control registers
  val ctlreg = new AxiLite4SlaveFactory(io.control)

  val ap_start = ctlreg.createReadAndWrite(Bool(), 0, 0).init(False)
  val ap_done = ctlreg.createReadOnly(Bool(), 0, 1).init(False)
  val ap_idle = ctlreg.createReadOnly(Bool(), 0, 2).init(True)
  val ap_ready = ctlreg.createReadOnly(Bool(), 0, 3).init(False)

  // ap_done, clear on read!
  ctlreg.onRead(0){ap_done.clear()}

  // net config reg
  val useIpAddr = ctlreg.createReadAndWrite(UInt(32 bits), 0x18, 0)
  val pkgWordCount = ctlreg.createReadAndWrite(UInt(32 bits), 0x20, 0)

  // status Cnt
  ctlreg.readAndWrite(notifCnt, 0x24, 0)
  ctlreg.readAndWrite(rxpkgCnt, 0x28, 0)
  ctlreg.readAndWrite(rxmetaCnt, 0x2c, 0)
  ctlreg.readAndWrite(rxdataCnt, 0x30, 0)
  ctlreg.readAndWrite(txmetaCnt, 0x34, 0)
  ctlreg.readAndWrite(txstatusCnt, 0x38, 0)
  ctlreg.readAndWrite(txdataCnt, 0x3c, 0)

  // soft reset
  val swRst = ctlreg.createReadAndWrite(Bool(), 0x40, 0).init(False)


  // rxData >> this >> txData
  val rxDataFifo = StreamFifo(netData(512), 512) // several brams
  rxDataFifo.io.flush := False
  rxDataFifo.io.pop.ready := False

  val server = new Area{

    // type cast
    val notification = io.net.notification.payload.toDataType(notif())

    // notification fifo >> read_pkg
    val notifFifo = StreamFifo(readPkg().asBits, 512) // a sdp bram
    notifFifo.io.flush := False

//    io.net.notification.ready <> (notifFifo.io.push.ready && rxDataFifo.io.push.ready) // ready to get both notif and rxData
    notifFifo.io.push.payload <>  notification.length ## notification.sid
    io.net.notification.ready <> (notifFifo.io.push.ready && rxDataFifo.io.availability>63) // ready to get both notif and rxDataFifo has enough space
    notifFifo.io.push.valid <> (io.net.notification.valid && rxDataFifo.io.availability>63)
    notifFifo.io.pop >> io.net.read_pkg

    // directly push rx_data to fifo for tx
    io.net.rx_data >> rxDataFifo.io.push

    val fsm = new StateMachine{

      val INIT = new State with EntryPoint
      val OPEN_PORT, WAIT_PORT_STATUS, READ_PKG = new State

      INIT
        .whenIsActive{
          notifFifo.io.flush := True
          rxDataFifo.io.flush := True
          notifCnt.clear()
          rxpkgCnt.clear()
          rxmetaCnt.clear()
          rxdataCnt.clear()

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
        }
    }
  }




  val client = new Area{

    val ip_tuple = new ipTuple
    ip_tuple.port := 5002
    ip_tuple.ipaddr := useIpAddr
    val rSid = Reg(UInt(16 bits)).init(0)
    val pkgCnt = Reg(UInt(16 bits)).init(0)

    val r_tx_status = Reg(Bool()).init(False)
    val r_tx_status_err = Reg(UInt(2 bits)).init(0)

    // cast
    val open_status = io.net.open_status.payload.toDataType(openStatus())
    val tx_status = io.net.tx_status.payload.toDataType(txStatus())

    // txMetaFifo
    val txMetaFifo = StreamFifo(txMeta().asBits, 512)
    txMetaFifo.io.flush := False
    txMetaFifo.io.pop >> io.net.tx_meta

    txMetaFifo.io.push.valid := False
    txMetaFifo.io.push.payload := 0

    val txLen = pkgWordCount * 64

    val fsm = new StateMachine {

      val INIT = new State with EntryPoint
      val INIT_CON, WAIT_CON, WAIT_PKG, SEND_PKG = new State

      INIT
        .whenIsActive{
          txMetaFifo.io.flush := True
          txmetaCnt.clear()
          txstatusCnt.clear()
          txdataCnt.clear()

          pkgCnt := 0
          when(ap_start){goto{INIT_CON}}
        }

      // init connection: open_connection,
      INIT_CON
        .whenIsActive{
          io.net.open_connection.payload := ip_tuple.asBits
          io.net.open_connection.valid := True
          when(io.net.open_connection.fire){goto(WAIT_CON)}
        }

      // wait connection: check open_status, if success, tx_metadata
      WAIT_CON
        .whenIsActive{
          io.net.open_status.ready := True

          txMetaFifo.io.push.payload := (txLen(15 downto 0) ## open_status.sid)

          when(io.net.open_status.fire){
            when(open_status.success){
              // get sid
              rSid := open_status.sid

              goto(WAIT_PKG)
            } otherwise{goto(INIT_CON)}
          }
        }

      // wait the first package
      WAIT_PKG
        .whenIsActive{

          // BUG!
          txMetaFifo.io.push.payload := (txLen(15 downto 0) ## rSid)

          when(rxDataFifo.io.occupancy>0 || rxDataFifo.io.push.fire){
            // push the first txMeta, fifo is always available
            txMetaFifo.io.push.valid := True
            pkgCnt := 1
            goto(SEND_PKG)
          }
        }

      // start_pkg: tx_metadata (send sid+pkg_length); tx_data (current word)
      SEND_PKG
        .whenIsActive{
          txMetaFifo.io.push.payload := (txLen(15 downto 0) ## rSid)
          io.net.tx_status.ready := True

          when(io.net.tx_status.fire){
//            r_tx_status := True
//            r_tx_status_err := tx_status.error

            // lose connection
            when(tx_status.error===1){goto(INIT_CON)} // connection lost
            when(tx_status.error===2){txMetaFifo.io.push.valid := True} // send tx_meta again
          }

//          // pkgCnt === 1 must wait for tx_stats back
//          rxDataFifo.io.pop.continueWhen(r_tx_status && r_tx_status_err===0) >> io.net.tx_data
          when(pkgCnt===1){
            rxDataFifo.io.pop.continueWhen(io.net.tx_status.fire && tx_status.error===0) >> io.net.tx_data
          } otherwise{
            rxDataFifo.io.pop >> io.net.tx_data
          }

          when(rxDataFifo.io.pop.fire){
            // the last pkg
            when(pkgCnt === pkgWordCount){
              pkgCnt := 1
              txMetaFifo.io.push.valid := True // send tx_meta for the next pkg
//              r_tx_status.clear()
//              r_tx_status_err.clearAll()
            } otherwise{pkgCnt := pkgCnt + 1}
          }

          when(swRst){
            io.net.close_connection.valid := True
            io.net.close_connection.payload := rSid
          }
        }
    }
  }

  val ap_ctrl = new Area {
    val fsm = new StateMachine {

      val IDLE = new State with EntryPoint
      val RUN = new State

      IDLE
        .whenIsActive{
          ap_idle := True
          ap_ready := False
          when(ap_start){
            ap_idle := False
            goto(RUN)
          }
        }

      RUN
        .whenIsActive{
          // sw_rst indicates back to IDLE

          when(swRst){
            swRst := False
            ap_done := True // when swRst is H, set ap_done to H; ap_done is cleanOnRead
            ap_ready := True
            ap_start := False //

            // reset other fsm
            server.fsm.goto(server.fsm.INIT)
            client.fsm.goto(client.fsm.INIT)

            goto(IDLE)
          }
        }
    }
  }

}


object RoundTripMain {
  def main(args: Array[String]): Unit = {
    SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = LOW), targetDirectory = "rtl").generateVerilog {
      val top = RoundTrip()
      top.renameIO()
      top
    }
  }
}



























