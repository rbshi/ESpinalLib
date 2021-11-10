/*
 * Copyright (c) 2020 Yizhou Shan, UC San Diego. All rights reserved.
 */


package util

import spinal.core._

/*
 * If you want to rename the IO signals your module,
 * you should extends your class with this trait.
 * At the end of your class, call `addPrePopTask(renameIO)`.
 */
trait RenameIO {

  /*
   * Only subclasses of Component can extend this RenameIO trait
   * @this is the calling class, therefore able to use SpinalHDL methods
   * i.e., getAllIo, getName.
   */
  this : Component =>

  def renameIO(): Unit = {

    this.noIoPrefix()

    for (port <- this.getAllIo) {
      val newName = port.getName()
        .replaceAll("(a?[wrb])_(payload_)?", "$1")

        // For Axi Stream, raw interface
        .replaceAll("_payload$", "_tdata")
        .replaceAll("_payload_data$", "_tdata")
        .replaceAll("_payload_kep$", "_tkeep")
        .replaceAll("_payload_last$", "_tlast")
        .replaceAll("_ready$", "_tready")
        .replaceAll("_valid$", "_tvalid")

        //
        .replaceAll("control_", "s_axi_control_")
        .replaceAll("net_", "")

        .replaceAll("(listen_port)", "m_axis_tcp_$1")
        .replaceAll("(port_status)", "s_axis_tcp_$1")
        .replaceAll("(notification)", "s_axis_tcp_$1")
        .replaceAll("(read_pkg)", "m_axis_tcp_$1")
        .replaceAll("(rx_meta)", "s_axis_tcp_$1")
        .replaceAll("(rx_data)", "s_axis_tcp_$1")
        .replaceAll("(open_connection)", "m_axis_tcp_$1")
        .replaceAll("(open_status)", "s_axis_tcp_$1")
        .replaceAll("(close_connection)", "m_axis_tcp_$1")
        .replaceAll("(tx_meta)", "m_axis_tcp_$1")
        .replaceAll("(tx_data)", "m_axis_tcp_$1")
        .replaceAll("(tx_status)", "s_axis_tcp_$1")

        // For Axi Stream, Fragment Interface
//        .replaceAll("_last$", "_tlast")
//        .replaceAll("_payload_t", "_t")
//        .replaceAll("_payload_fragment_t", "_t")
//        .replaceAll("_payload_", "_")

      port.setName(newName)
    }
    //println(f"Renamed the IO signal of ${this.getClass().getSimpleName}")
  }
}
