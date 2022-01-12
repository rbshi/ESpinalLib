package coyote

object HbmGen {
  def main(args: Array[String]): Unit = {

    for (ii <- 0 until 33){
      val portName = f"${ii}%02d"
      val txt1 =
        s"""    .S${portName}_AXI_araddr(s_axi_hbm[$ii].araddr),
          |    .S${portName}_AXI_arburst(s_axi_hbm[$ii].arburst),
          |    .S${portName}_AXI_arid(s_axi_hbm[$ii].arid),
          |    .S${portName}_AXI_arlen(s_axi_hbm[$ii].arlen),
          |    .S${portName}_AXI_arsize(s_axi_hbm[$ii].arsize),
          |    .S${portName}_AXI_arvalid(s_axi_hbm[$ii].arvalid),
          |    .S${portName}_AXI_awaddr(s_axi_hbm[$ii].awaddr),
          |    .S${portName}_AXI_awburst(s_axi_hbm[$ii].awburst),
          |    .S${portName}_AXI_awid(s_axi_hbm[$ii].awid),
          |    .S${portName}_AXI_awlen(s_axi_hbm[$ii].awlen),
          |    .S${portName}_AXI_awsize(s_axi_hbm[$ii].awsize),
          |    .S${portName}_AXI_awvalid(s_axi_hbm[$ii].awvalid),
          |    .S${portName}_AXI_rready(s_axi_hbm[$ii].rready),
          |    .S${portName}_AXI_bready(s_axi_hbm[$ii].bready),
          |    .S${portName}_AXI_wdata(s_axi_hbm[$ii].wdata),
          |    .S${portName}_AXI_wlast(s_axi_hbm[$ii].wlast),
          |    .S${portName}_AXI_wstrb(s_axi_hbm[$ii].wstrb),
          |    .S${portName}_AXI_wvalid(s_axi_hbm[$ii].wvalid),
          |    .S${portName}_AXI_arready(s_axi_hbm[$ii].arready),
          |    .S${portName}_AXI_awready(s_axi_hbm[$ii].awready),
          |    .S${portName}_AXI_rdata(s_axi_hbm[$ii].rdata),
          |    .S${portName}_AXI_rid(s_axi_hbm[$ii].rid),
          |    .S${portName}_AXI_rlast(s_axi_hbm[$ii].rlast),
          |    .S${portName}_AXI_rresp(s_axi_hbm[$ii].rresp),
          |    .S${portName}_AXI_rvalid(s_axi_hbm[$ii].rvalid),
          |    .S${portName}_AXI_wready(s_axi_hbm[$ii].wready),
          |    .S${portName}_AXI_bid(s_axi_hbm[$ii].bid),
          |    .S${portName}_AXI_bresp(s_axi_hbm[$ii].bresp),
          |    .S${portName}_AXI_bvalid(s_axi_hbm[$ii].bvalid),""".stripMargin
      println(txt1)
      println()
    }
  }
}
