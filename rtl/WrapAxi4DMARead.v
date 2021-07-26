
module WrapAxi4DMARead(
  output              io_axi_arvalid,
  input               io_axi_arready,
  output     [63:0]   io_axi_araddr,
  output     [0:0]    io_axi_arid,
  output     [7:0]    io_axi_arlen,
  output     [2:0]    io_axi_arsize,
  output     [1:0]    io_axi_arburst,
  input               io_axi_rvalid,
  output              io_axi_rready,
  input      [511:0]  io_axi_rdata,
  input      [0:0]    io_axi_rid,
  input      [1:0]    io_axi_rresp,
  input               io_axi_rlast,
  input      [63:0]   io_start_addr,
  input      [7:0]    io_len_burst,
  input      [31:0]   io_num_burst,
  input      [7:0]    io_stride,
  output     [31:0]   io_cnt_clk,
  input               io_ap_start,
  output reg          io_ap_ready,
  output reg          io_ap_done,
  output reg          io_ap_idle,
  input               clk,
  input               reset
);

    Axi4DMARead u_Axi4DMARead(
        io_axi_arvalid,
        io_axi_arready,
        io_axi_araddr,
        io_axi_arid,
        io_axi_arlen,
        io_axi_arsize,
        io_axi_arburst,
        io_axi_rvalid,
        io_axi_rready,
        io_axi_rdata,
        io_axi_rid,
        io_axi_rresp,
        io_axi_rlast,
        io_start_addr,
        io_len_burst,
        io_num_burst,
        io_stride,
        io_cnt_clk,
        io_ap_start,
        io_ap_ready,
        io_ap_done,
        io_ap_idle,
        clk,
        reset
    );



    // the "macro" to dump signals
`ifdef COCOTB_SIM
    initial begin
    $dumpfile ("wave.vcd");
    $dumpvars ();
    end
`endif

endmodule : WrapAxi4DMARead
