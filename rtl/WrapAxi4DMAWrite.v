
module WrapAxi4DMAWrite(
    output wire          io_axi_awvalid,
    input  wire          io_axi_awready,
    output wire [31:0]   io_axi_awaddr,
    output wire [0:0]    io_axi_awid,
    output wire [7:0]    io_axi_awlen,
    output wire [2:0]    io_axi_awsize,
    output wire [1:0]    io_axi_awburst,
    output wire          io_axi_wvalid,
    input  wire          io_axi_wready,
    output wire [511:0]  io_axi_wdata,
    output wire [63:0]   io_axi_wstrb,
    output wire          io_axi_wlast,
    input  wire          io_axi_bvalid,
    output wire          io_axi_bready,
    input  wire [0:0]    io_axi_bid,
    input  wire [1:0]    io_axi_bresp,
    input  wire [31:0]   io_start_addr,
    input  wire          io_ap_start,
    output wire          io_ap_ready,
    output wire          io_ap_done,
    output wire          io_ap_idle,
    input  wire          clk,
    input  wire          reset
);

    Axi4DMAWrite u_Axi4DMAWrite(
        io_axi_awvalid,
        io_axi_awready,
        io_axi_awaddr,
        io_axi_awid,
        io_axi_awlen,
        io_axi_awsize,
        io_axi_awburst,
        io_axi_wvalid,
        io_axi_wready,
        io_axi_wdata,
        io_axi_wstrb,
        io_axi_wlast,
        io_axi_bvalid,
        io_axi_bready,
        io_axi_bid,
        io_axi_bresp,
        io_start_addr,
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

endmodule : WrapAxi4DMAWrite