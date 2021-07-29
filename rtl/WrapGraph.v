
module WrapGraph(

    output wire          io_axi_awvalid,
    input  wire          io_axi_awready,
    output wire [63:0]   io_axi_awaddr,
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

    output wire          io_axi_arvalid,
    input  wire          io_axi_arready,
    output wire [63:0]   io_axi_araddr,
    output wire [0:0]    io_axi_arid,
    output wire [7:0]    io_axi_arlen,
    output wire [2:0]    io_axi_arsize,
    output wire [1:0]    io_axi_arburst,
    input  wire          io_axi_rvalid,
    output wire          io_axi_rready,
    input  wire [511:0]  io_axi_rdata,
    input  wire [0:0]    io_axi_rid,
    input  wire [1:0]    io_axi_rresp,
    input  wire          io_axi_rlast,

    input  wire [63:0]   io_addr_ptr,
    input  wire [63:0]   io_addr_data,
    input  wire [63:0]   io_addr_queue,
    input  wire [31:0]   io_start_node,
    output wire [31:0]   io_cnt_clk,
    input  wire          io_ap_start,
    output wire          io_ap_ready,
    output wire          io_ap_done,
    output wire          io_ap_idle,
    input  wire          clk,
    input  wire          reset

);

    GraphSSSP u_GraphSSSP(
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
        io_addr_ptr,
        io_addr_data,
        io_addr_queue,
        io_start_node,
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

endmodule : WrapGraph
