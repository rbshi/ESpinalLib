// Generator : SpinalHDL v1.5.0    git head : 83a031922866b078c411ec5529e00f1b6e79f8e7
// Component : Axi4DMAWrite
// Git hash  : 3da20cadaf478eb55405854fa44c4567d552ba77


`define Axi4DMAWritePhase_binary_sequential_type [1:0]
`define Axi4DMAWritePhase_binary_sequential_IDLE 2'b00
`define Axi4DMAWritePhase_binary_sequential_SETUP 2'b01
`define Axi4DMAWritePhase_binary_sequential_WRITE 2'b10
`define Axi4DMAWritePhase_binary_sequential_RESPONSE 2'b11


module Axi4DMAWrite (
  output reg          io_axi_aw_valid,
  input               io_axi_aw_ready,
  output reg [31:0]   io_axi_aw_payload_addr,
  output     [0:0]    io_axi_aw_payload_id,
  output reg [7:0]    io_axi_aw_payload_len,
  output     [2:0]    io_axi_aw_payload_size,
  output     [1:0]    io_axi_aw_payload_burst,
  output reg          io_axi_w_valid,
  input               io_axi_w_ready,
  output reg [511:0]  io_axi_w_payload_data,
  output     [63:0]   io_axi_w_payload_strb,
  output reg          io_axi_w_payload_last,
  input               io_axi_b_valid,
  output reg          io_axi_b_ready,
  input      [0:0]    io_axi_b_payload_id,
  input      [1:0]    io_axi_b_payload_resp,
  input      [31:0]   io_start_addr,
  input      [7:0]    io_len_burst,
  input      [7:0]    io_num_burst,
  input      [7:0]    io_stride,
  input               io_ap_start,
  output reg          io_ap_ready,
  output reg          io_ap_done,
  output reg          io_ap_idle,
  input               clk,
  input               reset
);
  wire       [31:0]   _zz_io_axi_aw_payload_addr;
  wire       [22:0]   _zz_io_axi_aw_payload_addr_1;
  wire       [15:0]   _zz_io_axi_aw_payload_addr_2;
  reg        `Axi4DMAWritePhase_binary_sequential_type phase;
  reg        [7:0]    lenBurst;
  reg        [7:0]    numBurst;
  wire                when_Axi4DMAWrite_l118;
  wire                when_Axi4DMAWrite_l131;
  wire                when_Axi4DMAWrite_l135;
  `ifndef SYNTHESIS
  reg [63:0] phase_string;
  `endif


  assign _zz_io_axi_aw_payload_addr_1 = ({7'd0,_zz_io_axi_aw_payload_addr_2} <<< io_axi_aw_payload_size);
  assign _zz_io_axi_aw_payload_addr = {9'd0, _zz_io_axi_aw_payload_addr_1};
  assign _zz_io_axi_aw_payload_addr_2 = (io_stride * io_len_burst);
  `ifndef SYNTHESIS
  always @(*) begin
    case(phase)
      `Axi4DMAWritePhase_binary_sequential_IDLE : phase_string = "IDLE    ";
      `Axi4DMAWritePhase_binary_sequential_SETUP : phase_string = "SETUP   ";
      `Axi4DMAWritePhase_binary_sequential_WRITE : phase_string = "WRITE   ";
      `Axi4DMAWritePhase_binary_sequential_RESPONSE : phase_string = "RESPONSE";
      default : phase_string = "????????";
    endcase
  end
  `endif

  assign io_axi_aw_payload_burst = 2'b01;
  always @(*) begin
    io_axi_aw_payload_len = io_len_burst;
    case(phase)
      `Axi4DMAWritePhase_binary_sequential_IDLE : begin
      end
      `Axi4DMAWritePhase_binary_sequential_SETUP : begin
        io_axi_aw_payload_len = io_len_burst;
      end
      `Axi4DMAWritePhase_binary_sequential_WRITE : begin
      end
      default : begin
      end
    endcase
  end

  assign io_axi_aw_payload_size = 3'b110;
  assign io_axi_aw_payload_id = 1'b0;
  assign io_axi_w_payload_strb = (1'b1 ? 64'hffffffffffffffff : 64'h0);
  always @(*) begin
    io_axi_aw_payload_addr = 32'h0;
    case(phase)
      `Axi4DMAWritePhase_binary_sequential_IDLE : begin
      end
      `Axi4DMAWritePhase_binary_sequential_SETUP : begin
        io_axi_aw_payload_addr = io_start_addr;
      end
      `Axi4DMAWritePhase_binary_sequential_WRITE : begin
        if(io_axi_aw_ready) begin
          if(when_Axi4DMAWrite_l118) begin
            io_axi_aw_payload_addr = (io_start_addr + _zz_io_axi_aw_payload_addr);
          end
        end
      end
      default : begin
      end
    endcase
  end

  always @(*) begin
    io_axi_aw_valid = 1'b0;
    case(phase)
      `Axi4DMAWritePhase_binary_sequential_IDLE : begin
      end
      `Axi4DMAWritePhase_binary_sequential_SETUP : begin
      end
      `Axi4DMAWritePhase_binary_sequential_WRITE : begin
        io_axi_aw_valid = 1'b1;
      end
      default : begin
      end
    endcase
  end

  always @(*) begin
    io_axi_w_payload_data = 512'h0;
    case(phase)
      `Axi4DMAWritePhase_binary_sequential_IDLE : begin
      end
      `Axi4DMAWritePhase_binary_sequential_SETUP : begin
      end
      `Axi4DMAWritePhase_binary_sequential_WRITE : begin
        io_axi_w_payload_data = 512'hffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff;
      end
      default : begin
      end
    endcase
  end

  always @(*) begin
    io_axi_w_valid = 1'b0;
    case(phase)
      `Axi4DMAWritePhase_binary_sequential_IDLE : begin
      end
      `Axi4DMAWritePhase_binary_sequential_SETUP : begin
      end
      `Axi4DMAWritePhase_binary_sequential_WRITE : begin
        io_axi_w_valid = 1'b1;
      end
      default : begin
      end
    endcase
  end

  always @(*) begin
    io_axi_w_payload_last = 1'b0;
    case(phase)
      `Axi4DMAWritePhase_binary_sequential_IDLE : begin
      end
      `Axi4DMAWritePhase_binary_sequential_SETUP : begin
      end
      `Axi4DMAWritePhase_binary_sequential_WRITE : begin
        if(when_Axi4DMAWrite_l131) begin
          io_axi_w_payload_last = 1'b1;
        end
      end
      default : begin
      end
    endcase
  end

  always @(*) begin
    io_axi_b_ready = 1'b1;
    case(phase)
      `Axi4DMAWritePhase_binary_sequential_IDLE : begin
      end
      `Axi4DMAWritePhase_binary_sequential_SETUP : begin
      end
      `Axi4DMAWritePhase_binary_sequential_WRITE : begin
      end
      default : begin
        if(io_axi_b_valid) begin
          io_axi_b_ready = 1'b1;
        end
      end
    endcase
  end

  always @(*) begin
    io_ap_idle = 1'b1;
    case(phase)
      `Axi4DMAWritePhase_binary_sequential_IDLE : begin
        if(io_ap_start) begin
          io_ap_idle = 1'b0;
        end
      end
      `Axi4DMAWritePhase_binary_sequential_SETUP : begin
        io_ap_idle = 1'b0;
      end
      `Axi4DMAWritePhase_binary_sequential_WRITE : begin
        io_ap_idle = 1'b0;
      end
      default : begin
        io_ap_idle = 1'b0;
      end
    endcase
  end

  always @(*) begin
    io_ap_ready = 1'b0;
    case(phase)
      `Axi4DMAWritePhase_binary_sequential_IDLE : begin
      end
      `Axi4DMAWritePhase_binary_sequential_SETUP : begin
      end
      `Axi4DMAWritePhase_binary_sequential_WRITE : begin
      end
      default : begin
        if(io_axi_b_valid) begin
          io_ap_ready = 1'b1;
        end
      end
    endcase
  end

  always @(*) begin
    io_ap_done = 1'b0;
    case(phase)
      `Axi4DMAWritePhase_binary_sequential_IDLE : begin
      end
      `Axi4DMAWritePhase_binary_sequential_SETUP : begin
      end
      `Axi4DMAWritePhase_binary_sequential_WRITE : begin
      end
      default : begin
        if(io_axi_b_valid) begin
          io_ap_done = 1'b1;
        end
      end
    endcase
  end

  assign when_Axi4DMAWrite_l118 = (lenBurst == 8'h0);
  assign when_Axi4DMAWrite_l131 = (lenBurst == 8'h0);
  assign when_Axi4DMAWrite_l135 = (numBurst == 8'h0);
  always @(posedge clk) begin
    if(reset) begin
      phase <= `Axi4DMAWritePhase_binary_sequential_IDLE;
    end else begin
      case(phase)
        `Axi4DMAWritePhase_binary_sequential_IDLE : begin
          if(io_ap_start) begin
            phase <= `Axi4DMAWritePhase_binary_sequential_SETUP;
          end
        end
        `Axi4DMAWritePhase_binary_sequential_SETUP : begin
          phase <= `Axi4DMAWritePhase_binary_sequential_WRITE;
        end
        `Axi4DMAWritePhase_binary_sequential_WRITE : begin
          if(when_Axi4DMAWrite_l135) begin
            phase <= `Axi4DMAWritePhase_binary_sequential_RESPONSE;
          end
        end
        default : begin
          if(io_axi_b_valid) begin
            phase <= `Axi4DMAWritePhase_binary_sequential_IDLE;
          end
        end
      endcase
    end
  end

  always @(posedge clk) begin
    case(phase)
      `Axi4DMAWritePhase_binary_sequential_IDLE : begin
      end
      `Axi4DMAWritePhase_binary_sequential_SETUP : begin
        lenBurst <= io_len_burst;
      end
      `Axi4DMAWritePhase_binary_sequential_WRITE : begin
        if(io_axi_aw_ready) begin
          if(when_Axi4DMAWrite_l118) begin
            lenBurst <= io_len_burst;
            numBurst <= (numBurst - 8'h01);
          end else begin
            lenBurst <= (lenBurst - 8'h01);
          end
        end
      end
      default : begin
      end
    endcase
  end


endmodule
