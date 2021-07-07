// Generator : SpinalHDL v1.5.0    git head : 83a031922866b078c411ec5529e00f1b6e79f8e7
// Component : Axi4DMAWrite
// Git hash  : 666dcbba79181659d0c736eb931d19ec1dc17a25


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
  output     [7:0]    io_axi_aw_payload_len,
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
  input               io_ap_start,
  output reg          io_ap_ready,
  output reg          io_ap_done,
  output reg          io_ap_idle,
  input               clk,
  input               reset
);
  wire       [5:0]    _zz_Axi4Incr_alignMask;
  wire       [11:0]   _zz_Axi4Incr_base;
  wire       [11:0]   _zz_Axi4Incr_base_1;
  wire       [11:0]   _zz_Axi4Incr_baseIncr;
  wire       [3:0]    _zz_Axi4Incr_wrapCase_1;
  wire       [3:0]    _zz_Axi4Incr_wrapCase_2;
  reg        [11:0]   _zz_Axi4Incr_result;
  wire       [10:0]   _zz_Axi4Incr_result_1;
  wire       [0:0]    _zz_Axi4Incr_result_2;
  wire       [9:0]    _zz_Axi4Incr_result_3;
  wire       [1:0]    _zz_Axi4Incr_result_4;
  wire       [8:0]    _zz_Axi4Incr_result_5;
  wire       [2:0]    _zz_Axi4Incr_result_6;
  wire       [7:0]    _zz_Axi4Incr_result_7;
  wire       [3:0]    _zz_Axi4Incr_result_8;
  wire       [6:0]    _zz_Axi4Incr_result_9;
  wire       [4:0]    _zz_Axi4Incr_result_10;
  wire       [5:0]    _zz_Axi4Incr_result_11;
  wire       [5:0]    _zz_Axi4Incr_result_12;
  wire       [4:0]    _zz_Axi4Incr_result_13;
  wire       [6:0]    _zz_Axi4Incr_result_14;
  wire       [3:0]    _zz_Axi4Incr_result_15;
  wire       [7:0]    _zz_Axi4Incr_result_16;
  wire       [2:0]    _zz_Axi4Incr_result_17;
  wire       [8:0]    _zz_Axi4Incr_result_18;
  wire       [1:0]    _zz_Axi4Incr_result_19;
  wire       [9:0]    _zz_Axi4Incr_result_20;
  reg        `Axi4DMAWritePhase_binary_sequential_type phase;
  reg        [7:0]    lenBurst;
  wire       [2:0]    Axi4Incr_validSize;
  reg        [31:0]   Axi4Incr_result;
  wire       [19:0]   Axi4Incr_highCat;
  wire       [6:0]    Axi4Incr_sizeValue;
  wire       [11:0]   Axi4Incr_alignMask;
  wire       [11:0]   Axi4Incr_base;
  wire       [11:0]   Axi4Incr_baseIncr;
  reg        [1:0]    _zz_Axi4Incr_wrapCase;
  wire       [3:0]    Axi4Incr_wrapCase;
  `ifndef SYNTHESIS
  reg [63:0] phase_string;
  `endif


  assign _zz_Axi4Incr_alignMask = {(3'b101 < Axi4Incr_validSize),{(3'b100 < Axi4Incr_validSize),{(3'b011 < Axi4Incr_validSize),{(3'b010 < Axi4Incr_validSize),{(3'b001 < Axi4Incr_validSize),(3'b000 < Axi4Incr_validSize)}}}}};
  assign _zz_Axi4Incr_base_1 = io_axi_aw_payload_addr[11 : 0];
  assign _zz_Axi4Incr_base = _zz_Axi4Incr_base_1;
  assign _zz_Axi4Incr_baseIncr = {5'd0, Axi4Incr_sizeValue};
  assign _zz_Axi4Incr_wrapCase_1 = {1'd0, Axi4Incr_validSize};
  assign _zz_Axi4Incr_wrapCase_2 = {2'd0, _zz_Axi4Incr_wrapCase};
  assign _zz_Axi4Incr_result_1 = Axi4Incr_base[11 : 1];
  assign _zz_Axi4Incr_result_2 = Axi4Incr_baseIncr[0 : 0];
  assign _zz_Axi4Incr_result_3 = Axi4Incr_base[11 : 2];
  assign _zz_Axi4Incr_result_4 = Axi4Incr_baseIncr[1 : 0];
  assign _zz_Axi4Incr_result_5 = Axi4Incr_base[11 : 3];
  assign _zz_Axi4Incr_result_6 = Axi4Incr_baseIncr[2 : 0];
  assign _zz_Axi4Incr_result_7 = Axi4Incr_base[11 : 4];
  assign _zz_Axi4Incr_result_8 = Axi4Incr_baseIncr[3 : 0];
  assign _zz_Axi4Incr_result_9 = Axi4Incr_base[11 : 5];
  assign _zz_Axi4Incr_result_10 = Axi4Incr_baseIncr[4 : 0];
  assign _zz_Axi4Incr_result_11 = Axi4Incr_base[11 : 6];
  assign _zz_Axi4Incr_result_12 = Axi4Incr_baseIncr[5 : 0];
  assign _zz_Axi4Incr_result_13 = Axi4Incr_base[11 : 7];
  assign _zz_Axi4Incr_result_14 = Axi4Incr_baseIncr[6 : 0];
  assign _zz_Axi4Incr_result_15 = Axi4Incr_base[11 : 8];
  assign _zz_Axi4Incr_result_16 = Axi4Incr_baseIncr[7 : 0];
  assign _zz_Axi4Incr_result_17 = Axi4Incr_base[11 : 9];
  assign _zz_Axi4Incr_result_18 = Axi4Incr_baseIncr[8 : 0];
  assign _zz_Axi4Incr_result_19 = Axi4Incr_base[11 : 10];
  assign _zz_Axi4Incr_result_20 = Axi4Incr_baseIncr[9 : 0];
  always @(*) begin
    case(Axi4Incr_wrapCase)
      4'b0000 : begin
        _zz_Axi4Incr_result = {_zz_Axi4Incr_result_1,_zz_Axi4Incr_result_2};
      end
      4'b0001 : begin
        _zz_Axi4Incr_result = {_zz_Axi4Incr_result_3,_zz_Axi4Incr_result_4};
      end
      4'b0010 : begin
        _zz_Axi4Incr_result = {_zz_Axi4Incr_result_5,_zz_Axi4Incr_result_6};
      end
      4'b0011 : begin
        _zz_Axi4Incr_result = {_zz_Axi4Incr_result_7,_zz_Axi4Incr_result_8};
      end
      4'b0100 : begin
        _zz_Axi4Incr_result = {_zz_Axi4Incr_result_9,_zz_Axi4Incr_result_10};
      end
      4'b0101 : begin
        _zz_Axi4Incr_result = {_zz_Axi4Incr_result_11,_zz_Axi4Incr_result_12};
      end
      4'b0110 : begin
        _zz_Axi4Incr_result = {_zz_Axi4Incr_result_13,_zz_Axi4Incr_result_14};
      end
      4'b0111 : begin
        _zz_Axi4Incr_result = {_zz_Axi4Incr_result_15,_zz_Axi4Incr_result_16};
      end
      4'b1000 : begin
        _zz_Axi4Incr_result = {_zz_Axi4Incr_result_17,_zz_Axi4Incr_result_18};
      end
      default : begin
        _zz_Axi4Incr_result = {_zz_Axi4Incr_result_19,_zz_Axi4Incr_result_20};
      end
    endcase
  end

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
  assign io_axi_aw_payload_len = 8'h0;
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
        io_axi_w_payload_last = 1'b1;
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
        end else begin
          io_axi_b_ready = 1'b0;
        end
      end
    endcase
  end

  assign Axi4Incr_validSize = io_axi_aw_payload_size[2 : 0];
  assign Axi4Incr_highCat = io_axi_aw_payload_addr[31 : 12];
  assign Axi4Incr_sizeValue = {(3'b110 == Axi4Incr_validSize),{(3'b101 == Axi4Incr_validSize),{(3'b100 == Axi4Incr_validSize),{(3'b011 == Axi4Incr_validSize),{(3'b010 == Axi4Incr_validSize),{(3'b001 == Axi4Incr_validSize),(3'b000 == Axi4Incr_validSize)}}}}}};
  assign Axi4Incr_alignMask = {6'd0, _zz_Axi4Incr_alignMask};
  assign Axi4Incr_base = (_zz_Axi4Incr_base & (~ Axi4Incr_alignMask));
  assign Axi4Incr_baseIncr = (Axi4Incr_base + _zz_Axi4Incr_baseIncr);
  always @(*) begin
    casez(io_axi_aw_payload_len)
      8'b????1??? : begin
        _zz_Axi4Incr_wrapCase = 2'b11;
      end
      8'b?????1?? : begin
        _zz_Axi4Incr_wrapCase = 2'b10;
      end
      8'b??????1? : begin
        _zz_Axi4Incr_wrapCase = 2'b01;
      end
      default : begin
        _zz_Axi4Incr_wrapCase = 2'b00;
      end
    endcase
  end

  assign Axi4Incr_wrapCase = (_zz_Axi4Incr_wrapCase_1 + _zz_Axi4Incr_wrapCase_2);
  always @(*) begin
    case(io_axi_aw_payload_burst)
      2'b00 : begin
        Axi4Incr_result = io_axi_aw_payload_addr;
      end
      2'b10 : begin
        Axi4Incr_result = {Axi4Incr_highCat,_zz_Axi4Incr_result};
      end
      default : begin
        Axi4Incr_result = {Axi4Incr_highCat,Axi4Incr_baseIncr};
      end
    endcase
  end

  always @(posedge clk) begin
    if(reset) begin
      io_ap_ready <= 1'b0;
      io_ap_done <= 1'b0;
      io_ap_idle <= 1'b1;
      phase <= `Axi4DMAWritePhase_binary_sequential_IDLE;
    end else begin
      io_ap_idle <= 1'b1;
      io_ap_ready <= 1'b0;
      io_ap_done <= 1'b0;
      case(phase)
        `Axi4DMAWritePhase_binary_sequential_IDLE : begin
          io_ap_ready <= 1'b0;
          io_ap_done <= 1'b0;
          io_ap_idle <= 1'b1;
          if(io_ap_start) begin
            phase <= `Axi4DMAWritePhase_binary_sequential_SETUP;
            io_ap_idle <= 1'b0;
          end
        end
        `Axi4DMAWritePhase_binary_sequential_SETUP : begin
          phase <= `Axi4DMAWritePhase_binary_sequential_WRITE;
        end
        `Axi4DMAWritePhase_binary_sequential_WRITE : begin
          phase <= `Axi4DMAWritePhase_binary_sequential_RESPONSE;
        end
        default : begin
          if(io_axi_b_valid) begin
            io_ap_idle <= 1'b1;
            io_ap_ready <= 1'b1;
            io_ap_done <= 1'b1;
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
      end
      `Axi4DMAWritePhase_binary_sequential_WRITE : begin
        if(io_axi_aw_ready) begin
          lenBurst <= (lenBurst - 8'h01);
        end
      end
      default : begin
      end
    endcase
  end


endmodule
