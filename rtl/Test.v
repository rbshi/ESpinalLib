// Generator : SpinalHDL v1.5.0    git head : 83a031922866b078c411ec5529e00f1b6e79f8e7
// Component : Test
// Git hash  : a46e74b6e1a8555b691b5d7b82033e937e9692fc



module Test (
  output     [31:0]   io_output,
  input               clk,
  input               reset
);
  wire       [31:0]   _zz_cnt;
  wire       [2:0]    _zz_cnt_1;
  reg        [31:0]   cnt;

  assign _zz_cnt_1 = (3'b101 + 3'b001);
  assign _zz_cnt = {29'd0, _zz_cnt_1};
  assign io_output = cnt;
  always @(posedge clk) begin
    if(reset) begin
      cnt <= 32'h0;
    end else begin
      cnt <= (cnt + _zz_cnt);
    end
  end


endmodule
