// Generator : SpinalHDL v1.5.0    git head : 83a031922866b078c411ec5529e00f1b6e79f8e7
// Component : AxisFifo



module AxisFifo512 (
  input               io_s_axis_valid,
  output              io_s_axis_ready,
  input      [511:0]  io_s_axis_payload,
  output              io_m_axis_valid,
  input               io_m_axis_ready,
  output     [511:0]  io_m_axis_payload,
  output              io_s_axis_almostfull,
  input               clk,
  input               reset
);
  wire                fifo_io_push_ready;
  wire                fifo_io_pop_valid;
  wire       [511:0]  fifo_io_pop_payload;
  wire       [9:0]    fifo_io_occupancy;
  wire       [9:0]    fifo_io_availability;

  StreamFifo512 fifo (
    .io_push_valid      (io_s_axis_valid       ), //i
    .io_push_ready      (fifo_io_push_ready    ), //o
    .io_push_payload    (io_s_axis_payload     ), //i
    .io_pop_valid       (fifo_io_pop_valid     ), //o
    .io_pop_ready       (io_m_axis_ready       ), //i
    .io_pop_payload     (fifo_io_pop_payload   ), //o
    .io_flush           (1'b0                  ), //i
    .io_occupancy       (fifo_io_occupancy     ), //o
    .io_availability    (fifo_io_availability  ), //o
    .clk                (clk                   ), //i
    .reset              (reset                 )  //i
  );
  assign io_s_axis_ready = fifo_io_push_ready;
  assign io_m_axis_valid = fifo_io_pop_valid;
  assign io_m_axis_payload = fifo_io_pop_payload;
  assign io_s_axis_almostfull = (fifo_io_occupancy == 10'h1ff);

endmodule

module StreamFifo512 (
  input               io_push_valid,
  output              io_push_ready,
  input      [511:0]  io_push_payload,
  output              io_pop_valid,
  input               io_pop_ready,
  output     [511:0]  io_pop_payload,
  input               io_flush,
  output     [9:0]    io_occupancy,
  output     [9:0]    io_availability,
  input               clk,
  input               reset
);
  reg        [511:0]  _zz_logic_ram_port0;
  wire       [8:0]    _zz_logic_pushPtr_valueNext;
  wire       [0:0]    _zz_logic_pushPtr_valueNext_1;
  wire       [8:0]    _zz_logic_popPtr_valueNext;
  wire       [0:0]    _zz_logic_popPtr_valueNext_1;
  wire                _zz_logic_ram_port;
  wire                _zz_io_pop_payload;
  wire       [8:0]    _zz_io_availability;
  reg                 _zz_1;
  reg                 logic_pushPtr_willIncrement;
  reg                 logic_pushPtr_willClear;
  reg        [8:0]    logic_pushPtr_valueNext;
  reg        [8:0]    logic_pushPtr_value;
  wire                logic_pushPtr_willOverflowIfInc;
  wire                logic_pushPtr_willOverflow;
  reg                 logic_popPtr_willIncrement;
  reg                 logic_popPtr_willClear;
  reg        [8:0]    logic_popPtr_valueNext;
  reg        [8:0]    logic_popPtr_value;
  wire                logic_popPtr_willOverflowIfInc;
  wire                logic_popPtr_willOverflow;
  wire                logic_ptrMatch;
  reg                 logic_risingOccupancy;
  wire                logic_pushing;
  wire                logic_popping;
  wire                logic_empty;
  wire                logic_full;
  reg                 _zz_io_pop_valid;
  wire                when_Stream_l955;
  wire       [8:0]    logic_ptrDif;
  reg [511:0] logic_ram [0:511];

  assign _zz_logic_pushPtr_valueNext_1 = logic_pushPtr_willIncrement;
  assign _zz_logic_pushPtr_valueNext = {8'd0, _zz_logic_pushPtr_valueNext_1};
  assign _zz_logic_popPtr_valueNext_1 = logic_popPtr_willIncrement;
  assign _zz_logic_popPtr_valueNext = {8'd0, _zz_logic_popPtr_valueNext_1};
  assign _zz_io_availability = (logic_popPtr_value - logic_pushPtr_value);
  assign _zz_io_pop_payload = 1'b1;
  always @(posedge clk) begin
    if(_zz_io_pop_payload) begin
      _zz_logic_ram_port0 <= logic_ram[logic_popPtr_valueNext];
    end
  end

  always @(posedge clk) begin
    if(_zz_1) begin
      logic_ram[logic_pushPtr_value] <= io_push_payload;
    end
  end

  always @(*) begin
    _zz_1 = 1'b0;
    if(logic_pushing) begin
      _zz_1 = 1'b1;
    end
  end

  always @(*) begin
    logic_pushPtr_willIncrement = 1'b0;
    if(logic_pushing) begin
      logic_pushPtr_willIncrement = 1'b1;
    end
  end

  always @(*) begin
    logic_pushPtr_willClear = 1'b0;
    if(io_flush) begin
      logic_pushPtr_willClear = 1'b1;
    end
  end

  assign logic_pushPtr_willOverflowIfInc = (logic_pushPtr_value == 9'h1ff);
  assign logic_pushPtr_willOverflow = (logic_pushPtr_willOverflowIfInc && logic_pushPtr_willIncrement);
  always @(*) begin
    logic_pushPtr_valueNext = (logic_pushPtr_value + _zz_logic_pushPtr_valueNext);
    if(logic_pushPtr_willClear) begin
      logic_pushPtr_valueNext = 9'h0;
    end
  end

  always @(*) begin
    logic_popPtr_willIncrement = 1'b0;
    if(logic_popping) begin
      logic_popPtr_willIncrement = 1'b1;
    end
  end

  always @(*) begin
    logic_popPtr_willClear = 1'b0;
    if(io_flush) begin
      logic_popPtr_willClear = 1'b1;
    end
  end

  assign logic_popPtr_willOverflowIfInc = (logic_popPtr_value == 9'h1ff);
  assign logic_popPtr_willOverflow = (logic_popPtr_willOverflowIfInc && logic_popPtr_willIncrement);
  always @(*) begin
    logic_popPtr_valueNext = (logic_popPtr_value + _zz_logic_popPtr_valueNext);
    if(logic_popPtr_willClear) begin
      logic_popPtr_valueNext = 9'h0;
    end
  end

  assign logic_ptrMatch = (logic_pushPtr_value == logic_popPtr_value);
  assign logic_pushing = (io_push_valid && io_push_ready);
  assign logic_popping = (io_pop_valid && io_pop_ready);
  assign logic_empty = (logic_ptrMatch && (! logic_risingOccupancy));
  assign logic_full = (logic_ptrMatch && logic_risingOccupancy);
  assign io_push_ready = (! logic_full);
  assign io_pop_valid = ((! logic_empty) && (! (_zz_io_pop_valid && (! logic_full))));
  assign io_pop_payload = _zz_logic_ram_port0;
  assign when_Stream_l955 = (logic_pushing != logic_popping);
  assign logic_ptrDif = (logic_pushPtr_value - logic_popPtr_value);
  assign io_occupancy = {(logic_risingOccupancy && logic_ptrMatch),logic_ptrDif};
  assign io_availability = {((! logic_risingOccupancy) && logic_ptrMatch),_zz_io_availability};
  always @(posedge clk) begin
    if(reset) begin
      logic_pushPtr_value <= 9'h0;
      logic_popPtr_value <= 9'h0;
      logic_risingOccupancy <= 1'b0;
      _zz_io_pop_valid <= 1'b0;
    end else begin
      logic_pushPtr_value <= logic_pushPtr_valueNext;
      logic_popPtr_value <= logic_popPtr_valueNext;
      _zz_io_pop_valid <= (logic_popPtr_valueNext == logic_pushPtr_value);
      if(when_Stream_l955) begin
        logic_risingOccupancy <= logic_pushing;
      end
      if(io_flush) begin
        logic_risingOccupancy <= 1'b0;
      end
    end
  end


endmodule
