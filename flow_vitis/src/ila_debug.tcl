connect_hw_server -url alveo4c.ethz.ch:3121 -allow_non_jtag
current_hw_target [get_hw_targets */xilinx_tcf/Xilinx/21770297400LA]
set_property PARAM.FREQUENCY 15000000 [get_hw_targets */xilinx_tcf/Xilinx/21770297400LA]
open_hw_target
current_hw_device [get_hw_devices xcu280_u55c_0]
refresh_hw_device [lindex [get_hw_devices xcu280_u55c_0] 0]
close_hw_target {alveo4c.ethz.ch:3121/xilinx_tcf/Xilinx/21770297400LA}
open_hw_target -xvc_url localhost:10200
refresh_hw_device [lindex [get_hw_devices debug_bridge_0] 0]
display_hw_ila_data [ get_hw_ila_data hw_ila_data_1 -of_objects [get_hw_ilas -of_objects [get_hw_devices debug_bridge_0] -filter {CELL_NAME=~"pfm_top_i/dynamic_region/tmop_1/inst/ltch_debug"}]]
display_hw_ila_data [ get_hw_ila_data hw_ila_data_2 -of_objects [get_hw_ilas -of_objects [get_hw_devices debug_bridge_0] -filter {CELL_NAME=~"pfm_top_i/dynamic_region/tmop_1/inst/m_axi_debug"}]]
display_hw_ila_data [ get_hw_ila_data hw_ila_data_3 -of_objects [get_hw_ilas -of_objects [get_hw_devices debug_bridge_0] -filter {CELL_NAME=~"pfm_top_i/dynamic_region/tmop_1/inst/req_axi_debug"}]]

set_property TRIGGER_COMPARE_VALUE eq1'bR [get_hw_probes pfm_top_i/dynamic_region/tmop_1/inst/txnManGrp_1_io_lt_req_valid -of_objects [get_hw_ilas -of_objects [get_hw_devices debug_bridge_0] -filter {CELL_NAME=~"pfm_top_i/dynamic_region/tmop_1/inst/ltch_debug"}]]
run_hw_ila [get_hw_ilas -of_objects [get_hw_devices debug_bridge_0] -filter {CELL_NAME=~"pfm_top_i/dynamic_region/tmop_1/inst/ltch_debug"}]
set_property TRIGGER_COMPARE_VALUE eq1'bR [get_hw_probes pfm_top_i/dynamic_region/tmop_1/inst/req_axi_arvalid -of_objects [get_hw_ilas -of_objects [get_hw_devices debug_bridge_0] -filter {CELL_NAME=~"pfm_top_i/dynamic_region/tmop_1/inst/req_axi_debug"}]]
run_hw_ila [get_hw_ilas -of_objects [get_hw_devices debug_bridge_0] -filter {CELL_NAME=~"pfm_top_i/dynamic_region/tmop_1/inst/req_axi_debug"}]

set_property TRIGGER_COMPARE_VALUE eq1'bR [get_hw_probes pfm_top_i/dynamic_region/tmop_1/inst/m_axi_arvalid -of_objects [get_hw_ilas -of_objects [get_hw_devices debug_bridge_0] -filter {CELL_NAME=~"pfm_top_i/dynamic_region/tmop_1/inst/m_axi_debug"}]]
run_hw_ila [get_hw_ilas -of_objects [get_hw_devices debug_bridge_0] -filter {CELL_NAME=~"pfm_top_i/dynamic_region/tmop_1/inst/m_axi_debug"}]

wait_on_hw_ila [get_hw_ilas -of_objects [get_hw_devices debug_bridge_0] -filter {CELL_NAME=~"pfm_top_i/dynamic_region/tmop_1/inst/ltch_debug"}]\
  [get_hw_ilas -of_objects [get_hw_devices debug_bridge_0] -filter {CELL_NAME=~"pfm_top_i/dynamic_region/tmop_1/inst/req_axi_debug"}]
display_hw_ila_data [upload_hw_ila_data [get_hw_ilas -of_objects [get_hw_devices debug_bridge_0] -filter {CELL_NAME=~"pfm_top_i/dynamic_region/tmop_1/inst/ltch_debug"}]]

display_hw_ila_data [upload_hw_ila_data [get_hw_ilas -of_objects [get_hw_devices debug_bridge_0] -filter {CELL_NAME=~"pfm_top_i/dynamic_region/tmop_1/inst/req_axi_debug"}]]

