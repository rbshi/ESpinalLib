open_hw_manager
connect_hw_server -url alveo3c.ethz.ch:3121 -allow_non_jtag
open_hw_target -xvc_url localhost:10200
current_hw_target [get_hw_targets */xilinx_tcf/Xilinx/localhost:10200]
set_property PARAM.FREQUENCY 10000000 [get_hw_targets */xilinx_tcf/Xilinx/localhost:10200]
open_hw_target
current_hw_device [get_hw_devices debug_bridge_0]
refresh_hw_device -update_hw_probes false [lindex [get_hw_devices debug_bridge_0] 0]

set_property PROBES.FILE {/home/runshi/Workspace/hw/build/tmop_ila4/build_dir.hw.xilinx_u280_xdma_201920_3/tmop.ltx} [get_hw_devices debug_bridge_0]
set_property FULL_PROBES.FILE {/home/runshi/Workspace/hw/build/tmop_ila4/build_dir.hw.xilinx_u280_xdma_201920_3/tmop.ltx} [get_hw_devices debug_bridge_0]
refresh_hw_device [lindex [get_hw_devices debug_bridge_0] 0]
display_hw_ila_data [ get_hw_ila_data hw_ila_data_1 -of_objects [get_hw_ilas -of_objects [get_hw_devices debug_bridge_0] -filter {CELL_NAME=~"pfm_top_i/dynamic_region/tmop_1/inst/ltch_debug"}]]
display_hw_ila_data [ get_hw_ila_data hw_ila_data_2 -of_objects [get_hw_ilas -of_objects [get_hw_devices debug_bridge_0] -filter {CELL_NAME=~"pfm_top_i/dynamic_region/tmop_1/inst/m_axi_debug"}]]
display_hw_ila_data [ get_hw_ila_data hw_ila_data_3 -of_objects [get_hw_ilas -of_objects [get_hw_devices debug_bridge_0] -filter {CELL_NAME=~"pfm_top_i/dynamic_region/tmop_1/inst/req_axi_debug"}]]

set_property TRIGGER_COMPARE_VALUE eq1'bR [get_hw_probes pfm_top_i/dynamic_region/tmop_1/inst/txnManGrp_1_io_lt_req_valid -of_objects [get_hw_ilas -of_objects [get_hw_devices debug_bridge_0] -filter {CELL_NAME=~"pfm_top_i/dynamic_region/tmop_1/inst/ltch_debug"}]]
set_property TRIGGER_COMPARE_VALUE eq1'bR [get_hw_probes pfm_top_i/dynamic_region/tmop_1/inst/m_axi_awvalid -of_objects [get_hw_ilas -of_objects [get_hw_devices debug_bridge_0] -filter {CELL_NAME=~"pfm_top_i/dynamic_region/tmop_1/inst/m_axi_debug"}]]
set_property TRIGGER_COMPARE_VALUE eq1'bR [get_hw_probes pfm_top_i/dynamic_region/tmop_1/inst/req_axi_arvalid -of_objects [get_hw_ilas -of_objects [get_hw_devices debug_bridge_0] -filter {CELL_NAME=~"pfm_top_i/dynamic_region/tmop_1/inst/req_axi_debug"}]]
run_hw_ila [get_hw_ilas -of_objects [get_hw_devices debug_bridge_0] -filter {CELL_NAME=~"pfm_top_i/dynamic_region/tmop_1/inst/ltch_debug"}]
run_hw_ila [get_hw_ilas -of_objects [get_hw_devices debug_bridge_0] -filter {CELL_NAME=~"pfm_top_i/dynamic_region/tmop_1/inst/m_axi_debug"}]
run_hw_ila [get_hw_ilas -of_objects [get_hw_devices debug_bridge_0] -filter {CELL_NAME=~"pfm_top_i/dynamic_region/tmop_1/inst/req_axi_debug"}]
