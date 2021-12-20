proc edit_core {core} {

  set bif      [::ipx::get_bus_interfaces -of $core  "m_axi_0"] 
  set bifparam [::ipx::add_bus_parameter -quiet "MAX_BURST_LENGTH" $bif]
  set_property value        64           $bifparam
  set_property value_source constant     $bifparam
  set bifparam [::ipx::add_bus_parameter -quiet "NUM_READ_OUTSTANDING" $bif]
  set_property value        32           $bifparam
  set_property value_source constant     $bifparam
  set bifparam [::ipx::add_bus_parameter -quiet "NUM_WRITE_OUTSTANDING" $bif]
  set_property value        32           $bifparam
  set_property value_source constant     $bifparam
  set bif      [::ipx::get_bus_interfaces -of $core  "m_axi_1"] 
  set bifparam [::ipx::add_bus_parameter -quiet "MAX_BURST_LENGTH" $bif]
  set_property value        64           $bifparam
  set_property value_source constant     $bifparam
  set bifparam [::ipx::add_bus_parameter -quiet "NUM_READ_OUTSTANDING" $bif]
  set_property value        32           $bifparam
  set_property value_source constant     $bifparam
  set bifparam [::ipx::add_bus_parameter -quiet "NUM_WRITE_OUTSTANDING" $bif]
  set_property value        32           $bifparam
  set_property value_source constant     $bifparam
  set bif      [::ipx::get_bus_interfaces -of $core  "req_axi_0"] 
  set bifparam [::ipx::add_bus_parameter -quiet "MAX_BURST_LENGTH" $bif]
  set_property value        64           $bifparam
  set_property value_source constant     $bifparam
  set bifparam [::ipx::add_bus_parameter -quiet "NUM_READ_OUTSTANDING" $bif]
  set_property value        32           $bifparam
  set_property value_source constant     $bifparam
  set bifparam [::ipx::add_bus_parameter -quiet "NUM_WRITE_OUTSTANDING" $bif]
  set_property value        32           $bifparam
  set_property value_source constant     $bifparam
  set bif      [::ipx::get_bus_interfaces -of $core  "req_axi_1"] 
  set bifparam [::ipx::add_bus_parameter -quiet "MAX_BURST_LENGTH" $bif]
  set_property value        64           $bifparam
  set_property value_source constant     $bifparam
  set bifparam [::ipx::add_bus_parameter -quiet "NUM_READ_OUTSTANDING" $bif]
  set_property value        32           $bifparam
  set_property value_source constant     $bifparam
  set bifparam [::ipx::add_bus_parameter -quiet "NUM_WRITE_OUTSTANDING" $bif]
  set_property value        32           $bifparam
  set_property value_source constant     $bifparam

  ::ipx::associate_bus_interfaces -busif "m_axi_0" -clock "ap_clk" $core 
  ::ipx::associate_bus_interfaces -busif "req_axi_0" -clock "ap_clk" $core 
  ::ipx::associate_bus_interfaces -busif "m_axi_1" -clock "ap_clk" $core 
  ::ipx::associate_bus_interfaces -busif "req_axi_1" -clock "ap_clk" $core 

  ::ipx::associate_bus_interfaces -busif "s_axi_control" -clock "ap_clk" $core

  # Specify the freq_hz parameter 
  set clkbif      [::ipx::get_bus_interfaces -of $core "ap_clk"]
  set clkbifparam [::ipx::add_bus_parameter -quiet "FREQ_HZ" $clkbif]
  # Set desired frequency                   
  set_property value 250000000 $clkbifparam
  # set value_resolve_type 'user' if the frequency can vary. 
  set_property value_resolve_type user $clkbifparam
  # set value_resolve_type 'immediate' if the frequency cannot change. 
  # set_property value_resolve_type immediate $clkbifparam
  set mem_map    [::ipx::add_memory_map -quiet "s_axi_control" $core]
  set addr_block [::ipx::add_address_block -quiet "reg0" $mem_map]

  set reg      [::ipx::add_register "CTRL" $addr_block]
  set_property description    "Control signals"    $reg
  set_property address_offset 0x000 $reg
  set_property size           32    $reg
  set field [ipx::add_field AP_START $reg]
    set_property ACCESS {read-write} $field
    set_property BIT_OFFSET {0} $field
    set_property BIT_WIDTH {1} $field
    set_property DESCRIPTION {Control signal Register for 'ap_start'.} $field
    set_property MODIFIED_WRITE_VALUE {modify} $field
  set field [ipx::add_field AP_DONE $reg]
    set_property ACCESS {read-only} $field
    set_property BIT_OFFSET {1} $field
    set_property BIT_WIDTH {1} $field
    set_property DESCRIPTION {Control signal Register for 'ap_done'.} $field
    set_property READ_ACTION {modify} $field
  set field [ipx::add_field AP_IDLE $reg]
    set_property ACCESS {read-only} $field
    set_property BIT_OFFSET {2} $field
    set_property BIT_WIDTH {1} $field
    set_property DESCRIPTION {Control signal Register for 'ap_idle'.} $field
    set_property READ_ACTION {modify} $field
  set field [ipx::add_field AP_READY $reg]
    set_property ACCESS {read-only} $field
    set_property BIT_OFFSET {3} $field
    set_property BIT_WIDTH {1} $field
    set_property DESCRIPTION {Control signal Register for 'ap_ready'.} $field
    set_property READ_ACTION {modify} $field
  set field [ipx::add_field RESERVED_1 $reg]
    set_property ACCESS {read-only} $field
    set_property BIT_OFFSET {4} $field
    set_property BIT_WIDTH {3} $field
    set_property DESCRIPTION {Reserved.  0s on read.} $field
    set_property READ_ACTION {modify} $field
  set field [ipx::add_field AUTO_RESTART $reg]
    set_property ACCESS {read-write} $field
    set_property BIT_OFFSET {7} $field
    set_property BIT_WIDTH {1} $field
    set_property DESCRIPTION {Control signal Register for 'auto_restart'.} $field
    set_property MODIFIED_WRITE_VALUE {modify} $field
  set field [ipx::add_field RESERVED_2 $reg]
    set_property ACCESS {read-only} $field
    set_property BIT_OFFSET {8} $field
    set_property BIT_WIDTH {24} $field
    set_property DESCRIPTION {Reserved.  0s on read.} $field
    set_property READ_ACTION {modify} $field

  set reg      [::ipx::add_register "GIER" $addr_block]
  set_property description    "Global Interrupt Enable Register"    $reg
  set_property address_offset 0x004 $reg
  set_property size           32    $reg

  set reg      [::ipx::add_register "IP_IER" $addr_block]
  set_property description    "IP Interrupt Enable Register"    $reg
  set_property address_offset 0x008 $reg
  set_property size           32    $reg

  set reg      [::ipx::add_register "IP_ISR" $addr_block]
  set_property description    "IP Interrupt Status Register"    $reg
  set_property address_offset 0x00C $reg
  set_property size           32    $reg

  set reg      [::ipx::add_register -quiet "txnLen" $addr_block]
  set_property address_offset 0x010 $reg
  set_property size           [expr {4*8}]   $reg

  set reg      [::ipx::add_register -quiet "txnCnt" $addr_block]
  set_property address_offset 0x014 $reg
  set_property size           [expr {4*8}]   $reg




  set reg      [::ipx::add_register -quiet "addrOffset0" $addr_block]
  set_property address_offset 0x18 $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "addrOffset1" $addr_block]
  set_property address_offset 0x1c $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "addrOffset2" $addr_block]
  set_property address_offset 0x20 $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "addrOffset3" $addr_block]
  set_property address_offset 0x24 $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "addrOffset4" $addr_block]
  set_property address_offset 0x28 $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "addrOffset5" $addr_block]
  set_property address_offset 0x2c $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "addrOffset6" $addr_block]
  set_property address_offset 0x30 $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "addrOffset7" $addr_block]
  set_property address_offset 0x34 $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "txnExeCnt0" $addr_block]
  set_property address_offset 0x38 $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "txnExeCnt1" $addr_block]
  set_property address_offset 0x3c $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "txnExeCnt2" $addr_block]
  set_property address_offset 0x40 $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "txnExeCnt3" $addr_block]
  set_property address_offset 0x44 $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "txnExeCnt4" $addr_block]
  set_property address_offset 0x48 $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "txnExeCnt5" $addr_block]
  set_property address_offset 0x4c $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "txnExeCnt6" $addr_block]
  set_property address_offset 0x50 $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "txnExeCnt7" $addr_block]
  set_property address_offset 0x54 $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "txnAbortCnt0" $addr_block]
  set_property address_offset 0x58 $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "txnAbortCnt1" $addr_block]
  set_property address_offset 0x5c $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "txnAbortCnt2" $addr_block]
  set_property address_offset 0x60 $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "txnAbortCnt3" $addr_block]
  set_property address_offset 0x64 $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "txnAbortCnt4" $addr_block]
  set_property address_offset 0x68 $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "txnAbortCnt5" $addr_block]
  set_property address_offset 0x6c $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "txnAbortCnt6" $addr_block]
  set_property address_offset 0x70 $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "txnAbortCnt7" $addr_block]
  set_property address_offset 0x74 $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "clkCnt" $addr_block]
  set_property address_offset 0x78 $reg
  set_property size           [expr {4*8}]   $reg 

  set reg      [::ipx::add_register -quiet "m_axi_0_ptr0" $addr_block]
  set_property address_offset 0x7c $reg
  set_property size           [expr {8*8}]   $reg
  set regparam [::ipx::add_register_parameter -quiet {ASSOCIATED_BUSIF} $reg]
  set_property value m_axi_0 $regparam


  set reg      [::ipx::add_register -quiet "req_axi_0_ptr0" $addr_block]
  set_property address_offset 0x84 $reg
  set_property size           [expr {8*8}]   $reg
  set regparam [::ipx::add_register_parameter -quiet {ASSOCIATED_BUSIF} $reg]
  set_property value req_axi_0 $regparam


  set reg      [::ipx::add_register -quiet "m_axi_1_ptr0" $addr_block]
  set_property address_offset 0x8c $reg
  set_property size           [expr {8*8}]   $reg
  set regparam [::ipx::add_register_parameter -quiet {ASSOCIATED_BUSIF} $reg]
  set_property value m_axi_1 $regparam


  set reg      [::ipx::add_register -quiet "req_axi_1_ptr0" $addr_block]
  set_property address_offset 0x94 $reg
  set_property size           [expr {8*8}]   $reg
  set regparam [::ipx::add_register_parameter -quiet {ASSOCIATED_BUSIF} $reg]
  set_property value req_axi_1 $regparam

  set_property slave_memory_map_ref "s_axi_control" [::ipx::get_bus_interfaces -of $core "s_axi_control"]

  set_property xpm_libraries {XPM_CDC XPM_MEMORY XPM_FIFO} $core
  set_property sdx_kernel true $core
  set_property sdx_kernel_type rtl $core
}


create_project -force kernel_pack $path_to_tmp_project
# normally an ip includes - a top.v file generated by spinal; external .v/.sv files
add_files -norecurse [glob $path_to_gen_hdl/tmop_multi.v $path_to_ext_hdl/HashTable/*.sv $path_to_ext_hdl/HashTable/*.v]

set __ip_list [get_property ip_repo_paths [current_project]]

set_property ip_repo_paths $__ip_list [current_project]
update_ip_catalog


update_compile_order -fileset sources_1
update_compile_order -fileset sim_1
::ipx::package_project -root_dir $path_to_packaged -vendor xilinx.com -library RTLKernel -taxonomy /KernelIP -import_files -set_current false
::ipx::unload_core $path_to_packaged/component.xml
::ipx::edit_ip_in_project -upgrade true -name tmp_edit_project -directory $path_to_packaged $path_to_packaged/component.xml



set core [::ipx::current_core]

# foreach user_parameter [list C_S_AXI_CONTROL_ADDR_WIDTH C_S_AXI_CONTROL_DATA_WIDTH C_M_AXI_0_ADDR_WIDTH C_M_AXI_0_DATA_WIDTH C_M_AXI_1_ADDR_WIDTH C_M_AXI_1_DATA_WIDTH C_REQ_AXI_0_ADDR_WIDTH C_REQ_AXI_0_DATA_WIDTH C_REQ_AXI_1_ADDR_WIDTH C_REQ_AXI_1_DATA_WIDTH] {
#   ::ipx::remove_user_parameter $user_parameter $core
# }

set_property supported_families { } $core
set_property auto_family_support_level level_2 $core

ipx::create_xgui_files $core

edit_core $core

::ipx::update_checksums $core
::ipx::check_integrity -kernel $core
::ipx::save_core $core
::ipx::unload_core $core
unset core
close_project -delete




