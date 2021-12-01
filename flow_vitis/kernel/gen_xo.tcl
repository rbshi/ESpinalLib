#
# Copyright (C) 2019-2021 Xilinx, Inc
#
# Licensed under the Apache License, Version 2.0 (the "License"). You may
# not use this file except in compliance with the License. A copy of the
# License is located at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations
# under the License.
#

if { $::argc != 7 } {
    puts $::argv
    puts "ERROR: Program \"$::argv0\" requires 7 arguments!\n"
    puts "Usage: $::argv0 <xoname> <krnl_name> <package_tcl_path> <path_to_packaged> <path_to_tmp_project> <path_to_gen_hdl> <path_to_ext_hdl>\n"
    exit
}

set xoname  [lindex $::argv 0]
set krnl_name [lindex $::argv 1]
set package_tcl_path [lindex $::argv 2]
set path_to_packaged [lindex $::argv 3]
set path_to_tmp_project [lindex $::argv 4]
set path_to_gen_hdl [lindex $::argv 5]
set path_to_ext_hdl [lindex $::argv 6]

source -notrace ${package_tcl_path}

if {[file exists "${xoname}"]} {
    file delete -force "${xoname}"
}

package_xo -xo_path ${xoname} -kernel_name ${krnl_name} -ip_directory ${path_to_packaged}