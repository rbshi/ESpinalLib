cmake_minimum_required(VERSION 3.0)

project(vitis_prj)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR})
find_package(Vitis)
find_package(Threads)

# Platform
set(DSA_NAME "xilinx_u280_xdma_201920_3" CACHE STRING "DSA string for v++/xocc.")
set(KERNEL_NAME "tmop" CACHE STRING "Kernel name.")
set(HOST_NAME ${KERNEL_NAME} CACHE STRING "Host executable name.")
# set(IP_DIR $ENV{HOME}/Workspace/hw/ip/${KERNEL_NAME}_ex/exports CACHE STRING "Path of .xo file.")
set(IP_DIR ${CMAKE_BINARY_DIR}/ip/exports CACHE STRING "Path of .xo file.")

# From Step in v++
set(VPP_FROM_STEP "system_link" CACHE STRING "From step in v++ compilation.")

# Common src and head that comes with Xilinx VITIS example
set(OCL_COMMON_SRC ${CMAKE_CURRENT_SOURCE_DIR}/common/includes/xcl2/xcl2.cpp)
set(OCL_COMMON_INCLUDE ${CMAKE_CURRENT_SOURCE_DIR}/common/includes/xcl2)

#FIXME: should VIVADO_INCLUDE be added in to FindVitis?
set(CXXFLAGS -g -std=c++14 -I${Vitis_INCLUDE_DIRS} -I{OCL_COMMON_INCLUDE} -I${XRT_ROOT}/include -I${VITIS_ROOT}/../../Vivado/${Vitis_VERSION}/include -luuid)
set(LDFLAGS -L${Vitis_LIBRARIES} -lOpenCL -pthread -L${XRT_ROOT}/lib)
set(HOST_SRC ${OCL_COMMON_SRC} ${CMAKE_CURRENT_SOURCE_DIR}/src/${HOST_NAME}.cpp)

include_directories(${Vitis_INCLUDE_DIRS} ${OCL_COMMON_INCLUDE})
link_directories(${XRT_ROOT}/lib/)

# Host
add_executable(${HOST_NAME} ${HOST_SRC})
target_link_libraries(${HOST_NAME} ${Vitis_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} m pthread uuid xrt_coreutil)

#FIXME: only support one kernel now
function(binary_container_build TARGET)
    set(TEMP_DIR ${CMAKE_BINARY_DIR}/_x.${TARGET}.${DSA_NAME})
    set(BUILD_DIR ${CMAKE_BINARY_DIR}/build_dir.${TARGET}.${DSA_NAME})
    
    set(VPP_FLAGS -t ${TARGET} --platform ${DSA_NAME} --save-temps)
    if(NOT ${TARGET} EQUAL "hw")
        set(VPP_FLAGS ${VPP_FLAGS} -g)
    endif()


    if(DEFINED KERNEL_CFG_FILE)
        set(VPP_LDFLAGS_KERNEL --config ${CMAKE_CURRENT_SOURCE_DIR}/cfg/${KERNEL_CFG_FILE})
    # check if kernel config file (.cfg) exist
    elseif (EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/cfg/${KERNEL_NAME}.cfg)
        set(VPP_LDFLAGS_KERNEL --config ${CMAKE_CURRENT_SOURCE_DIR}/cfg/${KERNEL_NAME}.cfg)
    endif()

    add_custom_command(
        OUTPUT ${BUILD_DIR}/${KERNEL_NAME}.xclbin
        COMMENT "Link kernel code (.xclbin)"
        COMMAND ${Vitis_COMPILER} ${VPP_FLAGS} -l ${VPP_LDFLAGS} --temp_dir ${TEMP_DIR} ${VPP_LDFLAGS_KERNEL} 
                --log_dir ${TEMP_DIR}/logs --from_step ${VPP_FROM_STEP}
                -o${BUILD_DIR}/${KERNEL_NAME}.xclbin ${IP_DIR}/${KERNEL_NAME}.xo
        DEPENDS ${IP_DIR}/${KERNEL_NAME}.xo)
endfunction()


set(BUILD_MODE "hw")
foreach(mode IN LISTS BUILD_MODE)
    message(STATUS "Add MODE ${mode}")
    binary_container_build(${mode})
endforeach()

add_custom_command(
    OUTPUT ${CMAKE_BINARY_DIR}/emconfig.json
    COMMENT "Generating emconfig.json file for hardware emulation."
    COMMAND ${VITIS_ROOT}/bin/emconfigutil --platform ${DSA_NAME})

set(IP_PACKAGE_DIR ${CMAKE_BINARY_DIR}/ip/${KERNEL_NAME}.${DSA_NAME})
set(IP_TMP_DIR ${CMAKE_BINARY_DIR}/_x.ip/${KERNEL_NAME}.${DSA_NAME})
set(RTL_GEN_SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../rtl)
set(RTL_EXT_SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../rtl_src)
add_custom_target(hw_ip
    COMMENT "Package hardware ips."
    COMMAND vivado -mode batch -source ${CMAKE_CURRENT_SOURCE_DIR}/kernel/gen_xo.tcl -tclargs ${IP_DIR}/${KERNEL_NAME}.xo ${KERNEL_NAME} ${CMAKE_CURRENT_SOURCE_DIR}/kernel/${KERNEL_NAME}_package.tcl ${IP_PACKAGE_DIR} ${IP_TMP_DIR} ${RTL_GEN_SRC_DIR} ${RTL_EXT_SRC_DIR}
    )

add_custom_target(hw
    COMMENT "Build hardware."
    DEPENDS ${CMAKE_BINARY_DIR}/build_dir.hw.${DSA_NAME}/${KERNEL_NAME}.xclbin)

add_custom_target(clean_hw
    COMMENT "Cleaning all hw files."
    COMMAND rm -rf ${CMAKE_BINARY_DIR}/_x.hw.${DSA_NAME}
    COMMAND rm -rf ${CMAKE_BINARY_DIR}/build_dir.hw.${DSA_NAME})
