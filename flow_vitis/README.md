# Readme

This project is based on the Xilinx [vector addition example](https://github.com/Xilinx/Vitis_Accel_Examples/tree/2020.2/rtl_kernels/rtl_vadd). 

## Workflow

* (tested with Vitis 2020.2)
* add a folder outside the source project: e.g. `mkdir ../build && cd ../build`
* define the parameters in cmake, a basic command example: `cmake ../vitis_rtl -DKERNEL_NAME=krnl_vadd_rtl -DDSA_NAME=xilinx_u280_xdma_201920_3`
* package RTL kernel as an IP (.xo): `make hw_ip_build`
* build hardware: `make hw`
* build host side binary: `make`, the host code should be in `vitis_rtl/src` and has the same name with the kernel
