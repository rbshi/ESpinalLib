#include <cstdlib>
#include <cstring>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>
#include <thread>
#include <unistd.h>

#include "experimental/xrt_bo.h"
#include "experimental/xrt_device.h"
#include "experimental/xrt_ini.h"
#include "experimental/xrt_kernel.h"


using namespace std;


void wait_for_enter(const std::string& msg)
{
    std::cout << msg << std::endl;
    std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
}

int main(int argc, char **argv) {


  if (argc != 2) {
    cout << "Usage: ./tmop <.xclbin>" << endl;
    return 1;
  }

  std::string xclbin_fnm = argv[1];
  // int regA = atoi(argv[2]);
  // int regB = atoi(argv[3]);

  srand(3);

  if (xclbin_fnm.empty())
    throw std::runtime_error("FAILED_TEST\nNo xclbin specified");

  std::string cu_name = "tmop";

  unsigned int device_index = 0;

  auto device = xrt::device(device_index);
  auto uuid = device.load_xclbin(xclbin_fnm);

  std::cout << "finish load" << std::endl;


  // Allocate input buffer on HBM
  // int num_channel = 2;
  // int hbm_size = (1<<28); // 256MB  
  // std::vector<xrt::bo> hbm_buffer(num_channel);
  // std::vector<long*> hbm_buffer_ptr(num_channel);


  // for (int i = 0; i < num_channel * 1; i++) {
  //   hbm_buffer[i] = xrt::bo(device, hbm_size, 0, i);
  //   auto hbm_channel_ptr = hbm_buffer[i].map<long*>();
  //   hbm_buffer_ptr[i] = hbm_channel_ptr;
  //   // // move data to hbm, NEED COPY FIRST..
  //   // std::copy_n(ptr_start, in_column.m_num_lines[i] * INTS_IN_HBM_LINE, hbm_buffer_ptr[i]);
  //   memset(hbm_buffer_ptr[i], 0, hbm_size);
  //   hbm_buffer[i].sync(XCL_BO_SYNC_BO_TO_DEVICE, hbm_size/sizeof(long), 0);
  // }



  std::string cu_id = std::to_string(1);
  std::string krnl_name_full = cu_name + ":{" + cu_name + "_" + cu_id + "}";
  auto krnl_inst = xrt::kernel(device, uuid, krnl_name_full, 1);

  std::cout << "get krnl_name" << std::endl;

  wait_for_enter("\nPress ENTER to continue after setting up ILA trigger...");

  auto run = krnl_inst(4, 2, 0, 0);

  std::cout << "set parameters" << std::endl;
  // auto state = run.wait();  
  // auto run = krnl_inst(0, 0, 0, 128);

  sleep(2);

  // std::cout << "reg = " << krnl_inst.read_register(0x00) << std::endl;
  std::cout << "reg = " << krnl_inst.read_register(0x10) << std::endl;
  std::cout << "reg = " << krnl_inst.read_register(0x14) << std::endl;
  std::cout << "reg = " << krnl_inst.read_register(0x20) << std::endl;
  std::cout << "reg = " << krnl_inst.read_register(0x24) << std::endl;
  std::cout << "reg = " << krnl_inst.read_register(0x28) << std::endl;
  std::cout << "reg = " << krnl_inst.read_register(0x2c) << std::endl;

  return 0;
}





