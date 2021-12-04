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

#define REG_EXECNT_OFFS 0x20
#define REG_ABTCNT_OFFS 0x28

using namespace std;

void wait_for_enter(const std::string& msg)
{
    std::cout << msg << std::endl;
    std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
}

int main(int argc, char **argv) {


  if (argc != 4) {
    cout << "Usage: ./tmop <.xclbin> txnLen txnCnt" << endl;
    return 1;
  }

  std::string xclbin_fnm = argv[1];
  int txnLen = atoi(argv[2]);
  int txnCnt = atoi(argv[3]);

  srand(3);

  if (xclbin_fnm.empty())
    throw std::runtime_error("FAILED_TEST\nNo xclbin specified");

  std::string cu_name = "tmop";

  auto device = xrt::device(0); // deviceIdx 0
  auto uuid = device.load_xclbin(xclbin_fnm);
  std::cout << "Finish load" << std::endl;

  wait_for_enter("\nPress ENTER to continue after setting up ILA trigger...");

  // HBM[0]: txn access HBM[1]: instruction
   int num_channel = 2;
   int hbm_size = (1<<28); // 256MB

  // allocate workload
  xrt::bo hbm_inst_buffer = xrt::bo(device, hbm_size, 0, 1); // instructions in ch1
  auto inst_ptr = hbm_inst_buffer.map<long*>();
  for (int i_pe=0; i_pe<2; i_pe++){
    for (int i_txn=0; i_txn<2; i_txn++){
      for (int i_inst=0; i_inst<txnLen; i_inst++){
        int inst_offs = 8 * (i_pe * txnLen * txnCnt + txnLen * i_txn + i_inst);
        inst_ptr[inst_offs] = i_inst * 64; // address
        inst_ptr[inst_offs+1] = i_pe * i_txn; // data
        inst_ptr[inst_offs+2] = 1; // upgrade:mode
      }
    }
  }
  hbm_inst_buffer.sync(XCL_BO_SYNC_BO_TO_DEVICE, hbm_size/sizeof(long), 0);


  // get the kernel and start
  std::string cu_id = std::to_string(1);
  std::string krnl_name_full = cu_name + ":{" + cu_name + "_" + cu_id + "}";
  auto krnl_inst = xrt::kernel(device, uuid, krnl_name_full, 1);

  auto run = krnl_inst(txnLen, txnCnt, hbm_size + 0, hbm_size + 64 * txnLen * txnCnt); // txnLen, txnCnt, addr0, addr1

  std::cout << "Kernel starts..." << std::endl;
  auto state = run.wait();

  for (int i_pe=0; i_pe<2; i_pe++){
    std::cout << "PE[" << i_pe << "]: txnExe:" << krnl_inst.read_register(REG_EXECNT_OFFS+i_pe*4);
    std::cout << "\t txnAbt:" << krnl_inst.read_register(REG_ABTCNT_OFFS+i_pe*4) << std::endl;
  }

  return 0;
}





