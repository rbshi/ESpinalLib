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


  if (argc != 6) {
    cout << "Usage: ./tmop <.xclbin> txnLen txnCnt numPE numLT" << endl;
    return 1;
  }

  std::string xclbin_fnm = argv[1];
  int txnLen = atoi(argv[2]);
  int txnCnt = atoi(argv[3]);
  int numPE = atoi(argv[4]);
  int numLT = atoi(argv[5]);

  srand(3);

  if (xclbin_fnm.empty())
    throw std::runtime_error("FAILED_TEST\nNo xclbin specified");

  std::string cu_name = "tmop";

  auto device = xrt::device(0); // deviceIdx 0
  auto uuid = device.load_xclbin(xclbin_fnm);
  std::cout << "Finish load" << std::endl;


  // HBM[0]: txn access HBM[1]: instruction
   int num_channel = 2;
   int hbm_size = (1<<28); // 256MB

  // allocate workload
  xrt::bo hbm_inst_buffer = xrt::bo(device, hbm_size, 0, 1); // instructions in ch1
  auto inst_ptr = hbm_inst_buffer.map<long*>();
  for (int i_pe=0; i_pe<numPE; i_pe++){
    for (int i_txn=0; i_txn<txnCnt; i_txn++){
      for (int i_inst=0; i_inst<txnLen; i_inst++){
        int inst_line = i_pe * txnLen * txnCnt + txnLen * i_txn + i_inst;
        int inst_offs = 8 * inst_line;
        inst_ptr[inst_offs] = inst_line * 64 + (inst_line % numLT); // use different address for each txn inst, %numLT is to distribute the lock_req
        inst_ptr[inst_offs+1] = 0; // data
        inst_ptr[inst_offs+2] = 0; // upgrade:mode
      }
    }
  }
  hbm_inst_buffer.sync(XCL_BO_SYNC_BO_TO_DEVICE, hbm_size/sizeof(long), 0);


//  wait_for_enter("\nPress ENTER to continue after setting up ILA...");

  // get the kernel and start
  std::string cu_id = std::to_string(1);
  std::string krnl_name_full = cu_name + ":{" + cu_name + "_" + cu_id + "}";
  auto krnl_inst = xrt::kernel(device, uuid, krnl_name_full, 1);

  int addr_offs[numPE];
  for (int i_pe=0; i_pe<numPE; i_pe++){
    addr_offs[i_pe] = hbm_size + i_pe * txnLen * txnCnt * 64;
  }

//  auto run = krnl_inst(txnLen, txnCnt, addr_offs[0], addr_offs[1]); // 2 PE
  auto run = krnl_inst(txnLen, txnCnt, addr_offs[0], addr_offs[1], addr_offs[2], addr_offs[3]); // 4 PE

  std::cout << "Kernel starts..." << std::endl;
  auto state = run.wait();


  int reg_execnt_offs = 24 + numPE * 4;
  int reg_abtcnt_offs = 24 + numPE * 4 * 2;
  int reg_clkcnt = 24 + numPE * 4 * 3;

  for (int i_pe=0; i_pe<numPE; i_pe++){
    std::cout << "PE[" << i_pe << "]: txnExe:" << krnl_inst.read_register(reg_execnt_offs+i_pe*4);
    std::cout << "\t txnAbt:" << krnl_inst.read_register(reg_abtcnt_offs+i_pe*4) << std::endl;
  }
  std::cout << "clkCnt:" << krnl_inst.read_register(reg_clkcnt) << std::endl;

  return 0;
}





