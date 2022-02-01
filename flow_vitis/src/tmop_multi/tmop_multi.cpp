#include <cstdlib>
#include <cstring>
#include <iostream>
#include <iomanip>
#include <stdexcept>
#include <string>
#include <vector>
#include <thread>
#include <random>
#include <unistd.h>
#include <fstream>
#include <boost/program_options.hpp>


#include "experimental/xrt_bo.h"
#include "experimental/xrt_device.h"
#include "experimental/xrt_ini.h"
#include "experimental/xrt_kernel.h"

#include "../lib/util/util.hpp"


using namespace std;
using namespace fpga;

int main(int argc, char *argv[]) {

  // Read arguments
  boost::program_options::options_description programDescription("Options:");
  programDescription.add_options()("numch,h", boost::program_options::value<uint32_t>(), "Number of channel")
                                  ("txnlen,l", boost::program_options::value<uint32_t>(), "txnlen")
                                  ("txncnt,c", boost::program_options::value<uint32_t>(), "txncnt")
                                  ("numpe,n", boost::program_options::value<uint32_t>(), "numpe")
                                  ("wrratio,w", boost::program_options::value<double>(), "wrRatio")
                                  ("iszipfian,z", boost::program_options::value<bool>(), "iszipfian")
                                  ("isnaive,a", boost::program_options::value<bool>(), "isnaive")
                                  ("ztheta,t", boost::program_options::value<double>(), "zipFianTheta")
                                  ("xclbin,b", boost::program_options::value<string>(), "bitstream");
  
  boost::program_options::variables_map commandLineArgs;
  boost::program_options::store(boost::program_options::parse_command_line(argc, argv, programDescription), commandLineArgs);
  boost::program_options::notify(commandLineArgs);

  uint32_t numch = commandLineArgs["numch"].as<uint32_t>();
  // one hbm channel, each tuple with 64 B
  uint64_t gtsize = ((1<<28)*numch)>>6;
  std::cout << "\e[1mGlobalTableSize:\e[0m " << gtsize << std::endl;

  uint32_t txnlen = commandLineArgs["txnlen"].as<uint32_t>();
  uint32_t txncnt = commandLineArgs["txncnt"].as<uint32_t>();
  uint32_t numpe = commandLineArgs["numpe"].as<uint32_t>();
  double wrratio = commandLineArgs["wrratio"].as<double>();
  bool iszipfian = commandLineArgs["iszipfian"].as<bool>();
  bool isnaive = commandLineArgs["isnaive"].as<bool>();
  double ztheta = commandLineArgs["ztheta"].as<double>();
  string xclbin_fnm = commandLineArgs["xclbin"].as<string>();


  std::cout << "\e[1mParameters:\e[0m" << std::setw(3) << "wrRatio: " << wrratio 
    << std::setw(8) << "isZipFian: " << iszipfian 
    << std::setw(15) << "isNaive: " << isnaive << std::endl;

  
  // txnTask
  txnTask txn_task(iszipfian, ztheta, isnaive, gtsize);
  txn_task.keyInit(gtsize, wrratio, txnlen, txncnt, numpe, numch);

  // // FPGA
  if (xclbin_fnm.empty())
    throw std::runtime_error("FAILED_TEST\nNo xclbin specified");

  auto device = xrt::device(0); // deviceIdx 0
  auto uuid = device.load_xclbin(xclbin_fnm);  

  std::vector<xrt::bo> hbm_req_buffer(numch);
  uint32_t hbm_size = (1<<28); // 256MB
  for (int i = 0; i < numch; i++) {
    hbm_req_buffer[i] = xrt::bo(device, hbm_size, 0, numch+i);
    auto inst_ptr = hbm_req_buffer[i].map<long*>();
    for (int ii=0; ii<txnlen*txncnt*numpe/numch; ii++){
      *(inst_ptr+ii*8) = txn_task.getKey();
      *(inst_ptr+ii*8+1) = 0xffffffff;
      *(inst_ptr+ii*8+2) = txn_task.getRW() ? 1 : 0; // rd:0, wr:1
    }
    hbm_req_buffer[i].sync(XCL_BO_SYNC_BO_TO_DEVICE, hbm_size, 0); 
  }


  // get the kernel and start
  std::string cu_name = "tmop_multi";
  std::string cu_id = std::to_string(1);
  std::string krnl_name_full = cu_name + ":{" + cu_name + "_" + cu_id + "}";
  auto krnl_inst = xrt::kernel(device, uuid, krnl_name_full, 1);

  uint64_t addr_offs[numpe];
  for (int ich = 0; ich < numch; ich++){
    for (int ipe=0; ipe<numpe/numch; ipe++){
      addr_offs[ipe+numpe/numch*ich] = hbm_size*(numch+ich) + ipe * txnlen * txncnt * 64;
    }
  }

  // auto run = krnl_inst(txnlen, txncnt, addr_offs[0], addr_offs[1], addr_offs[2], addr_offs[3], addr_offs[4], addr_offs[5], addr_offs[6], addr_offs[7]); // 4x2 PE

  auto run = xrt::run(krnl_inst);
  run.set_arg(0, txnlen);
  run.set_arg(1, txncnt);
  for (int ii = 0; ii < numpe; ii++) {
    run.set_arg(2+ii, addr_offs[ii]);
  }
  run.start();
  std::cout << "Kernel starts..." << std::endl;
  auto state = run.wait();
  sleep(2);

  int reg_execnt_offs = 24 + numpe * 4;
  int reg_abtcnt_offs = 24 + numpe * 4 * 2;
  int reg_clkcnt = 24 + numpe * 4 * 3;

  int abrt_cnt = 0;
  for (int ipe=0; ipe<numpe; ipe++){
    std::cout << "PE[" << ipe << "]: txnExe:" << krnl_inst.read_register(reg_execnt_offs+ipe*4);
    int abrt_num = krnl_inst.read_register(reg_abtcnt_offs+ipe*4) ;
    abrt_cnt += abrt_num;
    std::cout << "\t txnAbt:" << abrt_num << std::endl;
  }
  std::cout << "clkCnt:" << krnl_inst.read_register(reg_clkcnt) << std::endl;
  std::cout << "abtCnt:" << abrt_cnt << std::endl;

  return 0;
}





