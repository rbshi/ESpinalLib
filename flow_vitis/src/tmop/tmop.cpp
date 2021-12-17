#include <cstdlib>
#include <cstring>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>
#include <thread>
#include <random>
#include <unistd.h>

#include "experimental/xrt_bo.h"
#include "experimental/xrt_device.h"
#include "experimental/xrt_ini.h"
#include "experimental/xrt_kernel.h"


#include "../lib/foedus/uniform_random.hpp"
#include "../lib/foedus/zipfian_random.hpp"

using namespace std;


void wait_for_enter(const std::string& msg)
{
    std::cout << msg << std::endl;
    std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
}

int txn_gen(long *mem, const uint32_t txn_len, const uint32_t txn_cnt, const uint64_t tup_offs, const uint32_t n_lt, const bool lt_dist, const bool rw){
  for (uint itxn = 0; itxn < txn_cnt; itxn++){
    for (uint iop = 0; iop < txn_len; iop++){
      uint64_t ntup = txn_len*itxn+iop;
      *(mem+(tup_offs+ntup)*8) = (ntup << 6) + (lt_dist ? ntup % n_lt : 0); // addr
      *(mem+(tup_offs+ntup)*8+1) = 0xffffffff;
      *(mem+(tup_offs+ntup)*8+2) = rw ? 1 : 0; // rd:0, wr:1
    }
  }
  return 0;
}


uint64_t GenerateKey(int isZipFan, uint32_t g_table_size, foedus::assorted::ZipfianRandom zipfian_rng, foedus::assorted::UniformRandom uniform_rng) {
  uint64_t r = 0;
  if (isZipFan) {
    r = zipfian_rng.next();
  } else {
    r = uniform_rng.uniform_within(0, g_table_size - 1);
  }
  return r;
}



int main(int argc, char **argv) {

  // one hbm channel, each tuple with 64 B
  uint32_t g_table_size = (1<<28)/64;
  double g_zipfian_theta = 0.5;
  double g_wr_ratio = 0.5;


  if (argc != 9) {
    cout << "Usage: ./tmop <.xclbin> txnLen txnCnt numPE isZipFian wrRatio zipFianTheta useNaive" << endl;
    return 1;
  }

  std::string xclbin_fnm = argv[1];
  int txnLen = atoi(argv[2]);
  int txnCnt = atoi(argv[3]);
  int numPE = atoi(argv[4]);
  int isZipFian = atoi(argv[5]);
  g_zipfian_theta = atof(argv[6]);
  g_wr_ratio = atof(argv[7]);  
  int useNaive = atoi(argv[8]);

  srand(3);

  // 
  foedus::assorted::ZipfianRandom zipfian_rng;
  foedus::assorted::UniformRandom uniform_rng; 

  // initialize key gen
  if (isZipFian) {
    zipfian_rng.init(g_table_size, g_zipfian_theta, 1237);
  } else {
    uniform_rng.set_current_seed(1237);
  }

  std::vector<uint64_t> keys;
  std::vector<bool> rw;

  if(!useNaive){
    for (int ii=0; ii<txnLen*txnCnt*numPE; ii++){
      if(isZipFian){
        keys.push_back(zipfian_rng.next());
      } else {
        keys.push_back(uniform_rng.uniform_within(0, g_table_size - 1));
      }
      rw.push_back(((double)rand() / ((double)RAND_MAX + 1)<g_wr_ratio)?true:false);
      // std::cout << "key =" << keys.back() << "\t rw = " << rw.back() << std::endl;
    }
  } else{
    for (int ii=0; ii<txnLen*txnCnt*numPE; ii++){
      keys.push_back(ii);
      rw.push_back(((double)rand() / ((double)RAND_MAX + 1)<g_wr_ratio)?true:false);
    }
  }
  
  // for (int ii=0; ii<txnLen*txnCnt*numPE; ii++){
  //   if(isZipFian){
  //     keys.push_back(zipfian_rng.next());
  //   } else {
  //     keys.push_back(uniform_rng.uniform_within(0, g_table_size - 1));
  //   }
  //   rw.push_back(((double)rand() / ((double)RAND_MAX + 1)<g_wr_ratio)?true:false);
  //   // std::cout << "key =" << keys.back() << "\t rw = " << rw.back() << std::endl;
  // }




  // confirm there's no repeat key in one txn
  for (int ii=0; ii<txnCnt*numPE;){
    bool ckflag = false;
    for (int jj=0; jj<txnLen; jj++){
      for (int kk=jj+1; kk<txnLen; kk++){
        if(keys[ii*txnLen+jj]==keys[ii*txnLen+kk]){
          keys[ii*txnLen+kk] += 1;
          // std::cout << "re-assign redundant key[" << ii*txnLen+kk << "] to " << keys[ii*txnLen+kk] << std::endl;
          ckflag = true;
        }
      }
    }
    if (!ckflag) ii++;
  }


  if (xclbin_fnm.empty())
    throw std::runtime_error("FAILED_TEST\nNo xclbin specified");

  std::string cu_name = "tmop";

  auto device = xrt::device(0); // deviceIdx 0
  auto uuid = device.load_xclbin(xclbin_fnm);
  std::cout << "Finish load" << std::endl;


  // HBM[0]: txn access HBM[1]: instruction
  int hbm_size = (1<<28); // 256MB

  // allocate workload
  xrt::bo hbm_inst_buffer = xrt::bo(device, hbm_size, 0, 1); // instructions in ch1
  auto inst_ptr = hbm_inst_buffer.map<long*>(); // each txn inst takes 4 long word (512 b)


  for (int ii=0; ii<txnLen*txnCnt*numPE; ii++){
    // *(inst_ptr+ii*8) = (keys.back() << 6) + (lt_dist ? ntup % n_lt : 0); // addr
    *(inst_ptr+ii*8) = keys.back();
    *(inst_ptr+ii*8+1) = 0xffffffff;
    *(inst_ptr+ii*8+2) = rw.back() ? 1 : 0; // rd:0, wr:1
    keys.pop_back(); rw.pop_back();
  }

  hbm_inst_buffer.sync(XCL_BO_SYNC_BO_TO_DEVICE, hbm_size, 0); 


  wait_for_enter("Setup ILA...");


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





