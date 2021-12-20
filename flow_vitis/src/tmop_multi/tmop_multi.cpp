#include <cstdlib>
#include <cstring>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>
#include <thread>
#include <random>
#include <unistd.h>
#include <fstream>

#include "experimental/xrt_bo.h"
#include "experimental/xrt_device.h"
#include "experimental/xrt_ini.h"
#include "experimental/xrt_kernel.h"


#include "../lib/foedus/uniform_random.hpp"
#include "../lib/foedus/zipfian_random.hpp"

using namespace std;

template<typename T>
void pop_front(std::vector<T> &v)
{
    if (v.size() > 0) {
        v.erase(v.begin());
    }
}

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
  uint32_t g_table_size = (1<<29)/64;

  if (argc != 9) {
    cout << "Usage: ./tmop_multi <.xclbin> txnLen txnCnt numPE wrRatio useNaive isZipFian zipFianTheta" << endl;
    return 1;
  }

  std::string xclbin_fnm = argv[1];
  int txnLen = atoi(argv[2]);
  int txnCnt = atoi(argv[3]);
  int numPE = atoi(argv[4]);
  double g_wr_ratio = atof(argv[5]);  
  int useNaive = atoi(argv[6]);
  int isZipFian = atoi(argv[7]);
  double g_zipfian_theta = atof(argv[8]);

  std::cout <<  "wr_ratio=" << g_wr_ratio << "\t zipfian_theta=" << g_zipfian_theta << std::endl;

  srand(3);

  // rand number generator
  foedus::assorted::ZipfianRandom zipfian_rng;
  foedus::assorted::UniformRandom uniform_rng; 

  // initialize key gen
  if (isZipFian) {
    zipfian_rng.init(g_table_size, g_zipfian_theta, 1237);
  } else {
    uniform_rng.set_current_seed(1237);
  }

  std::vector<uint64_t> keys;
  std::vector<bool> rw; // read or write

  
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
  } else{ // with Naive
    for (int ii=0; ii<txnLen*txnCnt*numPE; ii++){
      keys.push_back(ii%(txnLen*txnCnt));
      // keys.push_back(ii);
      rw.push_back(((double)rand() / ((double)RAND_MAX + 1)<g_wr_ratio)?true:false);
    }
  }

  int aa = 0;
  // confirm there's no repeat key in one txn
  for (int ii=0; ii<txnCnt*numPE;){
    bool ckflag = false;
    for (int jj=0; jj<txnLen; jj++){
      for (int kk=jj+1; kk<txnLen; kk++){
        if(keys[ii*txnLen+jj]==keys[ii*txnLen+kk]){
          keys[ii*txnLen+kk] += 1;
          // std::cout << "re-assign redundant key[" << ii*txnLen+kk << "] to " << keys[ii*txnLen+kk] << std::endl;
          ckflag = true;
          aa++;
        }
      }
    }
    if (!ckflag) ii++;
  }

  std::cout << "replaced intra-txn repeat number =" << aa << std::endl;
  std::cout << "key size = " << keys.size() << "\t rw size = " << rw.size() << std::endl;

  std::ofstream outFile("my_file.txt");
  for (const uint32_t &e : keys) outFile << e << std::endl;
  outFile.close();
  
  





  // FPGA
  if (xclbin_fnm.empty())
    throw std::runtime_error("FAILED_TEST\nNo xclbin specified");

  std::string cu_name = "tmop_multi";

  auto device = xrt::device(0); // deviceIdx 0
  auto uuid = device.load_xclbin(xclbin_fnm);
  std::cout << "Finish load" << std::endl;


  // HBM[0]: txn access HBM[1]: instruction
  int hbm_size = (1<<28); // 256MB

  // allocate workload
  xrt::bo hbm_inst_buffer = xrt::bo(device, hbm_size, 0, 2); // instructions in ch2
  auto inst_ptr = hbm_inst_buffer.map<long*>(); // each txn inst takes 4 long word (512 b)

  for (int ii=0; ii<txnLen*txnCnt*numPE; ii++){
    *(inst_ptr+ii*8) = keys.front();
    *(inst_ptr+ii*8+1) = 0xffffffff;
    *(inst_ptr+ii*8+2) = rw.front() ? 1 : 0; // rd:0, wr:1
    pop_front(keys); pop_front(rw);
  }

  hbm_inst_buffer.sync(XCL_BO_SYNC_BO_TO_DEVICE, hbm_size, 0); 


  // wait_for_enter("Setup ILA...");


  // get the kernel and start
  std::string cu_id = std::to_string(1);
  std::string krnl_name_full = cu_name + ":{" + cu_name + "_" + cu_id + "}";
  auto krnl_inst = xrt::kernel(device, uuid, krnl_name_full, 1);

  int addr_offs[numPE];
  for (int i_pe=0; i_pe<numPE; i_pe++){
    addr_offs[i_pe] = hbm_size*2 + i_pe * txnLen * txnCnt * 64;
  }

//  auto run = krnl_inst(txnLen, txnCnt, addr_offs[0], addr_offs[1]); // 2 PE
  auto run = krnl_inst(txnLen, txnCnt, addr_offs[0], addr_offs[1], addr_offs[2], addr_offs[3], addr_offs[4], addr_offs[5], addr_offs[6], addr_offs[7]); // 4x2 PE

  std::cout << "Kernel starts..." << std::endl;
  // auto state = run.wait();
  sleep(2);

  int reg_execnt_offs = 24 + numPE * 4;
  int reg_abtcnt_offs = 24 + numPE * 4 * 2;
  int reg_clkcnt = 24 + numPE * 4 * 3;

  int abrt_cnt = 0;
  for (int i_pe=0; i_pe<numPE; i_pe++){
    std::cout << "PE[" << i_pe << "]: txnExe:" << krnl_inst.read_register(reg_execnt_offs+i_pe*4);
    int abrt_num = krnl_inst.read_register(reg_abtcnt_offs+i_pe*4) ;
    abrt_cnt += abrt_num;
    std::cout << "\t txnAbt:" << abrt_num << std::endl;
  }
  std::cout << "clkCnt:" << krnl_inst.read_register(reg_clkcnt) << std::endl;
  std::cout << "abtCnt:" << abrt_cnt << std::endl;

  return 0;
}





