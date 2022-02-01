#pragma once

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

#include "../foedus/uniform_random.hpp"
#include "../foedus/zipfian_random.hpp"

#define DBG1(msg) do { std::cout << msg << std::endl; } while( false )

namespace fpga {

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

class txnTask {

private:
  bool iszipfian, isnaive;
  double ztheta, wrratio;
  uint64_t gtsize;
  uint32_t txnlen, txncnt, numpe;

  // TODO: change to map
  std::vector<uint64_t> keys;
  std::vector<bool> rw; // read or write

  foedus::assorted::ZipfianRandom zipfian_rng;
  foedus::assorted::UniformRandom uniform_rng;

public:

  txnTask(bool iszipfian, double ztheta, bool isnaive, uint64_t gtsize) : iszipfian(iszipfian), ztheta(ztheta), isnaive(isnaive), gtsize(gtsize) {
    // srand(3);
    if(iszipfian) {
      zipfian_rng.init(gtsize, ztheta, 1237);
    } else {
      uniform_rng.set_current_seed(1237);
    }
  }

  ~txnTask(){};

  // initialize key gen
  void keyInit(uint64_t gtsize, double wrratio, uint32_t txnlen, uint32_t txncnt, uint32_t numpe, uint32_t numch){
    if(!isnaive){
      for (int ii=0; ii<txnlen*txncnt*numpe; ii++){
        if(iszipfian){
          keys.push_back(zipfian_rng.next());
        } else {
          keys.push_back(uniform_rng.uniform_within(0, gtsize - 1));
        }
        rw.push_back(((double)rand() / ((double)RAND_MAX + 1)<wrratio)?true:false);
      }
    } else{
      for (int ich=0; ich<numch; ich++){
        // intentionally separate txn to individual channels
        for (int ii=0; ii<txnlen*txncnt*numpe/numch; ii++){
          keys.push_back(ii + ich*(1<<22));
          rw.push_back(((double)rand() / ((double)RAND_MAX + 1)<wrratio)?true:false);
        }
      }
    }

    if(iszipfian){
      // remove repeat
      int nrpt = 0;
      // confirm there's no repeat key in one txn
      for (int ii=0; ii<txncnt*numpe;){
        bool ckflag = false;
        for (int jj=0; jj<txnlen; jj++){
          for (int kk=jj+1; kk<txnlen; kk++){
            if(keys[ii*txnlen+jj]==keys[ii*txnlen+kk]){
              keys[ii*txnlen+kk] += 1;
              ckflag = true;
              nrpt++;
            }
          }
        }
        if (!ckflag) ii++;
      }
      if(nrpt){DBG1("replaced intra-txn repeat number =" << nrpt);}
    }

  }

  uint64_t getKey(){
    uint64_t val = keys.front();
    pop_front(keys);
    return val;
  }

  bool getRW(){
    bool val = rw.front();
    pop_front(rw);
    return val;
  }

  uint64_t getSize(){
    return keys.size();
  }  


};

}