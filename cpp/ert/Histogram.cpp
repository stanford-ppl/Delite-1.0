#include "Histogram.h"

#include <iostream>
#include <assert.h>
#include <math.h>
#include <sstream>
using namespace std;

#include "ERT.h"


Histogram::Histogram() {

#ifdef DEBUG  
  cout << "Constructing Histogram\n";
#endif
  
  sampleCount = 0;
  ln2 = log(2);
}

Histogram::Histogram( const vector<int> & vals) {
  
#ifdef DEBUG  
  cout << "Constructing Histogram with vals\n";
#endif
  
  ln2= log(2);
  sampleCount = 0;

  for(vector<int>::const_iterator iter = vals.begin(); iter != vals.end(); iter++) {
    if(iter[0] + 1 > (int) counts.size()) {
      counts.resize(iter[0]+1);
    }
    counts[*iter]++;
    sampleCount++;
  }
  



}


const list<unsigned int> Histogram::mostCommon() {
  
  list<unsigned int> mostCommon;
  unsigned int bestCount = 0;

  for(unsigned int i = 0 ; i < counts.size(); i++) {
    if(counts[i] > bestCount) {
      bestCount = counts[i];
      mostCommon.clear();
    }
    if(counts[i] == bestCount) {
      mostCommon.push_back(i);
    }
  }
  
  return mostCommon;
}
  
Histogram& Histogram::operator+=(const Histogram & that ) {
  
  if(that.counts.size() + 1 > counts.size()) {
    counts.resize(that.counts.size() + 1);
  }
  
  for(unsigned int i = 0; i < that.counts.size(); i++) {
    counts[i] += that.counts[i];
  }
  sampleCount += that.sampleCount;

  return (*this);
}

Histogram& Histogram::operator+=(const unsigned int sample) {
  
  if(sample + 1 >  counts.size()) {
    counts.resize(sample + 1);
  }
  counts[sample]++;
  sampleCount++;
  
  return (*this);
}

Histogram Histogram::operator-(const Histogram & that ) const {
  
  Histogram ret(*this);

#ifdef DEBUG
  assert(that.counts.size() <= ret.counts.size());
#endif

  for(unsigned int i = 0; i < that.counts.size(); i++) {
#ifdef DEBUG
    assert(ret.counts[i] >= that.counts[i]);
#endif
    ret.counts[i] -= that.counts[i];
    
  }
  ret.sampleCount -= that.sampleCount;

#ifdef DEBUG
  assert(ret.sampleCount >= 0);
#endif
  return ret;
}


Histogram Histogram::binaryHisto(unsigned int zeroCount, unsigned int oneCount) {
  
  Histogram ret;
  
  ret.counts.resize(2);
  
  if(zeroCount) 
    ret.counts[0] = zeroCount;
  
  if(oneCount)
    ret.counts[1] = oneCount;
  
  ret.sampleCount = zeroCount + oneCount;

  return ret;  
}

double Histogram::shanonEntropy() const {
  
  unsigned int total = sampleCount;
  double invTotal = 1.0/total;
  
  double result = 0;
  
  for(unsigned int i = 0; i < counts.size(); i++) {
    if(counts[i] == 0 || counts[i] == total) {
      continue;
    } else {
      double p = counts[i] * invTotal;
      // p * log2(p)
      result += p * (log(p)/ln2);
    }

  }


  return result;
}

string Histogram::str() const {

  stringstream ret;
  
  ret << "Histogram with " << this->getSampleCount() << " samples\n---------------------------------------------------\n";
  for(unsigned int i = 0; i < counts.size(); i++) {
    if(counts[i]>0)
    ret << "class: " << i << "  count: " << counts[i] << " \n";
  }
  ret << "---------------------------------------------------\n";
  
  return ret.str();
}



