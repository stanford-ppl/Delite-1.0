#include "VarianceStat.h"

#include <iostream>
#include <assert.h>
#include <math.h>
#include <sstream>
using namespace std;

#include "ERT.h"


VarianceStat::VarianceStat() {

#ifdef DEBUG  
  cout << "Constructing VarianceStat\n";
#endif
  
  count = 0;
  sum = 0;
  sumSqr = 0;
  
}

VarianceStat::VarianceStat( const vector<Classification> & vals) {
  
#ifdef DEBUG  
  cout << "Constructing VarianceStat with vals\n";
#endif
  

  sum = 0;
  sumSqr = 0;
  count = vals.size(); 
    
  for(vector<Classification>::const_iterator iter = vals.begin(); iter != vals.end(); iter++) {
    sum += iter[0];
    sumSqr += sqr(iter[0]);
  }

}


  
VarianceStat& VarianceStat::operator+=(const VarianceStat & that ) {
  
  count += that.count;
  sum += that.sum;
  sumSqr += that.sumSqr;
  
  return (*this);
}

VarianceStat & VarianceStat::operator+=(const Classification classif) {
  
  count++;
  sum += classif;
  sumSqr += sqr(classif);    
  
  return (*this);
}

VarianceStat VarianceStat::operator-(const VarianceStat & that ) const {
  
  VarianceStat ret(*this);

  ret.count -= that.count;
  ret.sum -= that.sum;
  ret.sumSqr -= that.sumSqr;
  
  return ret;
}


string VarianceStat::str() const {

  stringstream ret;
  
  ret << "VarianceStat[count=" << count << ", mean=" << getMean() << ", variance=" << getVariance() <<  "]" << endl;
  
  return ret.str();
}

double VarianceStat::relativeVarianceReduction(const VarianceStat &parent, const VarianceStat &left) {
  
  uint rightSampleCount = parent.getSampleCount() - left.getSampleCount();
  if(left.getSampleCount() == 0 || rightSampleCount == 0) {
    return 0;
  }
  
  double pv = parent.getVariance();
  if(pv < 1e-20) {
    return 0.0;
  } else {
    uint n = parent.getSampleCount();
    VarianceStat right = parent - left;
    return 1.0 - (left.getSampleCount() * left.getVariance() + right.getSampleCount() * right.getVariance())/ (n*pv);
  }
  
}



