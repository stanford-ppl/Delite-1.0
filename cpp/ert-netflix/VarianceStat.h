#ifndef __VARIANCESTAT_H
#define __VARIANCESTAT_H

#include <list>
#include <vector>
#include <map>
#include <string>
using namespace std;

#include "types.h"

class VarianceStat {
  
  uint count; 
  Classification sum;
  Classification sumSqr;
  
  static Classification sqr(Classification classif) {return classif * classif; };
    
 public:
  
  VarianceStat();
  
  VarianceStat(const vector<Classification> &);

  uint getSampleCount() const { return count;};
  
  inline Classification getMean() const { return sum/count;};
  inline double getVariance() const { return sumSqr / count - sqr(getMean()); };
    
  VarianceStat& operator+=(const VarianceStat & );
  VarianceStat& operator+=(const Classification);

  VarianceStat operator-(const VarianceStat & x) const;
  
  static double relativeVarianceReduction(const VarianceStat &parent, const VarianceStat &left);
  
  string str() const;



};

#endif
