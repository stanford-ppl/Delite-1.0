#ifndef HISTOGRAM_H
#define HISTOGRAM_H

#include <list>
#include <vector>
#include <map>
#include <string>
using namespace std;

class Histogram {
  
  unsigned int sampleCount; 
  vector<unsigned int> counts;
  double ln2;
    
  const vector<unsigned int> & getCounts() const { return counts; }

 public:
  
  Histogram();
  
  Histogram(const vector<int> &);

  unsigned int getSampleCount() const { return sampleCount;};
  
  static Histogram binaryHisto(unsigned int zeroCount, unsigned int oneCount);
  
  const list<unsigned int> mostCommon();
  
  Histogram& operator+=(const Histogram & );
  Histogram& operator+=(const unsigned int);

  double shanonEntropy() const;
  
  Histogram operator-(const Histogram & x) const;
  
  string str() const;



};

#endif
