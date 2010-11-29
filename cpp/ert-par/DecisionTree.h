#ifndef DECISIONTREE_H
#define DECISIONTREE_H

#include <vector>
#include <iostream>
using namespace std;

#include "Histogram.h"

// Classes for different type of nodes
class Node {
 public:    
  virtual Histogram classify(const vector<double> &) = 0;
  virtual ~Node() {}

};

class Leaf : public Node {
  
  Histogram histo;
  
 public:
  Leaf(const Histogram h) {
#ifdef DEBUG
    cout << "Constructing new Leaf\n";
#endif
    this->histo = h;
  };
  Histogram classify(const vector<double> & row) {
    return histo;
  };
    
};

class Split : public Node {
  
  Node * lt;
  Node * ge;
  unsigned int colIndex;
  double pivot;
  
 public:
  
  Split(unsigned int, double, Node*, Node*);
  Histogram classify(const vector<double> & row) {
    if(row[colIndex] < pivot)
      return lt->classify(row);
    else
      return ge->classify(row);
  };
  ~Split() {
    delete lt;
    delete ge;
  };

};


class DecisionTree {
  
  Node * root;
  
  Node * buildNode(const vector<const vector<double> *> & rows, const Histogram histo, const int K, const int NMin);
  void generateRandomSubset(vector<int> &, const unsigned int, const unsigned int);
  double randomPivotValue(const vector<double>& col) ;
  vector<int> colAsInt(const vector<const vector<double> * > & rows, int idx);
  vector<double> colAsDouble(const vector<const vector<double> *> & rows, int idx);
  
  bool sizeLE(const vector<const vector<double> * > &a, const unsigned int n) { return sizeLT(a,n+1);} ;
  bool sizeLT(const vector<const vector<double> * > &a, const unsigned int n) { return a.begin()[0]->size() < n;}

 public:
  
  DecisionTree(const vector<const vector<double> * > &, const Histogram, const int, const int);
  
  const Histogram classify(const vector<double> &) const;

  static void partitionSamples(const vector<const vector<double> * > &, vector<const vector<double> * > * , vector<const vector<double> *> * , const int , const double );
  
};





#endif
