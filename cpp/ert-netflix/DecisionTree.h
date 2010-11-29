#ifndef DECISIONTREE_H
#define DECISIONTREE_H

#include <vector>
#include <iostream>
using namespace std;

#include "VarianceStat.h"
#include "types.h"

// Classes for different type of nodes
class Node {
 public:    
  virtual VarianceStat classify(const Row &) = 0;
  virtual ~Node() {}

};

class Leaf : public Node {
  
  VarianceStat varStat;
  
 public:
  Leaf(const VarianceStat vs) {
#ifdef DEBUG
    cout << "Constructing new Leaf\n";
#endif
    this->varStat = vs;
  };
  VarianceStat classify(const Row & row) {
    return varStat;
  };
    
};

class Split : public Node {
  
  Node * lt;
  Node * ge;
  unsigned int colIndex;
  double pivot;
  
 public:
  
  Split(unsigned int, double, Node*, Node*);
  VarianceStat classify(const Row & row) {
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
  
  Node * buildNode(const vector<const Row *> & rows, const vector<Classification> &,const VarianceStat & vs, const int K, const int NMin);
  void generateRandomSubset(vector<uint> &, const unsigned int, const unsigned int);
  Attribute randomPivotValue(const AttributeColumn& col) ;
  
  bool sizeLE(const vector<const Row * > &a, const uint n) { return sizeLT(a,n+1);} ;
  bool sizeLT(const vector<const Row * > &a, const uint n) { return a.begin()[0]->size() < n;}

 public:
  
  DecisionTree(const vector<const Row * > &, const vector<Classification> &, const VarianceStat & vs, const int, const int);
  
  const VarianceStat classify(const Row &) const;
  
  
  static void partitionSamples(const vector<const Row * > &rows, 
			       const vector<Classification> & trainingClasses, 
			       vector<const Row *> * ltSamples, 
			       vector<Classification> * ltClasses, 
			       vector<const Row *> * geSamples, 
			       vector<Classification> * geClasses, 
			       const int colIndex, 
			       const double pivot);
    
};





#endif
