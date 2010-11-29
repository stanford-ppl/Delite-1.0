#include <assert.h>
#include <stdlib.h>
#include <limits>
#include <set>
using namespace std;

#include "DecisionTree.h"
#include "ERT.h"



extern unsigned int randSeed;
extern struct drand48_data randBuffer;

DecisionTree::DecisionTree(const vector<const Row * > & rows, const vector<Classification> & trainingClasses, const VarianceStat &vs, const int K, const int NMin) {
  
  //Call recursive function build node
  root = buildNode(rows, trainingClasses, vs, K, NMin);

}

struct ScoredIndex {
  double score;
  int colIndex;
  double pivot;
  VarianceStat vs;
};

Node * DecisionTree::buildNode(const vector<const Row *> & rows, const vector<Classification> & trainingClasses, const VarianceStat &vs, const int K, const int NMin) {

  int n = rows.begin()[0]->size();


  if(sizeLE(rows, NMin)) return new Leaf(vs);
  
  vector<uint> randomSubset;
  generateRandomSubset(randomSubset, K, n);
#ifdef DEBUG
  cout << "RandomSubset:" << toString(randomSubset) << " \n";
#endif
  
  vector<ScoredIndex> scoredIndices;
  for(unsigned int i=0; i < randomSubset.size(); i++) {
    int colIndex = randomSubset[i];
    Attribute pivot = randomPivotValue(colAs<Attribute>(rows,colIndex));
    VarianceStat subVS;
    for(uint i = 0; i < rows.size(); i++) {
      if(rows[i]->at(colIndex) < pivot) {
	subVS += trainingClasses[i];
      }
    }

    ScoredIndex si;
    si.score = VarianceStat::relativeVarianceReduction(vs, subVS);
    si.colIndex= colIndex;
    si.pivot = pivot;
    si.vs = subVS;
#ifdef DEBUG
    cout << "scoring col[" << colIndex << "]: score: " << si.score << " pivot: " << si.pivot << "\n";
    cout << "variance: " << si.vs.str();
#endif
    scoredIndices.push_back(si);
    
  }
  
  // Do I need to make two passes, I can merge this with above
  ScoredIndex * best = NULL;
  for(unsigned int i = 0; i < scoredIndices.size(); i++ ) {
    if(best == NULL || scoredIndices[i].score > best->score) best = & (scoredIndices[i]);
  }
  
#ifdef DEBUG
  assert(best != NULL);
#endif
  
  
  if(best->score < 1e-20) {
    return new Leaf(vs);
  } 

  
  int bestColIndex = best->colIndex;
  double bestPivot = best->pivot;

  VarianceStat ltVS = best->vs;
  VarianceStat geVS = vs - ltVS;
  
  vector<const Row *> ltSamples;
  vector<Classification> ltClasses;
  vector<const Row *> geSamples;
  vector<Classification> geClasses;

  partitionSamples(rows, trainingClasses, & ltSamples, &ltClasses,& geSamples, &geClasses, bestColIndex, bestPivot);

  return new Split(bestColIndex, bestPivot, buildNode(ltSamples, ltClasses, ltVS, K, NMin), buildNode(geSamples, geClasses, geVS, K, NMin));
  
  
}

void DecisionTree::partitionSamples(const vector<const Row * > &rows, 
				    const vector<Classification> & trainingClasses,
				    vector<const Row *> * ltSamples,
				    vector<Classification> * ltClasses, 
				    vector<const Row *> * geSamples, 
				    vector<Classification> * geClasses, 
				    const int colIndex, 
				    const double pivot) {
  
  for( uint i = 0 ; i < rows.size(); i++ ) {
    if(rows[i]->at(colIndex) < pivot) {
      ltSamples->push_back(rows[i]);
      ltClasses->push_back(trainingClasses[i]);
    }
    else {
      geSamples->push_back(rows[i]);
      geClasses->push_back(trainingClasses[i]);
    }
  }
  
}


Attribute DecisionTree::randomPivotValue(const AttributeColumn & col) {

  Attribute min = numeric_limits<Attribute>::max();
  Attribute max = numeric_limits<Attribute>::min();

  for(uint i=0; i < col.size(); i++) {
    Attribute curCol = col[i];
    if(curCol < min) min = curCol;
    if(curCol > max) max = curCol;
  }

  if(min == max) {
    return min;
  } else {
    long r = lrand48_r(&randBuffer, &r);
    Attribute pivot = r % (max-min) + min + 1;
    return pivot;
  }
}

void DecisionTree::generateRandomSubset(vector<uint> & rs, const unsigned int K, const unsigned int n){
  
#ifdef DEBUG
  assert(K <= n);
#endif

  //TODO, Nathan gets fancy here, so I should copy his code in the future, for now 
  // implementing simplest method 
  set<int> hist;
  while(rs.size() < K) {
    int r = rand_r(&randSeed) % n;
    if(hist.find(r) == hist.end()) {
      hist.insert(r);
      rs.push_back(r);
    }
    
  }
  

}


const VarianceStat DecisionTree::classify(const Row &row) const {
  
  return root->classify(row);
  
}



// Nodes and such code

Split::Split(unsigned int colIndex, double pivot, Node * lt, Node * ge) {
  
  this->colIndex = colIndex;
  this->pivot = pivot;
  this->lt = lt;
  this->ge = ge;

#ifdef DEBUG
    cout << "Constructing new Split\n";
#endif

}
