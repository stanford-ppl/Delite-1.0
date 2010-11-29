#include <assert.h>
#include <stdlib.h>
#include <limits>
#include <set>
using namespace std;

#include "DecisionTree.h"
#include "Histogram.h"
#include "ERT.h"


extern unsigned int randSeed;
extern struct drand48_data randBuffer;

DecisionTree::DecisionTree(const vector<const vector<double> * > & rows, const Histogram histo, const int K, const int NMin) {
  
  //Call recursive function build node
  root = buildNode(rows, histo, K, NMin);

}

struct ScoredIndex {
  double score;
  int colIndex;
  double pivot;
  Histogram histo;
};

Node * DecisionTree::buildNode(const vector<const vector<double> *> & rows, const Histogram histo, const int K, const int NMin) {

  int n = rows.begin()[0]->size() - 1;
  vector<int> category = colAsInt(rows, n);

#ifdef DEBUG
  cout << "Classes:" << toString(category) << "\n";
#endif

  if(sizeLE(rows, NMin)) return new Leaf(histo);
  
  vector<int> randomSubset;
  generateRandomSubset(randomSubset, K, n);
#ifdef DEBUG
  cout << "RandomSubset:" << toString(randomSubset) << " \n";
#endif
  
  vector<ScoredIndex> scoredIndices;
  for(unsigned int i=0; i < randomSubset.size(); i++) {
    int colIndex = randomSubset[i];
    double pivot = randomPivotValue(colAsDouble(rows,colIndex));
    Histogram subHisto;
    for(vector<const vector<double> * >::const_iterator iter = rows.begin(); iter != rows.end(); iter++) {
      if( iter[0]->at(colIndex) < pivot ) {
	//you are adding the class column to the histogram
	unsigned int clz = (unsigned int)iter[0]->at(n);
	subHisto += clz;
      }
    }
  
#ifdef DEBUG
    cout << subHisto.str();
#endif
    ScoredIndex si;
    si.score = ERT::normalizedShanonInformationGain(histo, subHisto);
    si.colIndex= colIndex;
    si.pivot = pivot;
    si.histo = subHisto;
#ifdef DEBUG
    cout << "scoring col[" << colIndex << "]: score: " << si.score << " pivot: " << si.pivot << "\n";
    cout << "histo: " << si.histo.str();
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

  if(best->score < 1e-20) return new Leaf(histo);
  
  int bestColIndex = best->colIndex;
  double bestPivot = best->pivot;

  Histogram ltHist = best->histo;
  Histogram geHist = histo - ltHist;
  
  vector<const vector<double> *> ltSamples;
  vector<const vector<double> *> geSamples;

  partitionSamples(rows, & ltSamples, & geSamples, bestColIndex, bestPivot);

  return new Split(bestColIndex, bestPivot, buildNode(ltSamples, ltHist, K, NMin), buildNode(geSamples, geHist, K, NMin));
  
  
}

void DecisionTree::partitionSamples(const vector<const vector<double> * > &rows, vector<const vector<double> *> * ltSamples, vector<const vector<double> *> * geSamples, const int colIndex, const double pivot) {
  
  vector<const vector<double> *>::const_iterator iter = rows.begin();
  
  for( ; iter!=rows.end(); iter++) {
    if(iter[0]->at(colIndex) < pivot)
      ltSamples->push_back(*iter);
    else 
      geSamples->push_back(*iter);
  }
  
}


double DecisionTree::randomPivotValue(const vector<double>& col) {

  double min = numeric_limits<double>::max();
  double max = numeric_limits<double>::min();

  for(unsigned int i=0; i < col.size(); i++) {
    double curCol = col[i];
    if(curCol < min) min = curCol;
    if(curCol > max) max = curCol;
  }

  
  double r;
  drand48_r(&randBuffer, &r);
  return min + r* (max-min);
  
  
}

void DecisionTree::generateRandomSubset(vector<int> & rs, const unsigned int K, const unsigned int n){
  
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


vector<int> DecisionTree::colAsInt(const vector<const vector<double> * > & rows, int idx) {

  vector<int> col;
  
  for( vector<const vector<double> *>::const_iterator iter = rows.begin(); iter != rows.end(); iter++) {
    col.push_back((int)iter[0]->at(idx));
  }
  
  return col;

}

vector<double> DecisionTree::colAsDouble(const vector<const vector<double> * > & rows, int idx) {

  vector<double> col;
  
  for( vector<const vector<double> *>::const_iterator iter = rows.begin(); iter != rows.end(); iter++) {
    col.push_back(iter[0]->at(idx));
  }
  
  return col;

}

// TODO implement classify
const Histogram DecisionTree::classify(const vector<double> &row) const {
  
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
