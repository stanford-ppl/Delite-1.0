#ifndef ERT_H
#define ERT_H

#include <vector>

using namespace std;

#include "Argument_helper.h"
#include "Histogram.h"
#include "DecisionTree.h"

class ERT {
  
  vector<DecisionTree *> trees;
  
  vector<int> colAsInt(const vector< vector<double> > &, int);
  
  vector<double> colAsDouble(const vector< vector<double> > &, int);
  
  

 public:
  
  ERT(const vector< vector< double > > &, int);
  ~ERT();
  const int classify(const vector< double > &row);
  static double normalizedShanonInformationGain(const Histogram &, const Histogram &);

};




// Other declarations for main program, probably need to split this into multiple files
void processArguments(dsr::Argument_helper &ah, const int argc, const char* argv[], int &M, string &training_filename, string & input_filename);

const int getClassification(const vector<DecisionTree> &trees, const vector< double > &row);

void loadRowsToBeClassified(const string &, vector< vector<double> > *rows, vector<int> * classifications);

void loadTrainingRows(const string & , vector< vector<double> > *rows);


string toString(const vector<double> &v);
string toString(const vector<int> &v);
string toString(const Histogram &);





#endif
