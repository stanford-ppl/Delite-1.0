#include <iostream>
#include <vector>
#include <sstream>
#include <fstream>
#include <math.h>
#include <stdlib.h>
#include <omp.h>

#ifdef DEBUG
#include <assert.h>
#endif

using namespace std;

#include "ERT.h"
#include "Argument_helper.h"
#include "Histogram.h"

//Globals
//This is only for single threaded case, multi threaded needs a buffer
//for each thread
struct drand48_data randBuffer;
unsigned int randSeed;


int main(int argc, const char* argv[]) {
  
  
  //Seeding Random number generator
  srand48_r(0,&randBuffer);
  
  // process arguments
  dsr::Argument_helper ah;
  string training_filename;
  string input_filename;
  int M ; //number of trees to randomly generate
  processArguments(ah, argc, argv, M, training_filename, input_filename);
  
  std::cout << "ERT is starting with threads=" << omp_get_max_threads() <<", hold on!\n";

  // Load training data
  vector< vector<double> > trainingRows;
  loadTrainingRows(training_filename, &trainingRows);  
  // Load input data
  vector< vector<double> > inputRows;
  vector<int> trueClasses;
  loadRowsToBeClassified(input_filename, &inputRows, &trueClasses);

  
  // Training Phase: construct your trees
  std::cout << "--- Training Phase ---\n";
  // Generate Trees
  ERT ert(trainingRows, M);
  
  // Classification phase
  std::cout << "--- Classification Phase ---\n";  
  
  //iterate through rows and classify
  vector<int> guessClasses;
  vector< vector<double> >::iterator iter;
  for(iter = inputRows.begin(); iter != inputRows.end(); iter++) {
    int classification = ert.classify(*iter);
    guessClasses.push_back(classification);
#ifdef SHOWCLASSDETAILS
    cout << "Processing row: " << toString(*iter) << "\n";
    cout << "Resulting Classification: " << classification << "\n";
#endif
  }

  unsigned int correctClasses = 0;
#ifdef DEBUG
  assert(guessClasses.size() == trueClasses.size());
#endif
  for(unsigned int i = 0; i < guessClasses.size(); i++) {
    if(guessClasses[i] == trueClasses[i]) {
      correctClasses++;
    }
#ifdef SHOWCLASSDETAILS 
    else {

      cout << "On line: " << i << " of input, guessed: " << guessClasses[i] << " instead of " << trueClasses[i] << endl;
    }
#endif
  }
  double successRate = (double)correctClasses / guessClasses.size();
  cout << "Overall Sucess Rate: " << successRate * 100 << "%" << endl;
}


void processArguments(dsr::Argument_helper &ah, const int argc, const char* argv[], int &M, string &training_filename, string & input_filename) {
#ifdef DEBUG
  std::cout << "--- processing command line arguments ---\n";
#endif
  M = 0;
  int numThreads = 1;
  ah.new_named_int('M', "M","M", "Number of random trees to generate",M);
  ah.new_named_int('T', "T","T", "Number threads to use",numThreads);
  ah.new_string("training_file", "filename that contains that training matrix values", training_filename);
  ah.new_string("input_file", "filename that contains rows you would like to be classified", input_filename);
  ah.set_author("Hassan Chafi");
  ah.set_description("Implements Extra Randomized Tree Algoritm");
  ah.set_version(0.01);
  ah.set_build_date("");
  ah.process(argc, argv);

  //Default M if still zero
  if(M == 0) {
    cout << "No acceptable value for M specified, using default M=100\n";
    M = 100;
  }

  omp_set_num_threads(numThreads);
  
}


void loadTrainingRows(const string & filename, vector< vector<double> > * rows) {
  
  ifstream training (filename.c_str());
  string line;
  if(training.is_open()) {
    //load lines
    getline(training, line);
    while(training.eof() == false) {
      vector<double> v;
      size_t spos = line.find(',');
      size_t opos = 0;
      while(spos != string::npos) {
	//extract attr and look for next
	string attrStr = line.substr(opos, spos - opos);
	double attr = atof(attrStr.c_str());
	v.push_back(attr);
	opos = spos + 1;
	spos = line.find(',', opos);
      }
      // now find class
      spos = line.find('.', opos);
      string clzStr = line.substr(opos, spos-opos);
      double clz = atof(clzStr.c_str());
      if(clz == 0) {
	cout << "Bad Value: " << clz << "str: " << clzStr << endl; 
	cout << "attrs: " << toString(v) << endl;
	cout << "line: " << line << endl;
	
      }
      v.push_back(clz);
      rows->push_back(v);
      // Get next line
      getline(training, line);
    }
    
  }
  training.close();
}

void loadRowsToBeClassified(const string & filename, vector< vector<double> > *rows, vector<int>  * classifications) {
  
  ifstream input(filename.c_str());
  string line;
  if(input.is_open()) {
    //load lines
    getline(input, line);
    while(input.eof() == false) {
      
      vector<double> v;
      size_t spos = line.find(',');
      size_t opos = 0;
      while(spos != string::npos) {
	//extract attr and look for next
	string attrStr = line.substr(opos, spos - opos);
	double attr = atof(attrStr.c_str());
	v.push_back(attr);
	opos = spos + 1;
	spos = line.find(',', opos);
      }
      // now find class
      spos = line.find('.', opos);
      string clzStr = line.substr(opos, spos-opos);
      unsigned int clz = atoi(clzStr.c_str());
      classifications->push_back(clz);
      rows->push_back(v);
      //process next line
      getline(input, line);
    }
    
  }
  input.close();

}


string toString(const vector<double> &v) {

  stringstream ret;
  
  ret << "[ ";
  for(vector<double>::const_iterator iter = v.begin(); iter != v.end(); iter++) {
    
    ret << *iter;
    if(iter+1 != v.end())
      ret << ", ";
  }
  ret << " ]";
  return ret.str();
}


string toString(const vector<int> &v) {

  stringstream ret;
  
  ret << "[ ";
  for(vector<int>::const_iterator iter = v.begin(); iter != v.end(); iter++) {
    
    ret << *iter;
    if(iter+1 != v.end())
      ret << ", ";
  }
  ret << " ]";
  return ret.str();
}




// ERT Class


ERT::ERT(const vector<vector<double> > & trainingRows, int M) {
  
#ifdef DEBUG
  cout << "ERT is being constructed\n";
#endif
  int n = (trainingRows.begin())->size() - 1; // number of attributes
  int K = (int)floor(sqrt(n)+0.5);
  int NMin = 2;
  
#ifdef DEBUG
  cout << "M: " << M << " n: " << n << " K: " << K << " NMin: " << NMin << " \n";
#endif

  Histogram topHisto(colAsInt(trainingRows, n));
  //cout << toString(colAsInt(trainingRows, n));
  //cout << topHisto.str();
  
  //switch to using pointers to enhance performance
  vector<const vector<double> *> rowPtrs;
  rowPtrs.reserve(trainingRows.size());
  for(unsigned int i = 0; i < trainingRows.size(); i++) {
    const vector<double> * ptr = &(trainingRows[i]);
    rowPtrs.push_back(ptr);
  }
  
  trees.resize(M);
#pragma omp parallel for shared(M) private(i)
  for(int i = 0; i < M; i++) {
#ifdef DEBUG
    cout << "constructing Decision Tree[" << i << "] \n";
#endif
    cout << "Thread[" << omp_get_thread_num() << "] is constructing tree["<<i<<"]"<<endl;
    trees[i] = new DecisionTree(rowPtrs, topHisto, K, NMin);
  }

}

ERT::~ERT() {
  
  for(unsigned int i = 0; i < trees.size(); i++) {
    delete trees[i];
  }
    
}


  
 



vector<int> ERT::colAsInt(const vector< vector<double> > & rows, int idx) {
  
  vector<int> col;
  
  for( vector<vector<double> >::const_iterator iter = rows.begin(); iter != rows.end(); iter++) {
      col.push_back((int)(*iter)[idx]);
  }
  
  return col;
  
}

vector<double> ERT::colAsDouble(const vector< vector<double> > & rows , int idx) {
  
  vector<double> col;
  
  for( vector<vector<double> >::const_iterator iter = rows.begin(); iter != rows.end(); iter++) {
    col.push_back((*iter)[idx]);
  }
  
  return col;

  
}

const int ERT::classify(const vector< double > &row) {
  
  Histogram histo;

  //cout << "processing row: " << toString(row) << endl;
  for ( vector<DecisionTree *>::const_iterator iter = this->trees.begin(); iter != this->trees.end(); iter++) {
    histo += (*iter)->classify(row);
    //cout << "after one tree, resulting histo: " << histo.str() << endl;
  }
  
  list<unsigned int> ties = histo.mostCommon();
  //get median value in case of ties
  list<unsigned int>::const_iterator iter = ties.begin();
  int index = ties.size()/2;
  while( index-- > 0) {
    iter++;
  }
  const int classification = *(iter);
  return classification;
  
  
}


double ERT::normalizedShanonInformationGain(const Histogram &parent , const Histogram & left) {
  
#ifdef DEBUG
  cout<< "Calculating Shanon Information Gain:\n";
#endif
  
  unsigned int rightSC = parent.getSampleCount() - left.getSampleCount();
  if(left.getSampleCount() == 0 || rightSC ==0) {
#ifdef DEBUG
    cout << "No information gain, returning 0.0\n";
#endif
    return 0.0;
  }

#ifdef DEBUG
  cout << "constructing binary histo s_hist\n"; 
#endif

  Histogram s_hist = Histogram::binaryHisto(left.getSampleCount(), parent.getSampleCount() - left.getSampleCount());

#ifdef DEBUG
  cout << "s_hist:" << s_hist.str();
#endif

  Histogram right = parent - left;
#ifdef DEBUG
  cout << "right:" << right.str();
#endif
  double p_left  = left.getSampleCount() * 1.0 / parent.getSampleCount();
  double p_right = 1.0 - p_left;
  
  double h_c = parent.shanonEntropy();
  double h_s = s_hist.shanonEntropy();
  double h_c_given_s = p_left * left.shanonEntropy() + p_right * right.shanonEntropy();
  double i_c_s = h_c - h_c_given_s;
  
  return 2 * i_c_s / (h_c + h_s);
  
}

