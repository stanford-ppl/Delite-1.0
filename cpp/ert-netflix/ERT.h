#ifndef __ERT_H
#define __ERT_H

#include <vector>
#include <sstream>
#include <sys/time.h>
#include <time.h>


using namespace std;

#include "Argument_helper.h"
#include "DecisionTree.h"
#include "types.h"


class ERT {
  
  vector<DecisionTree *> trees;
  
 public:
  
  ERT(const vector< Row > &,const vector<Classification> &,  int);
  ~ERT();
  const Classification classify(const vector< Attribute > &row);

};




// Other declarations for main program, probably need to split this into multiple files
void processArguments(dsr::Argument_helper &ah, const int argc, const char* argv[], int &M, int &T, int &C, int &S, string & netflix_dir, int & movie_id);

const int getClassification(const vector<DecisionTree> &trees, const vector< double > &row);

void loadRows(const string & dir, int num_rows, 
	      int offset, int S, int movie_id,
	      vector< Row  > * , vector<Classification> *, 
	      vector< Row  > * rows, vector<Classification> *);

bool processLine(string line, int movie_id, int S, Row *v, Classification * c);

void measureSuccess(const vector<Classification> & guess, const vector<Classification> & input);


string toString(const vector<double> &v);
string toString(const vector<int> &v);

uint64_t microTime();
uint64_t microElapsed();
extern uint64_t g_baseMicro;

// Templatized utility functions

template <class T>
inline std::string to_string (const T& t)
{
  std::stringstream ss;
  ss << t;
  return ss.str();
};

template <class T>
inline vector<T> colAs (const vector<Row> & rows, int idx) {
  vector<T> col;
  
  for( vector<Row>::const_iterator iter = rows.begin(); iter != rows.end(); iter++) {
    col.push_back((T)(*iter)[idx]);
  }
  
  return col;
}

template <class T>
inline vector<T> colAs (const vector<const Row *> & rows, int idx) {
  vector<T> col;
  
  for( vector<const Row *>::const_iterator iter = rows.begin(); iter != rows.end(); iter++) {
    col.push_back((T)iter[0]->at(idx));
  }
  
  return col;
}


#endif
