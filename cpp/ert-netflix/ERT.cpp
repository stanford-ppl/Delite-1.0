#include <ext/stdio_filebuf.h>
#include <iostream>
#include <vector>
#include <sstream>
#include <fstream>
#include <istream>
#include <math.h>
#include <stdlib.h>

#ifdef DEBUG
#include <assert.h>
#endif

using namespace std;

#include "ERT.h"
#include "Argument_helper.h"
#include "types.h"

//Globals
//This is only for single threaded case, multi threaded needs a buffer
//for each thread
struct drand48_data randBuffer;
unsigned int randSeed;



//DEFAULTS
#define DEFAULT_M 100
#define DEFAULT_T 90000
#define DEFAULT_C 10000
#define DEFAULT_S 0

int main(int argc, const char* argv[]) {
  
    
  microElapsed();
  
  
  
  cout << microElapsed() << ": " << "ERT is starting, hold on!\n";

  
  //Seeding Random number generator
  srand48_r(0,&randBuffer);
  
  // process arguments
  dsr::Argument_helper ah;
  string netflix_dir;
  int M ; //number of trees to randomly generate
  int T ; //number of rows to use for training
  int C ; //number of rows to use for classification
  int S ; //number of minimum ratings allowed per used row in either training or classification
  int movie_id; //the movie id that the ERT algorithm should target as classifier
  processArguments(ah, argc, argv, M, T, C, S, netflix_dir, movie_id);
  

  // Load training and classification data
  vector< Row > trainingRows;
  vector<Classification> trainingClasses;
  vector< Row > inputRows;
  vector<Classification> inputClasses;
  loadRows(netflix_dir, T, C, S, movie_id, &trainingRows, &trainingClasses, &inputRows, &inputClasses);

  
  // Training Phase: construct your trees
  cout << microElapsed() << ": " << "--- Training Phase ---\n";
  // Generate Trees
  ERT ert(trainingRows, trainingClasses, M);
  
  // Classification phase
  cout << microElapsed() << ": " << "--- Classification Phase ---\n";  
  
  //iterate through rows and classify
  vector<Classification> guessClasses;
  vector< Row >::iterator iter;
  for(iter = inputRows.begin(); iter != inputRows.end(); iter++) {
    Classification classification = ert.classify(*iter);
    guessClasses.push_back(classification);
#ifdef SHOWCLASSDETAILS
    cout << microElapsed() << ": " << "Processing row: " << toString(*iter) << "\n";
    cout << microElapsed() << ": " << "Resulting Classification: " << classification << "\n";
#endif
  }
  
  measureSuccess(guessClasses, inputClasses);
  
  
}

void measureSuccess(const vector<Classification> & guessClasses, const vector<Classification> & inputClasses) {
  uint correctClasses = 0;
  double sumSquaredValues = 0;
  uint numValues = inputClasses.size();
  uint ones = 0;
  uint twos = 0;
  uint threes = 0;
  uint fours = 0;
  uint fives = 0;
  
  
#ifdef DEBUG
  assert(guessClasses.size() == inputClasses.size());
#endif
  for(unsigned int i = 0; i < guessClasses.size(); i++) {
    if(round(guessClasses[i]) == inputClasses[i]) {
      correctClasses++;
    }
    #ifdef SHOWCLASSDETAILS 
    else {
      cout << "On line: " << i << " of input, guessed: " << round(guessClasses[i]) << " instead of " << inputClasses[i] << endl;
    }
    #endif
    double delta = inputClasses[i] - guessClasses[i];
    sumSquaredValues += delta*delta;
    switch((int)inputClasses[i]) {
    case 1:
      ones++;
      break;
    case 2:
      twos++;
      break;
    case 3:
      threes++;
      break;
    case 4:
      fours++;
      break;
    case 5:
      fives++;
      break;
#ifdef DEBUG
    default:
      assert(0);
#endif
      
    }

  }
  double successRate = (double)correctClasses / guessClasses.size();
  cout << microElapsed() << ": " << endl;
  cout << "------------------------------------------------------\n"
       << "Distribution of Values\n"
       << "------------------------------------------------------\n"
       << "1: " << ((double)ones)/numValues * 100 << "%\n"
       << "2: " << ((double)twos)/numValues * 100 << "%\n"
       << "3: " << ((double)threes)/numValues * 100 << "%\n"
       << "4: " << ((double)fours)/numValues * 100 << "%\n"
       << "5: " << ((double)fives)/numValues * 100 << "%\n"
       << "------------------------------------------------------\n";
  cout << "Overall Success Rate: " << successRate * 100 << "%" << endl;
  cout << "rmse: " << sqrt(sumSquaredValues/numValues) << endl;

    
}


void processArguments(dsr::Argument_helper &ah, const int argc, const char* argv[], int &M, int &T, int &C, int &S, string &netflix_directory, int &movie_id) {
#ifdef DEBUG
  std::cout << "--- processing command line arguments ---\n";
#endif
  // Setting parameters to zero so that we can detect if they are missing from command line
  M = T = C = S = 0;
  ah.new_named_int('M', "M","M", "Number of random trees to generate",M);
  ah.new_named_int('T', "T","T", "Number of rows to use for training",T);
  ah.new_named_int('C', "C","C", "Number of rows to use for classification",C);
  ah.new_named_int('S', "S","S", "Drop rows with less than S ratings",S);
  ah.new_string("netflix_directory", "Directory that contains the netflix movie rating data", netflix_directory);
  ah.new_int("movie_id", "Movie id that you would like to run the ERT algorithm on", movie_id);
  ah.set_description("Implements Extra Randomized Tree Algoritm and use NETFLIX prize input");
  ah.set_version(0.01);
  ah.set_build_date("");
  ah.process(argc, argv);

  //Default if some input parameters have been ommitted
  if(M == 0) {
    cout << "No acceptable value for M specified, using default M=" <<  DEFAULT_M << endl;
    M = DEFAULT_M;
  }
  
  if(T == 0) {
    cout << "No acceptable value for T specified, using default T=" << DEFAULT_T << endl;
    T = DEFAULT_T;
  }

  if(C == 0) {
    cout << "No acceptable value for C specified, using default C=" << DEFAULT_C << endl;
    C = DEFAULT_C;
  }

  if(S == 0) {
    cout << "No acceptable value for S specified, using default S=" << DEFAULT_S << endl; 
    S = DEFAULT_S;
  }
  
}

void loadRows(const string & dir, 
	      int numTrainingRows, 
	      int numInputRows, 
	      int S, 
	      int movie_id,
	      vector< Row > *trainingRows, vector<Classification>  * trainingClasses, 
	      vector< Row > *inputRows, vector<Classification>  * inputClasses) {

  //this is all hardcoded for netflix input, and needs to be rewritten for other input sources
  int file_id = 1;
  const int last_id = 49;
  
  /// Loop across all files in the director
  while(file_id <= last_id && (numTrainingRows != 0 || numInputRows != 0)) {
    string file_id_str;
    
    if(file_id > 9) {
      file_id_str = "00" + to_string<int>(file_id);
    }
    else {
      file_id_str = "000" + to_string<int>(file_id);
    }
    
    string cmd = "zcat " + dir + "/customer_data_"+ file_id_str + ".txt.gz";
    cout << microElapsed() << ": " << "loading file:" + cmd << endl << flush;
    FILE * fp = popen(cmd.c_str(), "r");
    __gnu_cxx::stdio_filebuf<char> fb(fp, ios::in) ;
    istream input(&fb);
    ///
    
    cout << microElapsed() << ": " << "Total lines left to load: " << numTrainingRows + numInputRows << endl;

    string line;
    //load lines
    getline(input, line);
    
    //handle training data
    while(input.eof() == false && numTrainingRows != 0) {
      
      Row r;
      Classification c;
      if(processLine(line, movie_id, S,&r, &c)) {
	trainingRows->push_back(r);
	trainingClasses->push_back(c);
	numTrainingRows--;
      }
      getline(input, line);
    }
    
    //handle input data
    while(input.eof() == false && numInputRows != 0) {
      
      Row r;
      Classification c;
      if(processLine(line, movie_id,S, &r, &c)) {
	inputRows->push_back(r);
	inputClasses->push_back(c);
	numInputRows--;
      }
      //process next line
      getline(input, line);
    }
    
    ///
    pclose(fp);
    file_id++;
  }

}

bool processLine(string line, int movie_id, int S, Row *r, Classification * c){
  
  size_t spos = line.find(',');
  //dicarding cutomer id
  size_t opos = spos+1;
  spos = line.find(',', opos);
  int found = 1;
  int rated = 0; 
  while(spos != string::npos) {
    //extract attr and look for next
    string token = line.substr(opos, spos - opos);
    if(found == movie_id) {
      //found movie id col
      Classification classification = atof(token.c_str());
      if(classification < 1) {
	//discard this row, since target movie is not rated
	return false;
      }
      // found class
      *c = classification;
    } else {
      Attribute attr = atoi(token.c_str());
      if(attr > 0) 
	rated++;
      r->push_back(attr);
    }
    opos = spos + 1;
    spos = line.find(',', opos);
    found++;
  }
  
  if(rated >= S)
    return true;
  else
    return false;
  
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


ERT::ERT(const vector<Row> & trainingRows, const vector<Classification> & trainingClasses,int M) {
  
#ifdef DEBUG
  cout << "ERT is being constructed\n";
#endif
  int n = (trainingRows.begin())->size(); // number of attributes
  int K = (int)floor(sqrt(n)+0.5);
  int NMin = 2;
  
#ifdef DEBUG
  cout << "M: " << M << " n: " << n << " K: " << K << " NMin: " << NMin << " \n";
#endif

  VarianceStat topVS(trainingClasses);
  
  
  vector<const Row *> rowPtrs;
  rowPtrs.reserve(trainingRows.size());
  for(uint i = 0; i < trainingRows.size(); i++) {
    const Row * ptr = &(trainingRows[i]);
    rowPtrs.push_back(ptr);
  }
  
  trees.resize(M);
  for(int i = 0; i < M; i++) {
#ifdef DEBUG
    cout << "constructing Decision Tree[" << i << "] \n";
#endif
    trees[i] = new DecisionTree(rowPtrs,trainingClasses, topVS, K, NMin);
    cout << "." << flush;
  }
  cout << endl;

}

ERT::~ERT() {
  
  for(unsigned int i = 0; i < trees.size(); i++) {
    delete trees[i];
  }
    
}


  

const Classification ERT::classify(const vector< Attribute > &row) {
  
  VarianceStat vs;

  //cout << "processing row: " << toString(row) << endl;
  for ( vector<DecisionTree *>::const_iterator iter = this->trees.begin(); iter != this->trees.end(); iter++) {
    vs += (*iter)->classify(row);
    //cout << "after one tree, resulting histo: " << histo.str() << endl;
  }
  
  const Classification classification = vs.getMean();
  return classification;
  
  
}


uint64_t g_baseMicro;

uint64_t microTime() {
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return ((uint64_t)tv.tv_sec) * 1000000 + tv.tv_usec;
}

uint64_t microElapsed() {
  uint64_t now = microTime();
  if (g_baseMicro == 0) {
    g_baseMicro = now;
  }
  return now - g_baseMicro;
}
