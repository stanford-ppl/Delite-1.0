/*
 * Naive Bayes Example Application
 *
 * author:  Peter McMahon
 * created: February, March 2009
 *
 * modified by:   Arvind Sujeeth
 * last modified: 6/27/2009
 *
 */

package ppl.apps.ml.nb

import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml.io.MLInputReader
import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.appinclude._
import java.lang.Math.log
import ppl.delite.dsl.primitive.{DeliteInt, DeliteDouble}
import ppl.delite.core.{Config, Delite, DeliteCore, DeliteApplication}
import ppl.delite.metrics._
import ppl.delite.core.executor.DeliteGPUThreadPool

object NaiveBayes_GPU extends DeliteApplication {
  class Dataset( val numDocs: Int, val numTokens: Int, val features: Matrix[Double], val classifications: Vector[Double] ) {
    //val features: Matrix[Double] = in_features
    //val classifications: Vector[Double] = in_classifications
  }

  def print_usage = {
    println("NaiveBayes_GPU <training file> <test file>")
    exit(-1)
  }

  def run(args: Array[String]) {
    println("Naive Bayes Example Application")

    if (args.length < 2) print_usage

    Delite.init = true

    val trainingFile = args(0)
    val testFile = args(1)

    var traindata : Dataset = null
    var testdata : Dataset = null
    try{
      traindata = io_GPU.readData(trainingFile)
      testdata = io_GPU.readData(testFile) // read test set
    }catch{
      case e: Exception => e.printStackTrace
    }

    Delite.init = false

    val numTimes = 10

    for (i <- 0 until numTimes) {

      DeliteGPUThreadPool.threads(0).cleanup_done = false
      DeliteGPUThreadPool.threads(0).cleanup = true
      while(!DeliteGPUThreadPool.threads(0).cleanup_done) {}
      
      PerformanceTimer.start("NaiveBayesGPU")
      
		 // Train Model
      val (phi_y1, phi_y0, phi_y) = train(traindata)
      phi_y1.force
      phi_y0.force

      PerformanceTimer.stop("NaiveBayesGPU")
      PerformanceTimer.print("NaiveBayesGPU")
    
	  if(i==(numTimes-1)) {
	  	val incorrect_classifications = test(testdata, phi_y1, phi_y0, phi_y)
     	println("Test error: " + java.lang.String.valueOf(incorrect_classifications.doubleValue() / testdata.numDocs.doubleValue()))
	  }
    }
    
    PerformanceTimer.save("NaiveBayesGPU")
  }

  def train(ts: Dataset) : (Vector[Double], Vector[Double], Double) = {

    val numTrainDocs = ts.numDocs
    val numTokens = ts.numTokens

    //val words_per_email = ts.features.mapToVec(ele => ele.sum[DeliteDouble]).toDouble
    val words_per_email = ts.features.trans.sumCol

    println("Training model on " + numTrainDocs + " documents.")

    val weightedspamcount = Vector.range(0, numTrainDocs).map(i => if (ts.classifications(i) == 1) words_per_email(i) else 0.).sum[DeliteDouble]
    val spamcount = ts.classifications.sum[DeliteDouble]
	  val weightednonspamcount = Vector.range(0, numTrainDocs).map(i => if (ts.classifications(i) == 0) words_per_email(i) else 0.).sum[DeliteDouble]
    //weightedspamcount.force
    //weightednonspamcount.force
    
    var phi_y1 = Vector.range(0, numTokens).mapNB(j => {
      var spamwordcount = 0.0
      for (i <- 0 until numTrainDocs)
        {
          if (ts.classifications(i) == 1)
            {
              spamwordcount = spamwordcount + ts.features(i, j)
            }
        }
      (spamwordcount + 1) / (weightedspamcount + numTokens)
    })(numTokens, numTrainDocs, 1)(ts.classifications, ts.features, weightedspamcount)

    var phi_y0 = Vector.range(0, numTokens).mapNB( j => {
      var nonspamwordcount = 0.0
      for (i <- 0 until numTrainDocs)
        {
          if (ts.classifications(i) == 0)
            {
              nonspamwordcount = nonspamwordcount + ts.features(i, j)
            }
        }
      (nonspamwordcount + 1) / (weightednonspamcount + numTokens)
    })(numTokens, numTrainDocs, 0)(ts.classifications, ts.features, weightednonspamcount)

    // Above two map operations can be replaced with below codes not to use special map operations
    /*
    val zeros = Vector[Double](numTrainDocs)
    val ones = Vector.range(0, numTrainDocs).map(ele => 1.0)
    val predicates_y1 = ts.classifications.equal(ones)
    val predicates_y0 = ts.classifications.equal(zeros)
    var phi_y1 = ts.features.sumColPred(predicates_y1)
    var phi_y0 = ts.features.sumColPred(predicates_y0)
    phi_y1 = (phi_y1 + 1.0) / (weightedspamcount+numTokens)
    phi_y0 = (phi_y0 + 1.0) / (weightednonspamcount+numTokens)
    */

    val phi_y = spamcount / numTrainDocs

    (phi_y1, phi_y0, phi_y)
  }

  def test(testdata : Dataset, phi_y1 : Vector[Double], phi_y0 : Vector[Double], phi_y : Double) : Int = {
    //val numTestDocs = testdata.classifications.length;
    //val numTokens = testdata.features.w; // width of matrix
    val numTestDocs = testdata.numDocs
    val numTokens = testdata.numTokens

    println("Testing model on " + numTestDocs + " documents.")

    //var output = Vector.zeros(numTestDocs)
    var output = Vector.range(0, numTestDocs).map(j => {
      //for (j <- 0 until numTestDocs)
      //{
      // compute log(p(x|y=1)p(y=1)) and log(p(x|y=0)p(y=0))
      var p_norm = 0.0
      var p_spam = 0.0
      for (i <- 0 until numTokens)
        {
          if (testdata.features(j, i) > 0)
            {
              p_norm = p_norm + (log(phi_y0(i)) + log(1-phi_y)) * testdata.features(j,i)
              p_spam = p_spam + (log(phi_y1(i)) + log(phi_y)) * testdata.features(j,i)
            }
        }

      if (p_spam > p_norm)
        {
          //output(j) = 1
          1.
        }
      else{
        //output(j) = 0
        0.
      }
    })//, true)
    //}

    // Compute error on test set
    //HACK HACK HACK
    testdata.classifications.force
    output.force
    //HACK HACK HACK
    var incorrect_classifications = 0
    for (i <- 0 until numTestDocs){
      if (testdata.classifications(i) != output(i))
        incorrect_classifications += 1
    }

    incorrect_classifications
  }
}

object io_GPU {
  import ppl.apps.ml.nb.NaiveBayes_GPU.Dataset

  def readData(filename: String): Dataset = {
    // parse the input matrix into the following elements:
    //  inMatrix:   a (numDocs x numTokens) matrix, where each row represents a unique document
    //                 the jth column of row i represents the number of times the jth token appeared in doc i
    //  tokenlist:  a long string containing the list of all tokens (words)
    //  inCategory: a (numDocs x 1) vector containing the true classifications for the documents just read
    //                 the ith entry gives the correct class for the ith email, where spam is 1 and non-spam is 0.

    DeliteCore._GLOBAL_DEFER += 1
    val (inMatrix, tokenlist, inCategory) = MLInputReader.readTokenMatrix(filename)
    val (numDocs, numTokens) = MLInputReader.getTokenMatrixStats(filename)
    val ds = new Dataset(numDocs, numTokens, inMatrix.map(v => v.toDouble), inCategory.map(e => e.toDouble))
    DeliteCore._GLOBAL_DEFER -= 1
    ds
  }

  // reads training/test data in sparse matrix format
  //  def readData(filename: String): Dataset = {
  //    try {
  //      val in = new BufferedReader(new FileReader(filename))
  //      in.readLine() // file title
  //      val tmp = in.readLine() // string "X Y": X is number of documents, Y is number of words
  //      val numDocs = java.lang.Integer.parseInt(tmp.split(" ")(0))
  //      val numWords = java.lang.Integer.parseInt(tmp.split(" ")(1))
  //      val words = in.readLine().split(" ")
  //      var classifications = Vector.zeros(0)
  //      var matrixRows: List[Vector[Double]] = List()
  //      var line: String = in.readLine()
  //      var i = 0
  //      while (line != null)
  //      {
  //        val doc = line.split(" ") map {s => java.lang.Integer.parseInt(s)}
  //        classifications = Vector(true, classifications.toSeq.toList ::: List(doc(0).doubleValue()) : _*)
  //        var j = 1
  //        var currentColumn = 0;
  //        var matrixRow = Array.range(0, numWords).map(ele => 0.); // create a row with numWords zeros
  //        while (j < doc.length - 1)
  //        {
  //          currentColumn = currentColumn + doc(j);
  //          matrixRow(currentColumn) = doc(j+1).doubleValue();
  //          j += 2;
  //        }
  //        matrixRows = matrixRows ::: List(Vector(true, List.fromArray(matrixRow) map {s => s.doubleValue()} : _*))
  //        line = in.readLine()
  //        i = i + 1
  //      }
  //      in.close()
  //      val features = Matrix( matrixRows: _* )
  //
  //      val ds = new Dataset(features, classifications)
  //      ds // return
  //    }
  //    catch
  //    {
  //      case ex: Exception => println("File IO error."); null
  //    }
  //  }
}
