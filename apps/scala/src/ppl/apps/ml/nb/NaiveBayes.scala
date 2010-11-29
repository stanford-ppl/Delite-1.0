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
import ppl.delite.metrics._
import ppl.delite.core.{DeliteFunc, Delite, DeliteCore, DeliteApplication}

object NaiveBayes extends DeliteApplication {
  class Dataset(val numDocs: Int, val numTokens: Int, val features: Matrix[Double], val classifications: Vector[Double]){
    val featuresT = features.trans
  }

  def print_usage = {
    println("NaiveBayes <training file> <test file>")
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
      traindata = io.readData(trainingFile)
      testdata = io.readData(testFile) // read test set
    }catch{
      //case e: Exception => print_usage
      case e: Exception => e.printStackTrace
    }

    Delite.init = false
    val numTimes = 10
    for (i <- 0 until numTimes) {
      PerformanceTimer.start("NaiveBayes")

      // Train Model
      //val start_train = System.currentTimeMillis()
      val (phi_y1, phi_y0, phi_y) = train(traindata)
      phi_y1.force
      phi_y0.force
      PerformanceTimer.stop("NaiveBayes")
      PerformanceTimer.print("NaiveBayes")

      //println("phi_y1: " + phi_y1.pprint + " | phi_y0: " + phi_y0.pprint + " | phi_y: " + phi_y)
      val incorrect_classifications = test(testdata, phi_y1, phi_y0, phi_y)
      println("Test error: " + java.lang.String.valueOf(incorrect_classifications.doubleValue() / testdata.numDocs.doubleValue()))
    }

    PerformanceTimer.save("NaiveBayes")

  }

  def train(ts: Dataset) : (Vector[Double], Vector[Double], Double) = {
    val numTrainDocs = ts.numDocs
    val numTokens = ts.numTokens

    val words_per_email = (0::ts.features.numRows){ i => ts.features(i).sum[DeliteDouble] }

    println("Training model on " + numTrainDocs + " documents.")

    val spamcount = ts.classifications.sum[DeliteDouble]

    val phi_y1 = Vector.zeros(numTokens)
    val phi_y0 = Vector.zeros(numTokens)

    (0::numTokens).mforeach{ j => {
      var spamwordcount = 0.0
      var spam_totalwords = 0.0
      var nonspamwordcount = 0.0
      var nonspam_totalwords = 0.0

      for (i <- 0 until numTrainDocs){
        if (ts.classifications(i) == 1){
          spamwordcount += ts.featuresT(j,i)
          spam_totalwords += words_per_email(i)
        }
        else{
          nonspamwordcount += ts.featuresT(j,i)
          nonspam_totalwords += words_per_email(i)
        }
      }
      phi_y1(j) = (spamwordcount + 1) / (spam_totalwords + numTokens)
      phi_y0(j) = (nonspamwordcount + 1) / (nonspam_totalwords + numTokens)
      // TODO: fix plugin issue that causes deadlock when registering the mutable dep
    }}().force
    
    // TODO: this is the best syntax, but to work efficiently, we need tuple sum
    /*val phi_y1 = (0::numTokens){ j =>
      var spamwordcount = 0.0
      var numWords = 0.0
      for (i <- 0 until numTrainDocs){
        if (ts.classifications(i) == 1){
          spamwordcount += ts.features(i, j)
          numWords += words_per_email(i)
        }        
      }
      (spamwordcount + 1) / (numWords + numTokens)
	  }

    val phi_y0 = (0::numTokens){ j =>
      var nonspamwordcount = 0.0
      var numWords = 0.0
      for (i <- 0 until numTrainDocs){
        if (ts.classifications(i) == 0){
          nonspamwordcount += ts.features(i, j)
          numWords += words_per_email(i)
        }
      }
      (nonspamwordcount + 1) / (numWords + numTokens)
	  }*/
	   

// TODO: use compiler plugin to automatically insert j =>, and replace any $ inside the closure with j

//     val phi_y0 = (0::numTokens) {
//      var nonspamwordcount = 0.0
//      for (i <- 0 until numTrainDocs){
//        if (ts.classifications(i) == 0){
//          nonspamwordcount += ts.featuresT($, i)
//        }
//      }
//      (nonspamwordcount + 1) / (weightednonspamcount + numTokens)    
//    }

    val phi_y = spamcount / numTrainDocs

    (phi_y1, phi_y0, phi_y)
  }

  def test(testdata : Dataset, phi_y1 : Vector[Double], phi_y0 : Vector[Double], phi_y : Double) : Int = {
    val numTestDocs = testdata.numDocs
    val numTokens = testdata.numTokens

    println("Testing model on " + numTestDocs + " documents.")

    var output = (0::numTestDocs){j => {
      // compute log(p(x|y=1)p(y=1)) and log(p(x|y=0)p(y=0))
      var p_norm = 0.0
      var p_spam = 0.0
      for (i <- 0 until numTokens){
        if (testdata.features(j,i) > 0){
          p_norm += (log(phi_y0(i)) + log(1-phi_y)) * testdata.features(j,i)
          p_spam += (log(phi_y1(i)) + log(phi_y)) * testdata.features(j,i)
        }
      }

      if (p_spam > p_norm){
        1.
      }
      else{
        0.
      }
    }}

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

object io {
  import ppl.apps.ml.nb.NaiveBayes.Dataset

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
    val ds = new Dataset(numDocs, numTokens, inMatrix.toDouble, inCategory.toDouble)
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
