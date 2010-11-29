package ppl.apps.ml.baseline.nb

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

import ppl.apps.ml.baseline.collection.{MLInputReader, DoubleMatrix, DoubleVector}
import ppl.delite.metrics.PerformanceTimer

object NaiveBayes {

  private class Dataset( val numDocs: Int, val numTokens: Int, val features: DoubleMatrix, val classifications: DoubleVector ) {
    val featuresT = features.trans
  }

  def print_usage = {
    println("NaiveBayes <training file> <test file>")
    exit(-1)
  }

  def main(args: Array[String]) {
    println("Naive Bayes Example Application")

    if (args.length < 2) print_usage

    val trainingFile = args(0)
    val testFile = args(1)

    var traindata : Dataset = null
    var testdata : Dataset = null

    try{
      traindata = io.readData(trainingFile)
      testdata = io.readData(testFile) // read test set
    }catch{
        case e: Exception => e.printStackTrace
   }

    // Train Model
    val numTimes = 10
    for (i <- 0 until numTimes) {
      PerformanceTimer.start("NaiveBayesbaseline")
      val (phi_y1, phi_y0, phi_y) = train(traindata)
      PerformanceTimer.stop("NaiveBayesbaseline")
      PerformanceTimer.print("NaiveBayesbaseline")

      val incorrect_classifications = test(testdata, phi_y1, phi_y0, phi_y)
      println("Test error: " + (incorrect_classifications.toDouble / testdata.numDocs.toDouble))
    }

    PerformanceTimer.save("NaiveBayesbaseline")

  }

  def train(ts: Dataset) : (DoubleVector, DoubleVector, Double) = {

    val numTrainDocs = ts.numDocs
    val numTokens = ts.numTokens

    val words_per_email = ts.features.mapToVec(ele => ele.sum)

    println("Training model on " + numTrainDocs + " documents.")

    val spamcount = ts.classifications.sum

    val phi_y1 = DoubleVector(numTokens)
    val phi_y0 = DoubleVector(numTokens)

    for (j <- 0 until numTokens) {
      var spamwordcount = 0.0
      var spam_totalwords = 0.0
      var nonspamwordcount = 0.0
      var nonspam_totalwords = 0.0

      for (i <- 0 until numTrainDocs) {
        if (ts.classifications(i) == 1) {
          spamwordcount += ts.featuresT(j,i)
          spam_totalwords += words_per_email(i)
        }
        else {
          nonspamwordcount += ts.featuresT(j,i)
          nonspam_totalwords += words_per_email(i)
        }
      }
      phi_y1(j) = (spamwordcount + 1) / (spam_totalwords + numTokens)
      phi_y0(j) = (nonspamwordcount + 1) / (nonspam_totalwords + numTokens)
    }

    val phi_y = spamcount / numTrainDocs

    (phi_y1, phi_y0, phi_y)
  }

  def test(testdata : Dataset, phi_y1 : DoubleVector, phi_y0 : DoubleVector, phi_y : Double) : Int = {

    val numTestDocs = testdata.numDocs
    val numTokens = testdata.numTokens

    println("Testing model on " + numTestDocs + " documents.")

    var output = DoubleVector(numTestDocs)
    for (j <- 0 until numTestDocs) {
      var p_norm = 0.0
      var p_spam = 0.0
      for (i <- 0 until numTokens) {
        if (testdata.features(j,i) > 0) {
          p_norm += (math.log(phi_y0(i)) + math.log(1-phi_y)) * testdata.features(j,i)
          p_spam += (math.log(phi_y1(i)) + math.log(phi_y)) * testdata.features(j,i)
        }
      }
      if (p_spam > p_norm) output(j) = 1.0
      else output(j) = 0.0
    }

    // Compute error on test set
    var incorrect_classifications = 0
    for (i <- 0 until numTestDocs){
      if (testdata.classifications(i) != output(i))
        incorrect_classifications += 1
    }
    incorrect_classifications
  }

  private object io {

    def readData(filename: String): Dataset = {
      // parse the input matrix into the following elements:
      //  inMatrix:   a (numDocs x numTokens) matrix, where each row represents a unique document
      //                 the jth column of row i represents the number of times the jth token appeared in doc i
      //  tokenlist:  a long string containing the list of all tokens (words)
      //  inCategory: a (numDocs x 1) vector containing the true classifications for the documents just read
      //                 the ith entry gives the correct class for the ith email, where spam is 1 and non-spam is 0.

      val (inMatrix, tokenlist, inCategory) = MLInputReader.readTokenMatrix(filename)
      val (numDocs, numTokens) = MLInputReader.getTokenMatrixStats(filename)
      val ds = new Dataset(numDocs, numTokens, inMatrix, inCategory)
      ds
    }
  }

}