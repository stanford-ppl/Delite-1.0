package ppl.apps.ml.baseline.svm

import ppl.delite.metrics.PerformanceTimer
import ppl.apps.ml.baseline.collection.{DoubleVector, MLInputReader}
import java.io.{FileWriter, BufferedWriter, PrintWriter, File}

object SVM {
  def print_usage = {
    println("Usage: SVM <train data file> <test data file> <model filename> <num tests>")
    exit(-1)
  }

  def main(args: Array[String]) = {
    if (args.length < 1) print_usage

    val trainfile = args(0)
    val testfile = args(1)
    val modelFile = args(2)
    val numTests = java.lang.Integer.parseInt(args(3))

    val runErrors = new java.util.Vector[Double]

    val f1 = new java.io.File(trainfile)
    val f2 = new java.io.File(testfile)
    if (!f1.exists || !f2.exists) print_usage

    // parse the input matrix into the following elements:
    //  inMatrix:   a (numDocs x numTokens) matrix, where each row represents a unique document
    //                 the jth column of row i represents the number of times the jth token appeared in doc i
    //  tokenlist:  a long string containing the list of all tokens (words)
    //  inCategory: a (numDocs x 1) vector containing the true classifications for the documents just read
    //                 the ith entry gives the correct class for the ith email, where spam is 1 and non-spam is 0.

    val (inMatrixTrain, tokenlistTrain, inCategoryTrain) = MLInputReader.readTokenMatrix(trainfile)
    val (inMatrixTest, tokenlistTest, inCategoryTest) = MLInputReader.readTokenMatrix(testfile)

    // adjust the classification labels to -1 and +1 for SMO
    val YTrain = inCategoryTrain.map(e => if (e == 0) -1; else 1)
    val YTest = inCategoryTest.map(e => if (e == 0) -1; else 1)

    for (iter <- 0 until numTests) {
      println(iter)
      // TRAIN FIRST
      PerformanceTimer.start("SVM")
      // run the SMO training algorithm
      val svm = new SVMModel()
      svm.smoTrain(inMatrixTrain, YTrain, 1, .01, 10)
      PerformanceTimer.stop("SVM")
      svm.computeWeights(inMatrixTrain, YTrain)
      svm.saveModel(modelFile)

      println("SVM training finished. Model saved to " + modelFile)

      // TEST RESULTS
      val numTestDocs = inMatrixTest.height
      val outputLabels = DoubleVector(numTestDocs)//.toInt(e => e.asInstanceOf[Int])
      val svm_test = new SVMModel(modelFile)
      for (i <- 0 until numTestDocs){
        outputLabels(i) = svm_test.classify(inMatrixTest(i))
      }
      println("SVM testing finished. Calculating error..")
      var errors = 0
      for (i <- 0 until numTestDocs){
        if (YTest(i) != outputLabels(i)) errors +=1
        //println("predicted class: " + outputLabels(i) + ", actual: " + Y(i))
      }
      printf("Classification error: %f\n", (errors.doubleValue()/numTestDocs.doubleValue()))
      runErrors.add(errors.doubleValue()/numTestDocs.doubleValue())
    }
    PerformanceTimer.print("SVM")
    PerformanceTimer.save("SVM")
    saveError(runErrors)
  }

  def saveError(errors: java.util.Vector[Double]) {
    try {
      val procstring = "-1"

      var file: String = new File("./").getCanonicalPath + "/SVM.p" + procstring + ".error"
      val writer = new PrintWriter(new BufferedWriter(new FileWriter(file, false)))
      for (i <- 0 until errors.size) {
        writer.println(errors.get(i) formatted ("%.10f"))
      }
      writer.close()
    }
    catch {
      case e: Exception => println("Unable to save SVM errors")
    }
  }
}