package ppl.apps.ml.svm

/* Entry point for SVM application.
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  6/21/09
 * modified: 6/23/09
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

import ppl.delite.core.DeliteApplication
import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml.{Vector, Matrix}
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.dsl.optiml.Precursors._
import ppl.delite.dsl.optiml.io.MLInputReader
import java.io.{BufferedReader, FileReader}
import ppl.delite.metrics._
import java.io.{BufferedWriter, File, PrintWriter, FileWriter}
import ppl.delite.core.Config
import ppl.delite.core.executor.DeliteGPUThreadPool

object SVM_GPU extends DeliteApplication {
  def print_usage = {
    println("Usage: SVM <train data file> <test data file> <model filename> <num tests>")
    exit(-1)
  }

  def run(args: Array[String]) = {
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
      //DeliteGPUThreadPool.threads(0).cleanup_done = false
      //DeliteGPUThreadPool.threads(0).cleanup = true
      //while(!DeliteGPUThreadPool.threads(0).cleanup_done) {}
      
	  println(iter)
      // TRAIN FIRST
	  val inMatrixTrainD = inMatrixTrain.toDouble
   	  val YTrainD = YTrain.toDouble
      
	  PerformanceTimer.start("SVMGPU")
      // run the SMO training algorithm
      val svm = new SVMModel_GPU()
      svm.smoTrain(inMatrixTrainD, YTrainD, 1, .001, 10)
      PerformanceTimer.stop("SVMGPU")
      PerformanceTimer.print("SVMGPU")
      
	  if(iter == numTests-1) {
	  	svm.saveModel(modelFile)
      	println("SVM training finished. Model saved to " + modelFile)

      	// TEST RESULTS
      	val numTestDocs = inMatrixTest.numRows
      	val outputLabels = Vector.zeros(numTestDocs).toInt(e => e.asInstanceOf[Int])
      	val svm_test = new SVMModel_GPU(modelFile)
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
    }
    PerformanceTimer.save("SVMGPU")
    //saveError(runErrors)
  }

  def saveError(errors: java.util.Vector[Double]) {
    try {
      val procstring = {
        if (Config.bypassExecutor && Config.debugEnabled) {
        // set procs to -1 for bypassExecutor case
          "-1"
        } else {
          Config.CPUThreadNum.toString
        }
      }
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
