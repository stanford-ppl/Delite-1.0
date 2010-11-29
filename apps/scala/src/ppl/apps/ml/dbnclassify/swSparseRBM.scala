package ppl.apps.ml.dbnclassify

import ppl.delite.core.appinclude._
//import ppl.apps.ml.io.MLInputReader
import ppl.delite.dsl.optiml.io.MLInputReader
import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.Precursors._
import ppl.delite.metrics._
import ppl.delite.dsl.primitive.DeliteDouble
import ppl.delite.core.{Delite, DeliteApplication}
import java.io.{BufferedWriter, File, PrintWriter, FileWriter}
import java.util.Random

/**
 * Created by IntelliJ IDEA.
 * User: joe
 * Date: Aug 19, 2010
 * Time: 4:24:26 PM
 * To change this template use File | Settings | File Templates.
 */

object swSparseRBM extends DeliteApplication {

  def run(args: Array[String]) = {
    if (args.length < 7) print_usage

    Delite.init = true

    val numHiddenUnits = Integer.parseInt(args(5)).intValue()
    val numdims = Integer.parseInt(args(4)).intValue()

    var epsilonw = 0.01 // Learning rate for weights
    var epsilonvb = 0.01 // Learning rate for biases of visible units
    var epsilonhb = 0.01 //Learning rate for biases of hidden units
    val weightcost = 0.001
    val initialmomentum = 0.5
    val finalmomentum = 0.9
    var sigma = 1 //the sigma used in the sparse RBM
    val targetActivation = 0.06 //the sparsity target
    val learningRateSparsity = 3.0 //set to 0 to disable target sparsity
    var averageActivations = Vector.zeros(numHiddenUnits)
    val zero_cutoff = 0.05 //cutoff for when this will map small weights to 0

    println("Reading MNIST dataset")
    // These are hardcoded for now
    val numcases = 100 // batchsize
    val numbatches = 600
      //numbatches * batchsize = 60000, which is the actual size of the training set
    val trainingdata = MLInputReader.read(args(0))

    // Initialize symmetric weights and biases
    var vishid = 0.1 * Matrix.randn(numdims, numHiddenUnits)
    var hidbiases = Vector.zeros(numHiddenUnits)
    var visbiases = Vector.zeros(numdims)

    var vishidinc = Matrix.zeros(numdims, numHiddenUnits)
    var hidbiasinc = Vector.zeros(numHiddenUnits)
    var visbiasinc = Vector.zeros(numdims)

    //error tracking
    var err_diff = 99999.9
    var prev_error_sum = 99999.9
    var iter = 0
    var errsum = DeliteDouble(9999999.9)
    var weightsNotSparse = true

    PerformanceTimer.start("SparseWeightSparseRBM: " + args(6))
    Delite.init = false

    while(iter < 200 && errsum.value > 0.000001 && weightsNotSparse){
      errsum = 0.0
      val momentum = if (iter > 5) finalmomentum else initialmomentum

      PerformanceTimer.start("iter" + iter)

      for (batch <- 0 until numbatches) {

        var data = trainingdata.sliceRows(batch * numcases, (batch + 1) * numcases) // data: numcases x numdims
//        if(batch % 100 == 0) println("Saving to: " + args(1) + "  Iteration (epoch): " + iter + ", Batch: " + batch)

        //Initial Positive phase
        val poshidprobs =  ((data * vishid + hidbiases.repmat(numcases, 1)) /** (1.0 / (sigma*sigma))*/ ).sigmoid
        val poshidstates = poshidprobs > Matrix.rand(numcases, numHiddenUnits)

        //keep these constant for weight update later
        val posprods = data.trans * poshidprobs
        val posvisact = data.sumCol
        val poshidact = poshidprobs.sumCol

        //Initial Negative phase
        val negdata = ( poshidstates * vishid.trans + visbiases.repmat(numcases, 1) )
        val neghidprobs = ((negdata * vishid + hidbiases.repmat(numcases, 1)) /** (1.0 / (sigma*sigma))*/ ).sigmoid

        val negprods = negdata.trans * neghidprobs
        val negvisact = negdata.sumCol
        val neghidact = neghidprobs.sumCol

        val diff = data - negdata
        errsum += (diff dot diff).sum[DeliteDouble]

        //update sparsity
        averageActivations = .01*averageActivations + .99*(poshidact)/numcases
        val sparsityError = averageActivations - targetActivation
        val sparsityinc =  sparsityError * learningRateSparsity * (0 - epsilonhb)


        // Update weights and biases
        vishidinc = vishidinc * momentum + epsilonw * ((posprods - negprods) / numcases  /*- (vishid * weightcost)*/)
        visbiasinc = visbiasinc * momentum + (posvisact - negvisact) * (epsilonvb / numcases)
        hidbiasinc = hidbiasinc * momentum + sparsityinc + (epsilonhb/numcases)*(poshidact-neghidact)

        vishid = vishid + vishidinc
        visbiases = visbiases + visbiasinc
        hidbiases = hidbiases + hidbiasinc
      }

//      if(sigma > 0.05) sigma = sigma*0.99 //sigma decay

      //check sparsity of weight matrix
      if(iter % 1 == 0){
        PerformanceTimer.start("sparseCheck"+iter)

        val newZeros = (vishid.cutoffAndReturn(zero_cutoff))
        val sparsity : Double = 1.0 - ((newZeros.sum[DeliteDouble] * 1.0)/(numdims*numHiddenUnits)) // number of zeros / matrix size
        println("sparsity of weight matrix = " + sparsity)
        if(sparsity < .1)
          weightsNotSparse = false

        PerformanceTimer.stop("sparseCheck"+iter)
        PerformanceTimer.print("sparseCheck"+iter)
      }

      PerformanceTimer.stop("iter" + iter)
      PerformanceTimer.print("iter" + iter)

      err_diff = prev_error_sum - errsum.value
      prev_error_sum = errsum.value
      iter = iter + 1
      println("--> ending iteration " + iter + " this iterations error sum: " + errsum.value + " and err_diff = " + err_diff)
    }

    //For debugging the latter half - TODO: remove
//    vishid = MLInputReader.read("sparseWeights/vis_hidW")
//    val asdf = vishid.cutoffAndReturn(.05)
//    Delite.init = true

    // ============================================= begin sparse matrix weight phase =================================
    PerformanceTimer.start("converting")
    //make new sparse weight matrix and continue gibbs sampling.....
    var sparseVisHid = SparseMatrix.fromMatrix(vishid)
    println("switching at iter " + iter)
    println("sparseVisHid sparsity: " + sparseVisHid.sparsity)
    PerformanceTimer.stop("converting")
    PerformanceTimer.print("converting")

    while(iter < 200 && errsum.value > 0.000001){

      PerformanceTimer.start("sparse_iter_" + (iter+1))

      //initialize stuff for this iteration
      errsum = 0.0
      val momentum = if (iter > 20) finalmomentum else initialmomentum

      for (batch <- 0 until numbatches) {

        val data = trainingdata.sliceRows(batch * numcases, (batch + 1) * numcases) // data: numcases x numdims

        // Get the first sampling so everything is initialized, otherwise compile complains
        //Initial Positive phase
        val poshidprobs = (data * sparseVisHid + hidbiases.repmat(numcases, 1)).sigmoid
        val poshidstates = poshidprobs > Matrix.rand(numcases, numHiddenUnits)

        //keep these constant for weight update later
        val posprods = data.trans * poshidprobs
        val poshidact = poshidprobs.sumCol
        val posvisact = data.sumCol


        //Initial Negative phase
        val negdata = (poshidstates * sparseVisHid.trans + visbiases.repmat(numcases, 1)).sigmoid
        val neghidprobs = (negdata * sparseVisHid + hidbiases.repmat(numcases, 1)).sigmoid

        val negprods = negdata.trans * neghidprobs
        val neghidact = neghidprobs.sumCol
        val negvisact = negdata.sumCol

        val diff = data - negdata
        errsum += (diff dot diff).sum[DeliteDouble]


        //update sparsity
        averageActivations = .01*averageActivations + .99*(poshidact)/numcases
        val sparsityError = averageActivations - targetActivation
        val sparsityinc =  sparsityError * learningRateSparsity * (0 - epsilonhb)

        // Update weights and biases
        vishidinc = vishidinc * momentum + epsilonw * ((posprods - negprods) / numcases  /*- (sparseVisHid * weightcost)*/)
        visbiasinc = visbiasinc * momentum + (posvisact - negvisact) * (epsilonvb / numcases)
        hidbiasinc = hidbiasinc * momentum + sparsityinc + (epsilonhb/numcases)*(poshidact-neghidact)

        sparseVisHid.plusEqualsNNZOnly(vishidinc)
        visbiases = visbiases + visbiasinc
        hidbiases = hidbiases + hidbiasinc

      }

      if(iter % 10 == 0){ //every 10th iter, remove small values and cleanup the sparse matrix (cleanup may be time consuming?)
        PerformanceTimer.start("MapAndClean_" + iter)

        sparseVisHid.mapNNZ(e => if(e < zero_cutoff) 0.0 else e)
        sparseVisHid.cleanup

        PerformanceTimer.stop("MapAndClean_" + iter)
        PerformanceTimer.print("MapAndClean_" + iter)
      }

      err_diff = prev_error_sum - errsum.value
      prev_error_sum = errsum.value
      iter = iter + 1
      println("--> ending iteration " + iter + " this iterations error sum: " + errsum.value + " and err_diff = " + err_diff)

      PerformanceTimer.stop("sparse_iter_" + iter)
      PerformanceTimer.print("sparse_iter_" + iter)
    }

    // ============================================== end training with gibbs sampling =======================================

    PerformanceTimer.stop("SparseWeightSparseRBM: " + args(6))
    PerformanceTimer.print("SparseWeightSparseRBM: " + args(6))

    print("Saving transition weights and biases.......")
    sparseVisHid.force
    visbiases.force
    hidbiases.force
    saveSparseMatrix(sparseVisHid, args(1))
    saveVector(visbiases, args(2))
    saveVector(hidbiases, args(3))
    println("done")

    print("Generating and saving the hidden layer vectors for training next level....")
    val data = trainingdata.sliceRows(0, numbatches * numcases)
    val hidprobs = ((data * sparseVisHid) + hidbiases.repmat((numbatches*numcases), 1)).sigmoid
    hidprobs.force
    saveMatrix(hidprobs, args(6) + "LowerLayerTrain")
    println("done")

  }

  def print_usage = {
    println("Usage: RBM <training directory> <Output name of weights> <Output name of visible biases> <Output name of hidden biases>")
    exit(-1)
  }

  def saveSparseMatrix(matrix: SparseMatrix[Double], filename: String) = {
    try {
      var file = new File(filename)
      if(file.exists()) {
        println("this file exists! ..deleting file")
        file.delete()
      }

      val writer = new PrintWriter(new BufferedWriter(new FileWriter(file, true)));

      for(row <- 0 until matrix.numRows ) {
        for(col <- 0 until matrix.numCols ) {
          writer.print(matrix(row, col) + " ")
        }
        writer.println()
      }
      writer.close()
    }
    catch {
      case e: Exception => println("Unable to save the matrix to " + filename + "!")
    }
  }

  def saveMatrix(matrix: Matrix[Double], filename: String) = {
    try {
      var file = new File(filename)
      if(file.exists()) {
        println("this file exists! ..deleting file")
        file.delete()
      }

      val writer = new PrintWriter(new BufferedWriter(new FileWriter(file, true)));

      for(row <- 0 until matrix.numRows ) {
        for(col <- 0 until matrix.numCols ) {
          writer.print(matrix(row, col) + " ")
        }
        writer.println()
      }
      writer.close()
    }
    catch {
      case e: Exception => println("Unable to save the matrix to " + filename + "!")
    }
  }

  def saveVector(vector: Vector[Double], filename: String) = {
    try{
      var file = new File(filename)
      if(file.exists()) {
        println("this file exists! ..deleting file")
        file.delete()
      }

      val writer = new PrintWriter(new BufferedWriter(new FileWriter(file, true)));

      for(col <- 0 until vector.length ) {
        writer.println(vector(col))
      }
      writer.close()
    }
    catch {
      case e: Exception => println("Unable to save the matrix to " + filename + "!")
    }
  }

}