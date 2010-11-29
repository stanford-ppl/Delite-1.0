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

object SparseRBM extends DeliteApplication {

  def run(args: Array[String]) = {
    if (args.length < 7) print_usage

    Delite.init = true

    val maxEpoch = 50 // maximum number of epochs
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

    PerformanceTimer.start("SparseRBM: " + args(6))
    Delite.init = false

    while(iter < 200 && errsum.value > 0.000001){
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
        vishidinc = vishidinc * momentum + epsilonw * ((posprods - negprods) / numcases  /*- (vishid * weightcost)*/ )
        visbiasinc = visbiasinc * momentum + (posvisact - negvisact) * (epsilonvb / numcases)
        hidbiasinc = hidbiasinc * momentum + sparsityinc + (epsilonhb/numcases)*(poshidact-neghidact)

        vishid = vishid + vishidinc
        visbiases = visbiases + visbiasinc
        hidbiases = hidbiases + hidbiasinc
      }

//      if(sigma > 0.05) sigma = sigma*0.99 //sigma decay

      PerformanceTimer.stop("iter" + iter)
      PerformanceTimer.print("iter" + iter)

      err_diff = prev_error_sum - errsum.value
      prev_error_sum = errsum.value
      iter = iter + 1
      println("--> ending iteration " + iter + " this iterations error sum: " + errsum.value + " and err_diff = " + err_diff)

//      val newZeros = (vishid.cutoffAndReturn(.001))
//      val sparsity : Double = 1.0 - ((newZeros.sum[DeliteDouble] * 1.0)/(numdims*numHiddenUnits)) // number of zeros / matrix size
//      println("sparsity of weight matrix = " + sparsity)
    }

    PerformanceTimer.stop("SparseRBM: " + args(6))
    PerformanceTimer.print("SparseRBM: " + args(6))

    print("Saving transition weights and biases.......")
    vishid.force
    visbiases.force
    hidbiases.force
    saveMatrix(vishid, args(1))
    saveVector(visbiases, args(2))
    saveVector(hidbiases, args(3))
    println("done")

    print("Generating and saving the hidden layer vectors for training next level....")
    val data = trainingdata.sliceRows(0, numbatches * numcases)
    val hidprobs = ((data * vishid) + hidbiases.repmat((numbatches*numcases), 1)).sigmoid
    hidprobs.force
    saveMatrix(hidprobs, args(6) + "LowerLayerTrain")
    println("done")

  }

  def print_usage = {
    println("Usage: RBM <training directory> <Output name of weights> <Output name of visible biases> <Output name of hidden biases>")
    exit(-1)
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