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

/**
 * User: Anand Atreya
 * Adapted from Hinton's deep autoencoder implementation
 */

object RBM extends DeliteApplication {

  def run(args: Array[String]) = {
    if (args.length < 7) print_usage

    Delite.init = true

    val numHiddenUnits = Integer.parseInt(args(5)).intValue()
    val numdims = Integer.parseInt(args(4)).intValue()

    var epsilonw = 0.1 // Learning rate for weights
    var epsilonvb = 0.1 // Learning rate for biases of visible units
    var epsilonhb = 0.1 //Learning rate for biases of hidden units
    val weightcost = 0.0001
    val initialmomentum = 0.5
    val finalmomentum = 0.9

    println("Reading MNIST dataset")
    // These are hardcoded for now    
    val numcases = 100 // batchsize
    val numbatches = 600
      //numbatches * batchsize = 60000, which is the actual size of the training set
    val trainingdata = MLInputReader.read(args(0))
    //println("trainingdata rows: " + trainingdata.numRows + " cols: " + trainingdata.numCols)

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

    PerformanceTimer.start("RBM: " + args(6))
    Delite.init = false

    while(iter < 250 && errsum.value > 0.000001){

      PerformanceTimer.start("RBM_iter" + iter)

      errsum = 0.0
      val momentum = if (iter > 20) finalmomentum else initialmomentum
      if(iter > 100){  //decrease the learning rate in later iterations (as suggested in hinton's RBM guide)
        epsilonw = .01
        epsilonvb = .01
        epsilonhb = .01
      }


      for (batch <- 0 until numbatches) {

        var numGibbSamples = if(iter < 300) 0 else if(iter < 500) 5 else 11
        var data = trainingdata.sliceRows(batch * numcases, (batch + 1) * numcases) // data: numcases x numdims
        if(batch % 300 == 0) println("Saving to: " + args(1) + "  Iteration (epoch): " + iter + ", Batch: " + batch)


        // Get the first sampling so everything is initialized, otherwise compile complains
        //Initial Positive phase
        var poshidprobs = (data * vishid + hidbiases.repmat(numcases, 1)).sigmoid
        var poshidstates = poshidprobs > Matrix.rand(numcases, numHiddenUnits)

        //keep these constant for weight update later
        val posprods = data.trans * poshidprobs
        val poshidact = poshidprobs.sumCol
        val posvisact = data.sumCol

        //Initial Negative phase
        var negdata = (poshidstates * vishid.trans + visbiases.repmat(numcases, 1)).sigmoid
        var neghidprobs = (negdata * vishid + hidbiases.repmat(numcases, 1)).sigmoid

        if(numGibbSamples != 0){

          var neghidstates = neghidprobs > Matrix.rand(numcases, numHiddenUnits)
          var newdata = (neghidstates * vishid.trans + visbiases.repmat(numcases, 1)).sigmoid

          for(gs <- 0 until numGibbSamples){
            // Positive phase
            poshidprobs = (newdata * vishid + hidbiases.repmat(numcases, 1)).sigmoid
            poshidstates = poshidprobs > Matrix.rand(numcases, numHiddenUnits)

            // Negative phase
            negdata = (poshidstates * vishid.trans + visbiases.repmat(numcases, 1)).sigmoid
            neghidprobs = (negdata * vishid + hidbiases.repmat(numcases, 1)).sigmoid

             //if we're doing another iteration of gibbs sampling....
            if(gs != (numGibbSamples -1)){
              neghidstates = neghidprobs > Matrix.rand(numcases, numHiddenUnits)
              newdata = (neghidstates * vishid.trans + visbiases.repmat(numcases, 1)).sigmoid
            }
          } 
        }

        val negprods = negdata.trans * neghidprobs
        val neghidact = neghidprobs.sumCol
        val negvisact = negdata.sumCol

        val diff = data - negdata
        errsum += (diff dot diff).sum[DeliteDouble]

        // Update weights and biases
        vishidinc = vishidinc * momentum + epsilonw * ((posprods - negprods) / numcases  - (vishid * weightcost))
        visbiasinc = visbiasinc * momentum + (posvisact - negvisact) * (epsilonvb / numcases)
        hidbiasinc = hidbiasinc * momentum + (poshidact - neghidact) * (epsilonhb / numcases)

        vishid = vishid + vishidinc
        visbiases = visbiases + visbiasinc
        hidbiases = hidbiases + hidbiasinc

      }

      
      PerformanceTimer.stop("RBM_iter" + iter)
      PerformanceTimer.print("RBM_iter" + iter)

      err_diff = prev_error_sum - errsum.value
      prev_error_sum = errsum.value
      iter = iter + 1
      println("--> ending iteration " + iter + " this iterations error sum: " + errsum.value + " and err_diff = " + err_diff)
    }

    PerformanceTimer.stop("RBM: " + args(6))
    PerformanceTimer.print("RBM: " + args(6))

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
    val hidprobs = (1 + (- (data * vishid) - hidbiases.repmat((numbatches*numcases), 1)).exp).reciprocal
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
