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

object RF_RBM extends DeliteApplication {

  def run(args: Array[String]) = {
    if (args.length < 7) print_usage

    Delite.init = true

    val numHiddenUnits = Integer.parseInt(args(5)).intValue()
    val numdims = Integer.parseInt(args(4)).intValue()
    val xDim = Integer.parseInt(args(7)).intValue() //xDim of image
    val yDim = Integer.parseInt(args(8)).intValue() //yDim of image

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

    // Initialize symmetric weights and biases - change sparse/density of weight matrix here!!!!!!!!!!!!!!!
    var vishid = makeNeighborSparseMatrix(xDim, yDim, 4)
    println("vishid sparsity " + (vishid.size*1.0)/(vishid.numRows*vishid.numCols))
//    var vishid = makeNeighborMatrix(xDim,yDim, 3)
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

    PerformanceTimer.start("RBM")
    Delite.init = false

    while(iter < 250 && errsum.value > 0.000001){
      PerformanceTimer.start("iter"+iter)

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
        if(batch % 100 == 0) println("Saving to: " + args(1) + "  Iteration (epoch): " + iter + ", Batch: " + batch)


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
          var newdata = (1 + (-neghidstates * vishid.trans - visbiases.repmat(numcases, 1)).exp).reciprocal

          for(gs <- 0 until numGibbSamples){
            // Positive phase
            poshidprobs = (1 + (-newdata * vishid - hidbiases.repmat(numcases, 1)).exp).reciprocal
            poshidstates = poshidprobs > Matrix.rand(numcases, numHiddenUnits)

            // Negative phase
            negdata = (1 + (-poshidstates * vishid.trans - visbiases.repmat(numcases, 1)).exp).reciprocal
            neghidprobs = (1 + (-negdata * vishid - hidbiases.repmat(numcases, 1)).exp).reciprocal

             //if we're doing another iteration of gibbs sampling....
            if(gs != (numGibbSamples -1)){
              neghidstates = neghidprobs > Matrix.rand(numcases, numHiddenUnits)
              newdata = (1 + (-neghidstates * vishid.trans - visbiases.repmat(numcases, 1)).exp).reciprocal
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

        vishid = vishid.plusEqualsNNZOnly(vishidinc)
//        vishid = vishid.addNNZvaluesOnly(vishidinc)

        visbiases = visbiases + visbiasinc
        hidbiases = hidbiases + hidbiasinc
      }

      PerformanceTimer.stop("iter"+iter)
      PerformanceTimer.print("iter"+iter)

      err_diff = prev_error_sum - errsum.value
      prev_error_sum = errsum.value
      iter = iter + 1
      println("--> ending iteration " + iter + " this iterations error sum: " + errsum.value + " and err_diff = " + err_diff)
    }

    PerformanceTimer.stop("RBM")

    PerformanceTimer.print("DoubleMatrix_OP_mult_single")
    PerformanceTimer.print("RBM")

    print("Saving transition weights and biases.......")
    vishid.force
    visbiases.force
    hidbiases.force
    saveSparseMatrix(vishid, args(1))
//    saveMatrix(vishid, args(1))
    saveVector(visbiases, args(2))
    saveVector(hidbiases, args(3))
    println("done")

    print("Generating and saving the hidden layer vectors for training next level....")
    val data = trainingdata.sliceRows(0, numbatches * numcases)
    val hidprobs = (1 + (- (data * vishid) - hidbiases.repmat((numbatches*numcases), 1)).exp).reciprocal
    hidprobs.force
    saveMatrix(hidprobs, args(6) + "LowerLayerTrain")
    println("done")

    //PerformanceTimer.save("DoubleMatrix_OP_mult_single")
    //PerformanceTimer.save("RBM")
  }

  def makeNeighborMatrix(xdim: Int, ydim: Int, size: Int) : Matrix[Double] = {
    var N = Matrix.zeros(xdim*ydim, xdim*ydim)

    for(y <- 1 until (ydim + 1)){
      for(x <- 1 until (xdim + 1)){

        val iter = (y-1)*xdim + x

        for(rowoff <- (0-size) until (size+1)){
          for(coloff <- (0-size) until (size+1)){

            var row = y+rowoff
            if(row > ydim) row = row - ydim
            else if(row < 1) row = row + ydim

            var col = x + coloff
            if(col > xdim) col = col - xdim
            else if(col < 1) col = col + xdim

            val toPut = (row - 1)*xdim + col
            N((iter-1), (toPut-1)) = 1.0/(size*2 + 1)
            //the end matrix normalization code for this function
            //always sets all non-zero values equal to 1/(size*2 + 1)

          }
        }
      }
    }

    return N

  }

  def makeNeighborSparseMatrix(xdim: Int, ydim: Int, size: Int) : SparseMatrix[Double] = {
    var N = SparseMatrix.zeros(xdim*ydim, xdim*ydim)

    for(y <- 1 until (ydim + 1)){
      for(x <- 1 until (xdim + 1)){

        val iter = (y-1)*xdim + x

        for(rowoff <- (0-size) until (size+1)){
          for(coloff <- (0-size) until (size+1)){

            var row = y+rowoff
            if(row > ydim) row = row - ydim
            else if(row < 1) row = row + ydim

            var col = x + coloff
            if(col > xdim) col = col - xdim
            else if(col < 1) col = col + xdim

            val toPut = (row - 1)*xdim + col
            N((iter-1), (toPut-1)) = 1.0/(size*2 + 1)
            //the end matrix normalization code for this function
            //always sets all non-zero values equal to 1/(size*2 + 1)

          }
        }
      }
    }

    return N

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
