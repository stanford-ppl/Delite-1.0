package ppl.apps.ml.baseline.rbm

import ppl.apps.ml.baseline.collection._
import ppl.apps.ml.baseline.collection.Precursor._
import ppl.delite.metrics.PerformanceTimer

object RBM {
  def print_usage = {
    println("Usage: RBM <MNIST data file> <numHiddenUnits> <numcases>")
    exit(-1)
  }

  def main(args: Array[String]) = {

    if (args.length < 3) print_usage

    val maxEpoch = 10 // maximum number of epochs
    val numHiddenUnits = args(1).toInt

    val epsilonw = 0.1f // Learning rate for weights
    val epsilonvb = 0.1f // Learning rate for biases of visible units
    val epsilonhb = 0.1f //Learning rate for biases of hidden units
    val weightcost = 0.0002f
    val initialmomentum = 0.5f
    val finalmomentum = 0.9f

    println("Using " + numHiddenUnits + " hidden units.")
    
    println("Reading MNIST dataset")
    val numcases = args(2).toInt // batchsize
    //val numcases = 100 // batchsize
    val trainingdata = MLInputReader.readFloatMatrix(args(0))
    val numdims = trainingdata.width
    val numbatches = trainingdata.height / numcases

    // Initialize symmetric weights and biases
    var vishid = 0.1f * FloatMatrix.randomGaussian(numdims, numHiddenUnits)
    var hidbiases = FloatVector(numHiddenUnits)
    var visbiases = FloatVector(numdims)

    var vishidinc = FloatMatrix(numdims, numHiddenUnits)
    var hidbiasinc = FloatVector(numHiddenUnits)
    var visbiasinc = FloatVector(numdims)

    


    var numTimes = 10

    for (i <- 0 until numTimes){

    PerformanceTimer.start("RBMbaseline")

    for (epoch <- 0 until maxEpoch) {

      //var errsum = 0f
      for (batch <- 0 until numbatches) {

        //println("Epoch: " + epoch + ", Batch: " + batch)

        // Positive phase
        val data = trainingdata.sliceRows(batch * numcases, (batch + 1) * numcases) // data: numcases x numdims
        val poshidprobs = (1 + (-data * vishid - hidbiases.replicateMatrix(numcases, 1)).exp).reciprocal
        val posprods = data.trans * poshidprobs
        val poshidact = poshidprobs.sumColumns
        val posvisact = data.sumColumns
        val poshidstates = poshidprobs > FloatMatrix.random(numcases, numHiddenUnits)

        // Negative phase
        val negdata = (1 + (-poshidstates * vishid.trans - visbiases.replicateMatrix(numcases, 1)).exp).reciprocal
        val neghidprobs = (1 + (-negdata * vishid - hidbiases.replicateMatrix(numcases, 1)).exp).reciprocal
        val negprods = negdata.trans * neghidprobs
        val neghidact = neghidprobs.sumColumns
        val negvisact = negdata.sumColumns
        val diff = data - negdata
        //errsum += (diff dot diff).sum

        // Update weights and biases
        val momentum = if (epoch > 5) finalmomentum else initialmomentum
        vishidinc = vishidinc * momentum + ((posprods - negprods) / numcases * epsilonw - vishid * weightcost)
        visbiasinc = visbiasinc * momentum + (posvisact - negvisact) * (epsilonvb / numcases)
        hidbiasinc = hidbiasinc * momentum + (poshidact - neghidact) * (epsilonhb / numcases)

        vishid = vishid + vishidinc
        visbiases = visbiases + visbiasinc
        hidbiases = hidbiases + hidbiasinc
      }
      //println("--> Epoch " + epoch + " error = " + errsum)

    }

    PerformanceTimer.stop("RBMbaseline")
    PerformanceTimer.print("RBMbaseline")
    }
    PerformanceTimer.save("RBMbaseline")
    
  }


}
