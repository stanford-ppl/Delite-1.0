package ppl.apps.ml.rbm

import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml.io.MLInputReader
import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.Precursors._
import ppl.delite.metrics._
import ppl.delite.core.{Delite, DeliteApplication}
import ppl.delite.dsl.primitive.{DeliteFloat, DeliteDouble}

/**
 * User: Anand Atreya
 * Adapted from Hinton's deep autoencoder implementation
 */

object RBM extends DeliteApplication {
  def print_usage = {
    println("Usage: RBM <MNIST data file> <numHiddenUnits> <numcases>")
    exit(-1)
  }

  def run(args: Array[String]) = {

    if (args.length < 3) print_usage

    Delite.init = true

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
    val trainingdata = MLInputReader.read(args(0)).toFloat(e => e.floatValue())
    val numdims = trainingdata.numCols
    val numbatches = trainingdata.numRows / numcases

    // Initialize symmetric weights and biases
    var vishid = Matrix.randnf(numdims, numHiddenUnits) * 0.1f
    var hidbiases = Vector.zerosf(numHiddenUnits)
    var visbiases = Vector.zerosf(numdims)

    var vishidinc = Matrix.zerosf(numdims, numHiddenUnits)
    var hidbiasinc = Vector.zerosf(numHiddenUnits)
    var visbiasinc = Vector.zerosf(numdims)

    Delite.init = false

    var numTimes = 10

    for (i <- 0 until numTimes){

    PerformanceTimer.start("RBM")

    for (epoch <- 0 until maxEpoch) {
      //var errsum = DeliteFloat(0)
      for (batch <- 0 until numbatches) {
        //println("Epoch: " + epoch + ", Batch: " + batch)

        // Positive phase
        //PerformanceTimer.start("RBM-posphase", false)
        val data = trainingdata.sliceRows(batch * numcases, (batch + 1) * numcases) // data: numcases x numdims
        val poshidprobs = (data * vishid + hidbiases.repmat(numcases, 1)).sigmoid
        val posprods = data.trans * poshidprobs
        val poshidact = poshidprobs.sumCol
        val posvisact = data.sumCol
        val poshidstates = poshidprobs > Matrix.randf(numcases, numHiddenUnits)
        //PerformanceTimer.stop("RBM-posphase", false)

        // Negative phase
        //PerformanceTimer.start("RBM-negphase", false)
        val negdata = (poshidstates * vishid.trans + visbiases.repmat(numcases, 1)).sigmoid
        val neghidprobs = (negdata * vishid + hidbiases.repmat(numcases, 1)).sigmoid
        val negprods = negdata.trans * neghidprobs
        val neghidact = neghidprobs.sumCol
        val negvisact = negdata.sumCol
        val diff = data - negdata
        //errsum += (diff dot diff).sum[DeliteFloat]
        //PerformanceTimer.stop("RBM-negphase", false)

        // Update weights and biases
        //PerformanceTimer.start("RBM-biasupdates", false)
        val momentum = if (epoch > 5) finalmomentum else initialmomentum
        vishidinc = vishidinc * momentum + epsilonw * ((posprods - negprods) / numcases  - (vishid * weightcost))
        visbiasinc = visbiasinc * momentum + (posvisact - negvisact) * (epsilonvb / numcases)
        hidbiasinc = hidbiasinc * momentum + (poshidact - neghidact) * (epsilonhb / numcases)

        vishid = vishid + vishidinc
        visbiases = visbiases + visbiasinc
        hidbiases = hidbiases + hidbiasinc
        //PerformanceTimer.stop("RBM-biasupdates", false)
      }
      //println("--> Epoch " + epoch + " error = " + errsum.value)
    }

    PerformanceTimer.stop("RBM")
    PerformanceTimer.print("RBM")
    }
    PerformanceTimer.save("RBM")    
    //PerformanceTimer.print("RBM-posphase")
    //PerformanceTimer.save("RBM-posphase")
    //PerformanceTimer.print("RBM-negphase")
    //PerformanceTimer.save("RBM-negphase")
    //PerformanceTimer.print("RBM-biasupdates")
    //PerformanceTimer.save("RBM-biasupdates")
  }
}
