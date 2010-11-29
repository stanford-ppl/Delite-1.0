package ppl.apps.ml.rbm

import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml.io.MLInputReader
import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.Precursors
import ppl.delite.metrics._
import ppl.delite.dsl.primitive.DeliteFloat
import ppl.delite.core.{Delite, DeliteApplication}

/**
 * User: Anand Atreya
 * Adapted from Hinton's deep autoencoder implementation
 */

object RBM_GPU_Float extends DeliteApplication {
  def print_usage = {
    println("Usage: RBM <MNIST data file> <numHiddenUnits>")
    exit(-1)
  }

  def run(args: Array[String]) = {

    if (args.length < 2) print_usage

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
    val numcases = 100 // batchsize
    //val trainingdata = MLInputReader.readFloat(args(0) + "/trainingdata.ascii")
    val trainingdata = MLInputReader.read(args(0)).toFloat(e => e.floatValue())
    val numdims = trainingdata.numCols
    val numbatches = trainingdata.numRows / numcases

    // Initialize symmetric weights and biases
    var vishid = (Matrix.randnf(numdims, numHiddenUnits) * 0.1f)
    var hidbiases = Vector.zerosf(numHiddenUnits)
    var visbiases = Vector.zerosf(numdims)

    var vishidinc = Matrix.zerosf(numdims, numHiddenUnits)
    var hidbiasinc = Vector.zerosf(numHiddenUnits)
    var visbiasinc = Vector.zerosf(numdims)

    val datum = new Array[Matrix[Float]](numbatches)
    for(i <- 0 until numbatches)
      datum(i) = trainingdata.sliceRows(i*numcases,(i+1)*numcases)

    //val randNums = new Array[Matrix[Float]](numbatches)
    //for(i <- 0 until numbatches)
    //  randNums(i) = Matrix.randF(numcases, numHiddenUnits)

    Delite.init = false

	val numTimes = 10

	for(i <- 0 until numTimes) {

    PerformanceTimer.start("RBMGPU")

    //for (epoch <- 0 until 1) {
    for (epoch <- 0 until maxEpoch) {
      var errsum = DeliteFloat(0.0f)
      for (batch <- 0 until numbatches) {
        //println("Epoch: " + epoch + ", Batch: " + batch)

        // Positive phase
        val data = datum(batch)
        //val data = trainingdata.sliceRows(batch * numcases, (batch + 1) * numcases)

        val poshidprobs = ((data * vishid * -1.0f - hidbiases.repmat(numcases, 1)).exp + 1.0f).reciprocal
        val posprods = data.trans * poshidprobs
        val poshidact = poshidprobs.sumCol
        val posvisact = data.sumCol
        val poshidstates = poshidprobs > Matrix.randf(numcases, numHiddenUnits)
        //val poshidstates = poshidprobs > randNums(batch)
        //12
        // Negative phase

        val negdata = ((poshidstates * vishid.trans * -1.0f - visbiases.repmat(numcases, 1)).exp + 1.0f).reciprocal
        val neghidprobs = ((negdata * vishid * -1.0f - hidbiases.repmat(numcases, 1)).exp + 1.0f).reciprocal
        val negprods = negdata.trans * neghidprobs
        val neghidact = neghidprobs.sumCol
        val negvisact = negdata.sumCol
        val diff = data - negdata
        val diffdotdiff = diff dot diff
        //val summ = diffdotdiff.sum[DeliteFloat]

        errsum += diffdotdiff.sum[DeliteFloat]
        //21
        //diffdotdiff.force
        //errsum += diffdotdiff.sum[DeliteFloat]
        //errsum += DeliteFloat(1.0f)
        //errsum.force


        // Update weights and biases
        val momentum = if (epoch>5) finalmomentum else initialmomentum
        vishidinc = vishidinc * momentum + ((posprods - negprods) * (epsilonw / numcases) - vishid * weightcost)
        visbiasinc = visbiasinc * momentum + (posvisact - negvisact) * (epsilonvb / numcases)
        hidbiasinc = hidbiasinc * momentum + (poshidact - neghidact) * (epsilonhb / numcases)

        vishid = vishid + vishidinc
        visbiases = visbiases + visbiasinc
        hidbiases = hidbiases + hidbiasinc
        // 17
        //vishid.force
        //visbiases.force
        //hidbiases.force

      }
      println("--> Epoch " + epoch + " error = " + errsum.value)
    }
    hidbiases.force
    PerformanceTimer.stop("RBMGPU")
    PerformanceTimer.print("RBMGPU")
	}
    PerformanceTimer.save("RBMGPU")
  }
}
