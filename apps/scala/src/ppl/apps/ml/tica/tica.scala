package ppl.apps.ml.tica

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
 * User: joe
 * Date: Jul 12, 2010
  An adaptation of the TICA algorithm from the natural image statistics textbook (www.naturalimagestatistics.net)
 */

object tica extends DeliteApplication {

  def run(args: Array[String]) = {

    Delite.init = true
    //set this true if you want to use a smaller dataset
    val useSmallerDataSet=false

    print("loading whitened data... ")
    var data = MLInputReader.read("data/ml/nis/whitenedimages")
//    var data = MLInputReader.read("data/ml/nis/smallwhite")
    println("done")

    //chopping off data from whitened images generally doesn't let TICA converge! be warned!
    if(useSmallerDataSet) data = data.removeCols(0, (data.numCols - 1000)) 

    val dataSize = data.numCols
    println("data numRows " + data.numRows + " numcols " + data.numCols)
    //saveMatrix(data, "data/ml/nis/smallwhite")

    //dimensions for the topographic grid - set higher if you want sparser neighbor matrix later on
    val topoXDim = 16
    val topoYDim = 16

    //neighborhood size - set lower if you want sparser neighbor matrix later on
    val neighborhoodSize = 1

    PerformanceTimer.start("makeNM")
    //get neighbor matrix
    //val N = makeNeighborMatrix(topoXDim, topoYDim, neighborhoodSize)
    val N = makeNeighborSparseMatrix(topoXDim, topoYDim, neighborhoodSize)
    PerformanceTimer.stop("makeNM")
    PerformanceTimer.print("makeNM")

    //initialize orthogonalized weight matrix
    var W = orthogonalizeRows(Matrix.randn(topoXDim*topoYDim, data.numRows))

    //initial step size
    var mu = 1.0

    var iter = 0
    var notconverged = true

    Delite.init = false
    print("starting topographic ICA. Iteration : ")

    while(notconverged && (iter < 200)){

      iter = iter + 1
      print(iter + " ")
      PerformanceTimer.start("mainIter" + iter)
//      PerformanceTimer.start("test1-" + iter)
      //compute seperately estimates of independent components to speed up
      val Y = W*data
//      PerformanceTimer.stop("test1-" + iter)
//      PerformanceTimer.print("test1-" + iter)
//      PerformanceTimer.start("test2-" + iter)

      //compute local energies -- this operation is a sparse * dense matrix multiplication
      val K = N*(Y.map(e => scala.math.pow(e, 2)))
//      PerformanceTimer.stop("test2-" + iter)
//      PerformanceTimer.print("test2-" + iter)
//      PerformanceTimer.start("test3-" + iter)

      val epsilon = 0.1
      var gK = -((epsilon + K).map(e => if(e > 0.0) scala.math.pow(e, -0.5) else 0.0))
//      PerformanceTimer.stop("test3-" + iter)
//      PerformanceTimer.print("test3-" + iter)
//      PerformanceTimer.start("test4-" + iter)

      //calculate convolution with neighborhood - this operation is a sparse * dense matrix multiplication
      val F = N*gK
//      PerformanceTimer.stop("test4-" + iter)
//      PerformanceTimer.print("test4-" + iter)
//      PerformanceTimer.start("test5-" + iter)

      //calculate basic gradient
      var grad = (Y dot F)*(data.trans)/dataSize
//      PerformanceTimer.stop("test5-" + iter)
//      PerformanceTimer.print("test5-" + iter)
//      PerformanceTimer.start("test6-" + iter)

      grad = grad - (W*(grad.trans)*W)
//      PerformanceTimer.stop("test6-" + iter)
//      PerformanceTimer.print("test6-" + iter)
//      PerformanceTimer.start("test7-" + iter)

      //store old value
      val Wold = W.clone

      //do gradient step
      W = W + (mu*grad)
//      PerformanceTimer.stop("test7-" + iter)
//      PerformanceTimer.print("test7-" + iter)
//      PerformanceTimer.start("test8-" + iter)

      //orthogonalize rows and decorrelate estimated components
      W = orthogonalizeRows(W)
//      PerformanceTimer.stop("test8-" + iter)
//      PerformanceTimer.print("test8-" + iter)

      
      //change step size every 10th step
      if(iter%10 == 0 /*|| iter == 1*/){
        PerformanceTimer.start("test9-" + iter)
        val changefactor = 4/3

        //take different length steps
        var Wnew1 = Wold + (1/(changefactor*mu*grad))
        var Wnew2 = Wold + (changefactor*mu*grad)
        Wnew1 = orthogonalizeRows(Wnew1)
        Wnew2 = orthogonalizeRows(Wnew2)
        //compute objective function values

        val J2 = ((epsilon + (N*((Wnew2*data).map(e => scala.math.pow(e, 2))))).map(e => if(e > 0.0) scala.math.pow(e, 0.5) else 0.0)).sumCol
        val J1 = ((epsilon + (N*((Wnew1*data).map(e => scala.math.pow(e, 2))))).map(e => if(e > 0.0) scala.math.pow(e, 0.5) else 0.0)).sumCol
        val J0 = ((epsilon + (N*((W*data).map(e => scala.math.pow(e, 2))))).map(e => if(e > 0.0) scala.math.pow(e, 0.5) else 0.0)).sumCol

        var sumJ2 = 0.0
        var sumJ1 = 0.0
        var sumJ0 = 0.0
        for(i <- 0 until J2.length) sumJ2 = sumJ2 - J2(i)
        for(i <- 0 until J1.length) sumJ1 = sumJ1 - J1(i)
        for(i <- 0 until J0.length) sumJ2 = sumJ0 - J0(i)

        if((sumJ1 > sumJ2) && (sumJ1 > sumJ0)){
          mu = (1/changefactor)*mu
          W = Wnew1
        }
        else if((sumJ2 > sumJ1) && (sumJ2 > sumJ0)){
          mu = changefactor*mu
          W = Wnew2
        }
//        PerformanceTimer.stop("test9-" + iter)
//        PerformanceTimer.print("test9-" + iter)

      }

      //check if we have converged
      if(frobeniusNorm(grad) < .0001*topoXDim*topoYDim) notconverged=false //if norm is small enough, converge
      println("frobenius norm of gradient: " + frobeniusNorm(grad))

      PerformanceTimer.stop("mainIter" + iter)
      PerformanceTimer.print("mainIter" + iter)
    }

  }

  def frobeniusNorm(M: Matrix[Double]) : Double = {
    val prod = (M.trans)*M
    var sum = 0.0
    for(i <- 0 until prod.numRows) sum += prod(i,i) //add up trace of M'*M
    return scala.math.sqrt(sum)
  }

  def orthogonalizeRows(M: Matrix[Double]) : Matrix[Double] = {
    var vd = Matrix.eig(M*(M.trans)) //get eigenvectors and eigenvalues
    vd.force
    var v = vd(0)
    var d = vd(1)
    
    //now, finish orthogonalizing and return
    return (v*(d.map(e => if(e > 0.0) scala.math.pow(e, -0.5) else 0.0))*(v.inv))*M

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

}