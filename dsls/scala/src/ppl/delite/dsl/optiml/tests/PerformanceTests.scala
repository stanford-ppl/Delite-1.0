package ppl.delite.dsl.optiml.tests

import ppl.delite.core.appinclude._
import ppl.delite.metrics.PerformanceTimer
import ppl.delite.dsl.optiml.{Vector, Matrix, SparseMatrix}
import ppl.delite.dsl.optiml.specialized.{DoubleMatrix, DoubleSparseMatrix}
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.core.{Delite, DeliteApplication}
import collection.mutable.ArrayBuffer

/* Micro-performance tests for OptiML ops in Delite.
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: Jul 14, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

object PerformanceTests extends DeliteApplication {
  def run(args: Array[String]) = {
    for (i <- 1 until 11) {
      println("beginning iteration " + i )
      println("==========================================================")
      SMtimesSM(1000, 1000, 20, .1)
      //DMtimesSM(1000, 1000, 20, .1)
      //SMtimesDM(1000, 1000, 20, .1)
      //DMtimesDM(1000, 1000, 20, .1)
    }
  }



  def SMtimesSM(rows: Int, cols: Int, repeat: Int, nnz : Double) {
    Delite.init = true

    val a2 = sparseMatrixSetNNZ(rows, cols, nnz).asInstanceOf[DoubleSparseMatrix]
    val b2 = sparseMatrixSetNNZ(cols, rows, nnz).asInstanceOf[DoubleSparseMatrix]
    println("SMs ready, SM*SM at " + nnz + " sparsity starting")

    Delite.init = false

    var out2 = null.asInstanceOf[Matrix[Double]]
    PerformanceTimer.start("SM*SM", false)
    for (i <- 0 until repeat){
      out2 = a2 * b2
      out2.force
    }    
    PerformanceTimer.stop("SM*SM", false)
    PerformanceTimer.print("SM*SM")
  }

  def DMtimesSM(rows: Int, cols: Int, repeat: Int, nnz: Double) {

    Delite.init = true

    val a1 = sparseMatrixSetNNZ(cols, rows, nnz).asInstanceOf[DoubleSparseMatrix]
    val b1 = Matrix.randn(rows, cols).asInstanceOf[DoubleMatrix]
    println("SM ready, DM*SM at " + nnz + " sparsity starting")

    Delite.init = false

    var out1 = null.asInstanceOf[Matrix[Double]]
    PerformanceTimer.start("DM*SM", false)
    for (i <- 0 until repeat){
      out1 = b1 * a1
      out1.force
    }
    PerformanceTimer.stop("DM*SM", false)
    PerformanceTimer.print("DM*SM")
  }

  def SMtimesDM(rows: Int, cols: Int, repeat: Int, nnz: Double) {

    Delite.init = true

    val a1 = sparseMatrixSetNNZ(rows, cols, nnz).asInstanceOf[DoubleSparseMatrix]
    val b1 = Matrix.randn(cols, rows).asInstanceOf[DoubleMatrix]
    println("SM ready, SM*DM at " + nnz + " sparsity starting")

    Delite.init = false

    var out1 = null.asInstanceOf[Matrix[Double]]
    PerformanceTimer.start("SM*DM", false)
    for (i <- 0 until repeat){
      out1 = a1 * b1
      out1.force
    }
    PerformanceTimer.stop("SM*DM", false)
    PerformanceTimer.print("SM*DM")
  }

  def DMtimesDM(rows: Int, cols: Int, repeat: Int, nnz: Double) {

    Delite.init = true

    val a1 = Matrix.randn(rows, cols).asInstanceOf[DoubleMatrix]
    val b1 = Matrix.randn(cols, rows).asInstanceOf[DoubleMatrix]
    println("starting DM baseline")

    Delite.init = false

    var out1 = null.asInstanceOf[Matrix[Double]]
    PerformanceTimer.start("DM*DM", false)
    for (i <- 0 until repeat){
      out1 = a1 * b1
      out1.force
    }
    PerformanceTimer.stop("DM*DM", false)
    PerformanceTimer.print("DM*DM")
  }

  def compareToSparseMatrixOps(rows: Int, cols: Int, repeat: Int, nnzs: Int = 0) {
    var a2 = Matrix.zeros(rows, cols).asInstanceOf[DoubleMatrix]
    a2.force
    val smaller = if(rows < cols) rows else cols
    for (i <- 0 until smaller){
      a2(i, i) = 0.5
    }    
    val b2 = Matrix.randn(cols, rows).asInstanceOf[DoubleMatrix]

    var out2 = null.asInstanceOf[Matrix[Double]]
    PerformanceTimer.start("Sparse Matrix * Matrix", false)
    for (i <- 0 until repeat){
      out2 = a2 * b2
      out2.force
    }
    PerformanceTimer.stop("Sparse Matrix * Matrix", false)
    PerformanceTimer.print("Sparse Matrix * Matrix")

    val a1 = Matrix.zeros(cols, rows).asInstanceOf[DoubleMatrix]
    a1.force
    for (i <- 0 until smaller){
      a2(i, i) = 1
    }
    val b1 = Matrix.randn(rows, cols).asInstanceOf[DoubleMatrix]

    var out1 = null.asInstanceOf[Matrix[Double]]
    PerformanceTimer.start("Matrix * Sparse Matrix", false)
    for (i <- 0 until repeat){
      out1 = b1 * a1
      out1.force
    }
    PerformanceTimer.stop("Matrix * Sparse Matrix", false)
    PerformanceTimer.print("Matrix * Sparse Matrix")
  }

  def sparseMatrixSetNNZ(rows: Int, cols: Int, nnz: Double) : SparseMatrix[Double] = {
    val toReturn = SparseMatrix.zeros(rows,cols)
    val rowNNZcount = (((rows*cols)*nnz)/rows).intValue

    for(r <- 0 until rows){
      var nums = new ArrayBuffer[Int]()

      var idx = 0
      while(idx < cols){
        nums.append(idx)
        idx += 1
      }

      for(n <- 0 until rowNNZcount){
        val pos = (scala.math.random*nums.size).toInt //position to remove
        val rand = nums(pos) //the random value still left to pick from the training data
        toReturn(r, rand) = scala.math.random
        nums.remove(pos)
      }
    }

    return toReturn
  }

  def testVectorOps(length: Int) {
    val a = Vector.rand(length)
    val b = Vector.rand(length)

    PerformanceTimer.start("Vector Add", false)
    val c = (a+b).force
    PerformanceTimer.stop("Vector Add", false)
    PerformanceTimer.print("Vector Add")
  }

  def testMatrixOps(rows: Int, cols: Int) {
    val a2 = Matrix.ones(rows, cols)
    val b2 = Matrix.ones(rows, cols)
    val f : Matrix[Double] => Matrix[Double] = (e => (e + 1))

    PerformanceTimer.start("Matrix Add - ZipWith", false)
    val x = f(a2).asInstanceOf[DoubleMatrix]
    (x plusEqualsZipWith f(b2).asInstanceOf[DoubleMatrix])
    x.force
    PerformanceTimer.stop("Matrix Add - ZipWith", false)
    PerformanceTimer.print("Matrix Add - ZipWith")

    val a1 = Matrix.ones(rows, cols).asInstanceOf[DoubleMatrix]
    val b1 = Matrix.ones(rows, cols).asInstanceOf[DoubleMatrix]

    PerformanceTimer.start("Matrix Add - SingleTask", false)
    val y = f(a1).asInstanceOf[DoubleMatrix]
    (y plusEqualsSingle f(b1).asInstanceOf[DoubleMatrix])
    y.force
    PerformanceTimer.stop("Matrix Add - SingleTask", false)
    PerformanceTimer.print("Matrix Add - SingleTask")
  }
}
