package ppl.apps.ml.svm

/* SVMBatchModel implements a batch gradient descent solution to the primal SVM optimization
 * problem. It is based on the implementation in Chu et. al., Map Reduce for Machine Learning on Multicore.
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Jul 13, 2010
 * modified: Jul 13, 2020
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml.{Matrix, Vector}
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.dsl.optiml.io.MLOutputWriter
import ppl.delite.dsl.primitive.DeliteDouble
import ppl.delite.core.{DeliteFunc, Delite}

class SVMBatchModel() {
  // model data
  private var weights : Vector[Double] = null
  private var b : Double = 1.0

  // construct directly from model
  def this(modelFilename : String) = {
    this()
    Delite.init = true
    val in = loadVector(modelFilename)
    Delite.init = false
    b = in(in.length-1)
    weights = in.take(in.length-1)
  }

  /////////////
  // training

  def train(X: Matrix[Double], Y: Vector[Double], C: Double, tol: Double, max_passes: Int) = {
    println("Training SVM using the batch gradient descent algorithm")

    // internal model storage
    weights = Vector.ones(X.numCols)

    // intermediate training info
    val alpha = 0.1

    for (n <- 0 until 10){
      print(".")
      val weights2 = weights
      var result = Vector.zeros(X.numCols)
      var b_derivative = 0.0

      for (i <- 0 until X.numRows){
      //val result = sumimpl(0, X.numRows) { i =>
        //var acc = null.asInstanceOf[Vector[Double]]
        val wx : Double = weights2.dot(X(i))
        println("Y(i): " + Y(i) + " | wx: " + wx + " | Y(i) * (wx + b): " + Y(i)*(wx + b))
        if (Y(i) * (wx + b) < 1) {
          // the label should be +1 or -1
          if (Y(i) == 1) {
            //result = result + X(i)
            result = result + (X(i) - 1)
            //acc = X(i)
          }
          else if (Y(i) == -1){
            //result = result - X(i)
            result = result + (X(i) + 1)*(-1)
            //acc = X(i)*(-1)
          }
          else {
            throw new IllegalArgumentException("SVMBatchModel: found label that was not -1 or 1")
          }
          b_derivative += Y(i)
        }
        else {
          //acc = Vector.zeros(X.numCols)
        }
        //acc
      }

//      var b_derivative : Double = sumimpl[DeliteDouble](0, X.numRows) { i =>
//        val wx = X(i).dot(weights2)
//        if (Y(i) * (wx + b) < 1) {
//          Y(i)
//        }
//        else {
//          0.0
//        }
//      }

      // sequential
      val w_derivative = (result*C+weights2)*alpha
      b_derivative *= C
      b_derivative *= alpha
      weights = (weights2 - w_derivative).force.asInstanceOf[Vector[Double]]
      b -= b_derivative

      println("weights: "); weights.pprint
      println("b: " + b)
    }

    print("\n")
  }

  ////////////
  // testing

  def classify(test_pt : Vector[Double]) : Int = {
    // SVM prediction is W'*X + b
    val wx : Double = weights.dot(test_pt)
    println("TEST: " + " | wx: " + wx + " | (wx + b): " + (wx + b))

    if ((wx + b) < 0){
      -1
    }
    else 1
  }

  ////////////
  // utility

  def saveModel(filename : String) = {
    Delite.init = true
    val out = weights.mutableClone
    out += b
    MLOutputWriter.writeVector(out, filename)
    Delite.init = false
  }
}