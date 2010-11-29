package ppl.apps.ml.baseline.svm

import ppl.apps.ml.baseline.collection.{MLOutputWriter, MLInputReader, DoubleMatrix, DoubleVector}
import ppl.apps.ml.baseline.collection.Precursor._

class SVMModel() {
  // model data
  private var weights : DoubleVector = null
  private var alphas : DoubleVector = null
  private var b : Double = 0.0

  // helpers
  private var randConstructor : java.util.Random = null

  // construct directly from model
  def this(modelFilename : String) = {
    this()
    val in = MLInputReader.readDoubleVector(modelFilename)
    b = in(in.length-1)
    weights = in.take(in.length-1)
  }

  /////////////
  // training

  def smoTrain(X: DoubleMatrix, Y: DoubleVector, C: Double, tol: Double, max_passes: Int) = {
    println("Training SVM using the SMO algorithm")

    // intermediate training info
    alphas = DoubleVector(X.height).trans // col vector

    val numSamples = X.height
    var passes = 0

    while (passes < max_passes){
      print(".")
      var num_changed_alphas = 0
      for (i <- 0 until numSamples){
        val f_i = (alphas*Y*(X*X(i).trans)).sum + b
        val E_i = f_i - Y(i)

        if (((Y(i)*E_i < -1*tol) && (alphas(i) < C)) || ((Y(i)*E_i > tol) && (alphas(i) > 0))){
          // select a candidate j from the remaining numSamples-i samples at random
          var j : Int = Math.floor(rand*(numSamples-1)).asInstanceOf[Int]+1
          while (j == i){
            j = Math.floor(rand*(numSamples-1)).asInstanceOf[Int]+1
          }

          val f_j = (alphas*Y*(X*X(j).trans)).sum + b
          val E_j = f_j - Y(j)

          var old_aj = alphas(j)
          var old_ai = alphas(i)

          // calculate bounds L and H that must hold in order for a_i, alphas(j) to
          // satisfy constraints and check
          var L = 0.0
          var H = 0.0
          if (Y(i) != Y(j)){
            L = Math.max(0, alphas(j) - alphas(i))
            H = Math.min(C, C + alphas(j) - alphas(i))
          }else{
            L = Math.max(0, alphas(i) + alphas(j) - C)
            H = Math.min(C, alphas(i) + alphas(j))
          }

          if (L != H){
            // calculate eta
            val eta = X(i).dot(X(j))*2 - X(i).dot(X(i)) - X(j).dot(X(j))
            // check eta
            if (eta < 0){
              // compute new alphas(j)
              alphas(j) = alphas(j) - Y(j)*(E_i-E_j)/eta
              // clip alphas(j) if necessary
              if (alphas(j) > H) alphas(j) = H
              else if (alphas(j) < L) alphas(j) = L

              // check alphas(j) convergence
              if (Math.abs(alphas(j) - old_aj) >  tol){
                // find a_i to maximize objective function
                old_ai = alphas(i)
                alphas(i) = alphas(i) + Y(i)*Y(j)*(old_aj-alphas(j))

                // compute the new b such that KKT conditions are satisfied
                val old_b = b
                // TODO: this is interesting; with deferral, we can compute both b1 and b2
                // without wasting cycles on the one that actually never gets used.
                val b1 = b - E_i - X(i).dot(X(i))*Y(i)*(alphas(i)-old_ai)
                          - X(i).dot(X(j))*Y(j)*(alphas(j)-old_aj)
                val b2 = b - E_j - X(i).dot(X(j))*Y(i)*(alphas(i)-old_ai)
                          - X(j).dot(X(j))*Y(j)*(alphas(j)-old_aj)
                if ((alphas(i) > 0) && (alphas(i) < C)){
                  b = b1
                }
                if ((alphas(j) > 0) && (alphas(j) < C)){
                  b = b2
                }
                if (old_b == b){
                  // neither threshold valid
                  b = ((b1+b2)/2)
                }

                num_changed_alphas += 1
              } // alpha converged?
            } // negative eta?
          } // L != H?
        } // main if (select alphas)
      } // for i = 1 to numSamples

      if (num_changed_alphas == 0){
        passes += 1
      }else{
        passes=0;
      }
    } // while

    // SMO finished        
    print("\n")
  }

  def computeWeights(X: DoubleMatrix, Y: DoubleVector){
    // internal model storage
    weights = DoubleVector(X.width)

    // compute the weights (assuming a linear kernel)
    for (i <- 0 until X.height){
      weights = weights + X(i)*alphas(i)*Y(i)
    }
  }
  ////////////
  // testing

  def classify(test_pt : DoubleVector) : Int = {
    // SVM prediction is W'*X + b
    if ((weights.dot(test_pt) + b) < 0){
      -1
    }
    else 1
  }

  ////////////
  // utility

  def rand : Double = {
    if (randConstructor == null) randConstructor = new java.util.Random(100)
    return randConstructor.nextDouble()
  }

  def saveModel(filename : String) = {
    val out = weights.clone
    out.append(b)
    MLOutputWriter.writeDoubleVector(out, filename)
  }
}