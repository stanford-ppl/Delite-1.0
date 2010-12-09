package ppl.apps.ml.lbp

/**
 * Created by IntelliJ IDEA.
 * User: mmwu
 * Date: Dec 6, 2010
 * Time: 5:42:56 PM
 * To change this template use File | Settings | File Templates.
 */

/**
 * A binary factor is a table over a pair of variables and is
 * associated with each edge in a pairwise markov random field.  All
 * data is represented in log form.
 */
class BinaryFactor(val v1: Int = 0, val arity1: Int = 0, val v2: Int = 0, val arity2: Int = 0) {
  val data = Array.fill(arity1 * arity2)(0.0)

  val logP = new {
    def index4(x1: Int, asg1: Int, x2: Int, asg2: Int): Int = {
      assert(asg1 < arity1)
      assert(asg2 < arity2)

      // Not symmetric
      if (v1 != v2) {
        assert((x1 == v1 && x2 == v2) || (x2 == v1 && x1 == v2))
        // Swap factors
        if (x1 == v2 && x2 == v1) {
          return asg2 + asg1 * arity1
        }
      }

      asg1 + asg2 * arity1
    }
    
    def apply(x1: Int, asg1: Int, x2: Int, asg2: Int) = {
      data(index4(x1, asg1, x2, asg2))
    }

    def update(x1: Int, asg1: Int, x2: Int, asg2: Int, v: Double) = {
      data(index4(x1, asg1, x2, asg2)) = v
    }

    def apply(asg1: Int, asg2: Int) = {
      data(asg1 + asg2 * arity1)
    }

    def update(asg1: Int, asg2: Int, v: Double) = {
      data(asg1 + asg2 * arity1) = v
    }
  }

  def setAgreement(lambda: Double) = {
    for (i <- 0 until arity1) {
      for (j <- 0 until arity2) {
        if (i != j)
          logP(i, j) = -lambda
        else
          logP(i, j) = 0
      }
    }
  }

  def setLaplace(lambda: Double) = {
    for (i <- 0 until arity1) {
      for (j <- 0 until arity2) {
        logP(i, j) = -Math.abs(i - j) * lambda;
      }
    }
  }

  override def toString() : String = {
    (0 until arity1).map((i) => {
      (0 until arity2).map((j) => logP(i, j)).mkString(" ")
    }).mkString("\n")
  }
}