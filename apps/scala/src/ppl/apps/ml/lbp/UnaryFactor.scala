package ppl.apps.ml.lbp

/**
 * Created by IntelliJ IDEA.
 * User: mmwu
 * Date: Dec 6, 2010
 * Time: 5:28:12 PM
 * To change this template use File | Settings | File Templates.
 */

object Factor {
}

class UnaryFactor(var v: Int, val arity: Int) {
  val data = Array.fill(arity)(0.0)

  val logP = new {
    def apply(i: Int) = data(i)
    def update(i: Int, d: Double) = {data(i) = d}
  }

  def copy(newV: Int = v) : UnaryFactor = {
    val uf = new UnaryFactor(newV, arity)
    Array.copy(data, 0, uf.data, 0, arity)
    uf
  }

  def uniform(value: Double = 0.0) = {
      for(asg <- 0 until arity)
        logP(asg) = value;
  }

  def normalize() = {
    assert(arity > 0)

    val max = data.reduceLeft((a: Double, b: Double) => if(a > b) a else b)

    // Scale and compute normalizing constant
    var Z = 0.0
    for(asg <- 0 until arity) {
      Z += Math.exp(logP(asg) - max)
      logP(asg) -= - max
    }

    assert(Z > 0)
    var logZ = Math.log(Z)

    for(asg <- 0 until arity) {
      logP(asg) -= logZ
    }
  }

  def times(other: UnaryFactor) = {
    assert(arity == other.arity)

    for(asg <- 0 until arity) {
      logP(asg) += other.logP(asg)
    }
  }

  def plus(other: UnaryFactor) = {
    assert(arity == other.arity)

    for(asg <- 0 until arity) {
      logP(asg) = Math.log(Math.exp(logP(asg)) + Math.exp(other.logP(asg)))
    }
  }

  def divide(other: UnaryFactor) = {
    assert(arity == other.arity)

    for(asg <- 0 until arity) {
      logP(asg) -= other.logP(asg)
    }
  }

  def convolve(bin_fact: BinaryFactor, other: UnaryFactor) : Unit = {
    for(x <- 0 until arity) {
      var sum = 0.0

      for(y <- 0 until other.arity) {
        sum += Math.exp(bin_fact.logP(v, x, other.v, y)) + other.logP(y)
      }

      assert( !(sum < 0) )
      // Guard against zeros
      if(sum == 0)
        sum = Double.MinValue

      logP(x) = Math.log(sum)
    }
  }

  def condition(bin_fact: BinaryFactor, asg: Int) = {
    val other_var = if(v == bin_fact.v1) bin_fact.v2 else bin_fact.v1

    for(x <- 0 until arity) {
      logP(x) += bin_fact.logP(v, x, other_var, asg)
    }
  }

  /** This = other * damping + this * (1-damping) */
  def damp(other: UnaryFactor, damping: Double) = {
    assert(arity == other.arity)
    if(damping != 0) {
      assert(damping >= 0)
      assert(damping < 1)

      for(asg <- 0 until arity) {
        logP(asg) = Math.log(damping * Math.exp(other.logP(asg)) +
          (1.0 - damping) * Math.exp(logP(asg)))
      }
    }
  }

  /** Compute the residual between two unary factors */
  def residual(other: UnaryFactor) : Double = {
    assert(arity == other.arity)
    var residual = 0.0
    for(asg <- 0 until arity) {
      residual += Math.abs(Math.exp(logP(asg)) -
        Math.exp(other.logP(asg)))
    }
    residual / arity
  }

  def expectation : Double = {
    var sum = 0.0
    var s2 = 0.0

    for(asg <- 0 until arity) {
      sum += asg * Math.exp(logP(asg))
      s2 += Math.exp(logP(asg))
    }

    sum / s2
  }
}
