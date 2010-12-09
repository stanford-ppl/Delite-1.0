package ppl.apps.ml.lbp

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 12/08/10
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 * Ported from GraphLab
 *
 * A unary factor is a table over a single variable and is associated
 * with edge variable in the pairwise markov random field.  Unary
 * factors are also used to represent messages. All data is
 * represented in log form.
 */

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

    val max = data.reduceLeft((a: Double, b: Double) => Math.max(a, b))

    // Scale and compute normalizing constant
    var Z = 0.0
    for(asg <- 0 until arity) {
      Z += Math.exp(logP(asg) - max)
      logP(asg) -= max
    }

    assert(Z > 0)
    var logZ = Math.log(Z)

    for(asg <- 0 until arity) {
      logP(asg) -= logZ
    }
  }

  // Multiply elementwise by other factor
  def times(other: UnaryFactor) = {
    assert(arity == other.arity)

    for(asg <- 0 until arity) {
      logP(asg) += other.logP(asg)
    }
  }

  // Add other factor elementwise
  def plus(other: UnaryFactor) = {
    assert(arity == other.arity)

    for(asg <- 0 until arity) {
      logP(asg) = Math.log(Math.exp(logP(asg)) + Math.exp(other.logP(asg)))
    }
  }

  // Divide elementwise by other factor
  def divide(other: UnaryFactor) = {
    assert(arity == other.arity)

    for(asg <- 0 until arity) {
      logP(asg) -= other.logP(asg)
    }
  }

  // this(x) = sum_y fact(x,y) * other(y)
  def convolve(bin_fact: BinaryFactor, other: UnaryFactor) : Unit = {
    for(x <- 0 until arity) {
      var sum = 0.0

      for(y <- 0 until other.arity) {
        sum += Math.exp(bin_fact.logP(v, x, other.v, y) + other.logP(y))
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
