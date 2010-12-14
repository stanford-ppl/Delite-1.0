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

  def copy(newV: Int = v): UnaryFactor = {
    val uf = new UnaryFactor(newV, arity)
    Array.copy(data, 0, uf.data, 0, arity)
    uf
  }

  def uniform(value: Double = 0.0) = {
    var asg = 0
    while (asg < arity) {
      data(asg) = value;
      asg += 1
    }
  }

  def normalize() = {
    assert(arity > 0)

    val max = data.reduceLeft((a: Double, b: Double) => Math.max(a, b))

    // Scale and compute normalizing constant
    var Z = 0.0

    var asg = 0
    while (asg < arity) {
      Z += Math.exp(data(asg) - max)
      data(asg) -= max
      asg += 1
    }

    assert(Z > 0)
    var logZ = Math.log(Z)

    asg = 0
    while (asg < arity) {
      data(asg) -= logZ
      asg += 1
    }
  }

  // Multiply elementwise by other factor
  def times(other: UnaryFactor) = {
    assert(arity == other.arity)

    var asg = 0
    while (asg < arity) {
      data(asg) += other.data(asg)
      asg += 1
    }
  }

  // Add other factor elementwise
  def plus(other: UnaryFactor) = {
    assert(arity == other.arity)

    var asg = 0
    while (asg < arity) {
      data(asg) = Math.log(Math.exp(data(asg)) + Math.exp(other.data(asg)))
      asg += 1
    }
  }

  // Divide elementwise by other factor
  def divide(other: UnaryFactor) = {
    assert(arity == other.arity)

    var asg = 0
    while (asg < arity) {
      data(asg) -= other.data(asg)
      asg += 1
    }
  }

  // this(x) = sum_y fact(x,y) * other(y)
  def convolve(bin_fact: BinaryFactor, other: UnaryFactor): Unit = {
    for (x <- 0 until arity) {
      var sum = 0.0

      for (y <- 0 until other.arity) {
        sum += Math.exp(bin_fact.data(bin_fact.index4(v, x, other.v, y)) + other.data(y))
      }

      assert(!(sum < 0))
      // Guard against zeros
      if (sum == 0)
        sum = Double.MinValue

      data(x) = Math.log(sum)
    }
  }

  def condition(bin_fact: BinaryFactor, asg: Int) = {
    val other_var = if (v == bin_fact.v1) bin_fact.v2 else bin_fact.v1

    var x = 0
    while (x < arity) {
      data(x) += bin_fact.data(bin_fact.index4(v, x, other_var, asg))
      x += 1
    }
  }

  /**This = other * damping + this * (1-damping) */
  def damp(other: UnaryFactor, damping: Double) = {
    assert(arity == other.arity)
    if (damping != 0) {
      assert(damping >= 0)
      assert(damping < 1)

      var asg = 0
      while (asg < arity) {
        data(asg) = Math.log(damping * Math.exp(other.data(asg)) +
                (1.0 - damping) * Math.exp(data(asg)))
        asg += 1
      }
    }
  }

  /**Compute the residual between two unary factors */
  def residual(other: UnaryFactor): Double = {
    assert(arity == other.arity)
    var residual = 0.0
    var asg = 0
    while (asg < arity) {
      residual += Math.abs(Math.exp(data(asg)) -
              Math.exp(other.data(asg)))
      asg += 1
    }
    residual / arity
  }

  // Max assignment
  def max_asg() = {
    var max_asg = 0;
    var max_value = data(0);

    var asg = 0
    while (asg < arity) {
      if (data(asg) > max_value) {
        max_value = data(asg)
        max_asg = asg
      }
      asg += 1
    }

    max_asg
  }

  def expectation: Double = {
    var sum = 0.0
    var s2 = 0.0

    var asg = 0
    while (asg < arity) {
      sum += asg * Math.exp(data(asg))
      s2 += Math.exp(data(asg))
      asg += 1
    }

    sum / s2
  }

  override def toString(): String = {
    v + " " + data.mkString(" ")
  }
}
