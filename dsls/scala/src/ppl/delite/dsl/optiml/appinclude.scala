package ppl.delite.dsl.optiml

/* A collection of types and utils every Delite app using optiml should include.
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Jul 29, 2009
 * modified: Jul 29, 2009
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

import scala.reflect.ClassManifest

import ppl.delite.core._
import ppl.delite.core.ops.DeliteOP_SingleTask
import ppl.delite.core.include._
import ppl.delite.core.appinclude._
import ppl.delite.dsl.primitive._
import ppl.delite.dsl.optiml.io.MLInputReader
import ppl.delite.dsl.optiml.exception._
import ppl.delite.dsl.optiml.train.{Labels, TrainingSet}
import collection.mutable.{HashSet, ArrayBuffer, Cloneable}

object appinclude {

  ///////////////////////////
  // scala machinery

  case class Overloaded1()
  case class Overloaded2()
  case class Overloaded3()
  case class Overloaded4()
  case class Overloaded5()

  implicit val overloaded1 = new Overloaded1()
  implicit val overloaded2 = new Overloaded2()
  implicit val overloaded3 = new Overloaded3()
  implicit val overloaded4 = new Overloaded4()
  implicit val overloaded5 = new Overloaded5()

  ///////////////////////////
  // language machinery

  /**
   * random
   */

  private val INITIAL_SEED = 100
//  private class _randCls extends java.lang.ThreadLocal[scala.util.Random] {
//    override def initialValue = new scala.util.Random(INITIAL_SEED)
//  }
//  private var _randRef = new _randCls

  // these are thread-safe
  private var _randRef = new scala.util.Random(INITIAL_SEED)
  private val _intRandRef = new scala.util.Random(INITIAL_SEED)

  // this version is for optiml's use exclusively, so it does not interfere with application behavior
  private def _random[T](implicit c: ClassManifest[T]) : T =
      c match {
        case ClassManifest.Double => _intRandRef.nextDouble().asInstanceOf[T]
        case ClassManifest.Float => _intRandRef.nextFloat().asInstanceOf[T]
        case ClassManifest.Int => _intRandRef.nextInt().asInstanceOf[T]
        case ClassManifest.Long => _intRandRef.nextLong().asInstanceOf[T]
        case ClassManifest.Boolean => _intRandRef.nextBoolean().asInstanceOf[T]
        case _ => throw new UnsupportedOperationException()
    }

  // public version for application use
  def random[T](implicit c: ClassManifest[T]) : T =
      c match {
        case ClassManifest.Double => _randRef.nextDouble().asInstanceOf[T]
        case ClassManifest.Float => _randRef.nextFloat().asInstanceOf[T]
        case ClassManifest.Int => _randRef.nextInt().asInstanceOf[T]
        case ClassManifest.Long => _randRef.nextLong().asInstanceOf[T]
        case ClassManifest.Boolean => _randRef.nextBoolean().asInstanceOf[T]
        case _ => throw new UnsupportedOperationException()
    }

  def randomInt(end: Int) = _randRef.nextInt(end)
  def randomGaussian = _randRef.nextGaussian()

  def reseed {
    // reseeds for all threads
    //_randRef = new _randCls
    _randRef.setSeed(INITIAL_SEED)
    _intRandRef.setSeed(INITIAL_SEED)
  }


  /**
   * sum
   */


  // TODO: how do we handle returning tuples (e.g. for GDA's first loop)?
  // TODO:   option a) non-deferred version of sum, allows summing any non-DeliteDSLType, but returns a real result
  // TODO:   option b) DeliteTuple

  // TODO: with plugin, swap names sum and sumimpl

  // stub is replaced by impl by compiler plugin
  def sumimpl[A <: DeliteDSLType](start: Int, end: Int)(block: => A)
                             (implicit pFact: DeliteProxyFactory[A], vecPFact: DeliteProxyFactory[Vector[A]],
                              ops: ArithOps[A], c: ClassManifest[A]) : A = {
    null.asInstanceOf[A]
  }

  // TODO: how do we know we want to parallelize the sum? should we do a match on A and only parallelize if it is,
  // TODO: e.g., Matrix addition?
  def sum[A <: DeliteDSLType](start: Int, end: Int)(block: Int => A)
                             (implicit pFact: DeliteProxyFactory[A], vecPFact: DeliteProxyFactory[Vector[A]],
                                       ops: ArithOps[A], c: ClassManifest[A]) : A = {


    val numProcs = Config.CPUThreadNum

    // propagate embedded dependencies; currently we only run the plugin on the apps module
    // how could the plugin deal with "block" here, which is not known to be a DeliteFunc until runtime?
    // we could type block as a DeliteFunc, and use an implicit conversion to lift normal funcs..
    // but we don't know the dependencies of the DeliteFunc until run-time, since it varies by call-site
    val deps = block match {
      case f: DeliteFunc => f.deps
      case _ => Seq()
    }

    val coll = Vector.range(0,numProcs).map(DeliteFunc( j => {
      val chunk_st = start + end*j/numProcs
      val chunk_en = start + end*(j+1)/numProcs
      var acc = block(chunk_st)
      var i = chunk_st+1
      while(i < chunk_en) {
        ops.+=(acc, block(i))
        i += 1
      }
      acc
    }, deps: _*))

    coll.sum[A]
  }

  // TODO: is there a way we can provide primitive performance and still overload the name 'sum'?
  //       using ClassManifests doesn't work because block returns a DeliteDouble in the other case
  private val SUM_PRIMITIVE_THRESH = 1000
  def sumd(start: Int, end: Int)(block: Int => Double) : Double = {
    if ((end - start) < SUM_PRIMITIVE_THRESH) return sumd_seq(start, end)(block)

    val numProcs = Config.CPUThreadNum
    val deps = block match {
      case f: DeliteFunc => f.deps
      case _ => Seq()
    }

    val coll = Vector.range(0,numProcs).map(DeliteFunc( j => {
      val chunk_st = start + end*j/numProcs
      val chunk_en = start + end*(j+1)/numProcs
      var acc = block(chunk_st)
      var i = chunk_st+1
      while(i < chunk_en) {
        acc += block(i)
        i += 1
      }
      acc
    }, deps: _*))

    coll.sum[DeliteDouble]
  }

  def sumd_noplugin(start: Int, end: Int)(block: Int => Double) = sumd(start,end)(block)

  private def sumd_seq(start: Int, end: Int)(block: Int => Double) : Double = {
    var acc = block(start)
    var i = start+1
    while (i < end){
      acc += block(i)
      i += 1
    }
    acc
  }

  def sum(v: Vector[Double]) : Double = v.sum[DeliteDouble]
  def sum(m: Matrix[Double]) : Double = m.sum[DeliteDouble]

  /**
   * min
   */

  // sequential
  def min(vals: Double*) : Double = {
    var min = Math.MAX_DOUBLE
    var i = 0
    while (i < vals.length){
      if (vals(i) < min){
        min = vals(i)
       }
      i = i+1
    }

    min
  }

  // parallel
  def min(vals: Vector[Double]) = vals.min[DeliteDouble]
  def min(vals: Matrix[Double]) = vals.max[DeliteDouble]

  /**
   * max
   */

  def max(vals: Double*) : Double = {
    var max = Math.MIN_DOUBLE
    var i = 0
    while (i < vals.length){
      if (vals(i) > max){
        max = vals(i)
      }
    }

    max
  }

  def max(vals: Vector[Double]) = vals.max[DeliteDouble]
  def max(vals: Matrix[Double]) = vals.max[DeliteDouble]

  /**
   * avg
   */
  def avg(vals: Double*) : Double = {
    var acc = 0.0
    var i = 0
    while (i < vals.length){
      acc += vals(i)
    }

    if (vals.length == 0)
      0.0
    else
      acc / vals.length
  }

  /**
   * abs
   */
  def abs(d: Double) = Math.abs(d)
  def abs(v: Vector[Double])  = v.abs
  def abs(m: Matrix[Double]) = m.abs

  /**
   * (0::n) is an operator that returns an IndexVector, which is a Vector containing integer indices.
   * (0::n)    { } is an operator that returns a length n vector of results.
   *
   * :: is defined on some class MyRange
   * 0 is implicit converted to MyRange.:(numTokens)
   * MyRange has a method apply with parameter block : =>
   * MyRange    { block : => ... } gets mapped to MyRange( block )
   * can the rules of MyRange.apply() be more restrictive than arbitrary function... e.g. using compiler plugin analysis
   *  -- give LIMITED access to index variable (e.g. $ allowed but not $+1)
   *
   * for loops are discouraged and to be used rarely with care
   *
   */

  // Vector construction
  // ex.  (0::n) { Random.getDouble }
  // ex2. (0::n) { global($) )

  implicit def intToIndexOp(i : Int) = new IndexOp(i)
  class IndexOp(val _end : Int) {
    def ::(_start : Int) = new IndexVector(_start, _end)
  }

  // Matrix construction
  // ex.  (0::n, _) { ... }
  // ex2. (_, 0::n) { ... }

  class IndexWildcard
  val * = new IndexWildcard
  
  implicit def tuple2ToIndexVector1(tup: Tuple2[IndexVector, IndexWildcard])(implicit overloaded1 : Overloaded1) = new IndexVector2(tup._1, new IndexVectorWC)
  implicit def tuple2ToIndexVector2(tup: Tuple2[IndexWildcard, IndexVector])(implicit overloaded2 : Overloaded2) = new IndexVector2(new IndexVectorWC, tup._2)
  implicit def tuple2ToIndexVector3(tup: Tuple2[IndexVector, IndexVector])(implicit overloaded3 : Overloaded3) = new IndexVector2(tup._1, tup._2)

  /**
   * $ is a wildcard processed by the OptiML compiler. Its meaning is "SelfIndex". Since it has no arithmetic
   * operations defined, things like $+1 are not allowed (either for reading or writing). Even with $,
   * if we allow side-effects, we encourage but cannot guarantee safety (because a function could use absolute
   * indexes to write into shared global data).
   *
   * This restriction could be relaxed and remain safe(r):
   * it would be best if we could somehow allow arithmetic on $ for any non-indexing operation, but this is
   * getting pretty complicated. Let's deal with that issue when we run into it. More general, but still safe,
   * is to allow arbitrary arithmetic on $ but disallow side-effects inside the anonymous function.
   * Moreover, side-effects can be allowed (because we track dependencies), as long those side-effects do not
   * introduce INTER-CHUNK dependencies (i.e. must access a disjoint index set).
   *
   */
  class SelfIndex
  val $ = new SelfIndex

  // machinery to allow $ to pass the type checker before the compiler plugin phase
  implicit def $toInt(x: SelfIndex) : Int = null.asInstanceOf[Int]


  /**
   *   until converged
   */
  
  // ex.
  // implicit val diff : (T,T) => Double = (x:T, y:T) => Math.abs(x - y)
  // x = untilconverged(x, .05) {
  //   x => x / 2
  // }

  // TODO: do we need DeliteOP_RelaxedSingleTask? We can do the relaxations on the DSL side, and then determine which true
  //       dependencies to pass into Delite... one use for a DeliteOP_RelaxedSingleTask is if the Delite scheduler wanted
  //       to make the decision -- but it would probably be better to use a more general API for that
  private case class OP_UntilConvergedIteration[T <: DeliteDSLType](prev: T, block: T => T)
    extends DeliteOP_SingleTask[T](prev){
    
    def task = block(prev)
  }

  private case class OP_UntilConvergedDiff[T <: DeliteDSLType](prev: T, next: T, deps: Seq[T])(implicit diff: (T,T) => Double)
    extends DeliteOP_SingleTask[DeliteDouble](deps: _*){
    
    def task = diff(next, prev)
  }

  implicit val vecEucDiff : (Vector[Double], Vector[Double]) => Double = (v1,v2) => eucdist(v1, v2)
  implicit val matDiff : (Matrix[Double], Matrix[Double]) => Double = (m1,m2) => (m1-m2).abs.sum[DeliteDouble]

  /**
   * x is the value being iteratively refined
   * thresh is the allowed difference between x_old and x_new to be considered converged
   * if clone_prev_val is set to false, x_old and x_new may be the same value unless block produces a new instance
   * relax determines if multiple iterations will be in flight at once. this is only useful if block is a mutating function
   *   that can make independent progress from the same initial value.
   */
  private val RELAX_PCT = java.lang.Double.parseDouble(System.getProperty("optimlRelaxPct", "0.0"))
  private val MAX_IN_FLIGHT = 2
  def untilconverged_relaxed[T <: DeliteDSLType with Cloneable[T]](x: T, thresh: Double, max_iter: Int = 1000, clone_prev_val: Boolean = true)
                                                          (block: T => T)(implicit diff: (T,T) => Double, pFact: DeliteProxyFactory[T], c: ClassManifest[T]) : T = {

    val numProcs = if (Config.CPUThreadNum == 1) 1 else Config.CPUThreadNum / 2
    var delta = Math.MAX_DOUBLE
    var prev : T = null.asInstanceOf[T]
    var next : T = x
    var iter = 0

    while ((Math.abs(delta) > thresh) && (iter < max_iter)){
      if (clone_prev_val)
        prev = next.clone()
      else
        prev = next

      val deps = new ArrayBuffer[T](numProcs)                          
      try{
        // TODO: block is not being checked for dependencies by the plugin
        next = Delite.run(OP_UntilConvergedIteration(prev, block))//block(next)
        deps += prev
        deps += next
        
        // keep n iterations in flight in between barriers
        // note that in this implementation, we *always* relax num_procs-1 dependencies per iteration
        var n = 1
        while (n < numProcs && n < MAX_IN_FLIGHT){
          if (_random[Double] < RELAX_PCT){
            deps += Delite.run(OP_UntilConvergedIteration(prev, block))
          }
          n += 1
        }
      }
      catch{
        case e: Exception => throw new ConvergenceException("Converging block threw exception: " + e)
      }
      iter += 1

      delta = Delite.run(OP_UntilConvergedDiff(next, prev, deps))//diff(next, prev)
      //println("(" + delta + ")")
    }

    if (iter == max_iter){
      throw new ConvergenceException("Maximum iterations exceeded")
    }

    next
  }

  def untilconverged[T <: DeliteDSLType with Cloneable[T]](x: T, thresh: Double, max_iter: Int = 1000, clone_prev_val: Boolean = true)
                                                          (block: T => T)(implicit diff: (T,T) => Double) : T = {

    val numProcs = Config.CPUThreadNum
    var delta = Math.MAX_DOUBLE
    var prev : T = null.asInstanceOf[T]
    var next : T = x
    var iter = 0

    while ((Math.abs(delta) > thresh) && (iter < max_iter)){
      if (clone_prev_val)
        prev = next.clone()
      else
        prev = next

      try{
        next = block(next)
      }
      catch{
        case e: Exception => throw new ConvergenceException("Converging block threw exception: " + e)
      }
      iter += 1
      delta = diff(next, prev)
      //println("(" + delta + ")")
    }

    if (iter == max_iter){
      throw new ConvergenceException("Maximum iterations exceeded")
    }

    next
  }

  private val MIN_BATCH_PROCS = 4
  def gradient(x: TrainingSet[Double], y: Labels[Double], alpha: Double = .001, thresh: Double = .0001,
                 max_iter: Int = 10000)(hyp: Vector[Double] => Double) : Vector[Double] = {

    val numProcs = Config.CPUThreadNum
    if (numProcs < MIN_BATCH_PROCS){
      stochastic(x, y, alpha, thresh, max_iter)(hyp)
    }
    else{
      batch(x, y, alpha, thresh, max_iter)(hyp)
    }                                   
  }

  // stochastic: block() updates every jth parameter for every ith training sample
  //    while not converged{
  //      for i from 0 until m
  //        for j from 0 until n
  //            updatej(i,j)
  // hypothesis function maps a training example to a prediction

  // stochastic can only be parallelized across features, which is generally << samples
  def stochastic(x: TrainingSet[Double], y: Labels[Double], alpha: Double = .001, thresh: Double = .0001,
                 max_iter: Int = 10000)(hyp: Vector[Double] => Double) : Vector[Double] = {

    val theta = Vector.zeros(x.numFeatures)
    untilconverged(theta, thresh, max_iter) {
      theta => {
      for (i <- 0 until x.numSamples) {
        for (j <- 0 until x.numFeatures ) {
          theta(j) = theta(j) + alpha*(y(i) - hyp(x(i)))*x(i)(j)
        }
      }
      theta
      }
    }
  }

  // batch: block() updates each jth parameter from the sum of all ith training samples
  //    while not converged{
  //      for j from 0 until n
  //        j_update = sum((y(i) - h(x(i))*x(j,i)
  //        updatej(j_update)

  // in batch, the sum(...) loops over the entire training set independently, which is where the parallelism comes from
  // batch can be parallized across samples
  def batch(x: TrainingSet[Double], y: Labels[Double], alpha: Double = .001, thresh: Double = .0001,
            max_iter: Int = 10000)(hyp: Vector[Double] => Double) : Vector[Double] = {

    val theta = Vector.zeros(x.numFeatures)
    untilconverged(theta, thresh, max_iter) {
      theta => {
      for (j <- 0 until x.numFeatures) {
        val acc = sumd(0, x.numSamples) { i => {
          (y(i) - hyp(x(i))*x(i)(j))   // parallel work
        }}
        theta(j) = theta(j) + alpha*acc
      }
      theta
      }
    }
  }

  // coordinate ascent: analogous to stochastic gradient descent, but updates m parameters (alphas(0)...alphas(m-1))
  // at each update, all but alpha(i) must be held constant, so there are dependencies between every iteration

  //Loop until convergence {
  // For i = 1, . . . ,m, {
  //   alphas(i) := arg max alpha^(i) W(alphas(0), . . . , alphas(i-1), alphas^(i), alphas(i+1), . . . , alphas(m-1))
  // }
  //}

  /**
   *  input parsing
   */

  def loadMatrix(filename: String) = MLInputReader.read(filename)
  def loadVector(filename: String) = MLInputReader.readVector(filename)
  def loadTrainingSet(filename: String) = TrainingSet(MLInputReader.read(filename))
  //def loadTestSet(filename: String) = TestSet(MLInputReader.read(filename))


  /**
   * utilities
   */

  /* Maybe the best place for these are inside Matrix/Vector. Is there
   * any advantage to having them here from a productivity, expressivity, or optimization stand point? */

  // projection of of higher dimensional input to lower dimension
  //def project[A](m: Matrix[A]) : Matrix[A]

  // sampling of input to reduce data size
  def sample[A : ClassManifest](m: Matrix[A], numSamples: Int, sampleRows: Boolean = true, method: String = "random") : Matrix[A] = {
    method match {
      case "random" => randsample(m, numSamples, sampleRows)
      case _ => throw new UnsupportedOperationException("unknown sampling type selected")
    }
  }
  //def sample[A](v: Vector[A]) : Vector[A]

  def randsample[A : ClassManifest](m: Matrix[A], numSamples: Int, sampleRows: Boolean) : Matrix[A] = {
    val length = if (sampleRows) m.numRows else m.numCols
    val newRows = if (sampleRows) numSamples else m.numRows
    val newCols = if (sampleRows) m.numCols else numSamples

    val sampled = if(sampleRows) Matrix[A](0, newCols)
                  else Matrix[A](0,newRows) // transposed for efficiency

    val candidates = Vector.mrange(0, length)

    // transpose to make constructing sampling more efficient
    val mt = if (sampleRows) m else m.trans

    for (i <- 0 until numSamples){
      val r = i + randomInt(length-i)
      val idx = candidates(r)
      sampled += mt(idx).clone

      // remove index r from consideration
      val t = candidates(r)
      candidates(r) = candidates(i)
      candidates(i) = t            
    }

    if (sampleRows) sampled else sampled.trans
  }

  // interpolation of input to increase data size
  //def interpolate[A](m: Matrix[A]) : Matrix[A]
  //def interpolate[A](v: Vector[A]) : Vector[A]

  /**
   * distance
   */
  def dist(v1: Vector[Double], v2: Vector[Double], metric: String = "abs") : Double = {
    metric match {
      case "abs" => absdist(v1, v2)
      case "euc" => eucdist(v1, v2)
      case _ => throw new UnsupportedOperationException("unknown dist metric selected")
    }
  }

  def distm(m1: Matrix[Double], m2: Matrix[Double], metric: String = "abs") : Matrix[Double] = {

    // the layout representation is res(i,j) = dist(m1(i),m2(j))
    if (m1.numCols != m2.numCols) throw new IllegalArgumentException

    val out : Matrix[Double] = Matrix[Double](m1.numRows, m2.numRows)

    (0::m1.numRows){ i=>
      (0::m2.numRows){ j=>
        out(i,j) = dist(m1(i), m2(j), metric)
      }
    }
    out
  }

  def absdist(v1: Vector[Double], v2: Vector[Double]) : Double = (v1-v2).abs.sum[DeliteDouble]
  def eucdist(v1: Vector[Double], v2: Vector[Double]) : Double = Math.sqrt((v1-v2).map(e => Math.pow(e, 2)).sum[DeliteDouble])

  /**
   * nearest neighbor
   *
   * returns the nearest neighbor that is NOT an exact match
   */
  def nearest_neighbor(v: Vector[Double], m: Matrix[Double]) : Vector[Double] = {
    val mt = if (v.is_row) m else m.trans

    val dists = mt.mapRowsToVec(row => {
      val d = dist(row,v)
      if (d == 0.0) Math.MAX_DOUBLE else d
    })

    val minIdx = dists.minIndex
    if (v.is_row) mt(minIdx) else mt(minIdx).trans
  }
}
