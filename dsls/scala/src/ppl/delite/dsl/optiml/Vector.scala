/* Vector trait
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * modified: 2/23/10
 * credits:  Nathan Bronson (from PSeq.scala, DoublePSeq.scala)
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

package ppl.delite.dsl.optiml

import scala.reflect.ClassManifest
import scala.collection.mutable.Cloneable

import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.core.appinclude._
import ppl.delite.core.ops._
import ppl.delite.dsl.primitive._
import ppl.delite.core._
import ppl.delite.dsl.optiml.specialized._
import ppl.delite.cuda._

object Vector {

  ///////////////////////
  // user visible methods

  // weirdly, because of the signature Vector.apply(len: Int), apply(xs: A*) won't match if A is Int
  def apply[A : ClassManifest](len: Int) : Vector[A] = newVector[A](len)
  def apply[A : ClassManifest](xs: A*) : Vector[A] = newVectorFromSeq(xs)
  def apply[A : ClassManifest](is_row: Boolean, len: Int) : Vector[A] = newVector[A](len, is_row)
  def apply[A : ClassManifest](is_row: Boolean, xs: A*) : Vector[A] = newVectorFromSeq(xs, is_row)

  // special apply method for GPU use (Generate Impl with given array and given length 
  def apply[A : ClassManifest](xs: Array[A], is_row: Boolean, len: Int) : Vector[A] = newVector[A](xs, len, is_row)

  def fromSeq[A : ClassManifest](xs: Seq[A]) = newVectorFromSeq(xs)


  def flatten[A](pieces: Vector[Vector[A]])
    (implicit pfact: Vector.ProxyFactory[A], c: ClassManifest[A]): Vector[A] = {
    Delite.run(OP_flatten(pieces))
  }

  def ones(length: Int) : Vector[Double] = {
    Delite.run(OP_ones(length))
  }

  def zeros(length: Int) : Vector[Double] = {
    Delite.run(OP_zeros(length))
  }

  def zerosf(length: Int) : Vector[Float] = {
    Delite.run(OP_zerosf(length))
  }

  /* Generate a pseudo-random vector uniformly distributed on [0,1) */
  def rand(length: Int) : Vector[Double] = {
    Delite.run(OP_rand(length))
  }

  def randn(length: Int) : Vector[Double] = {
    Delite.run(OP_randn(length))
  }

  // TODO: right now there are only 2 distinctions between uniform and range:
  //       1) uniform defers, and range doesn't (why?)
  //       2) uniform returns doubles and range returns ints (need to check if this makes a substantial difference in terms of efficiency)
  def uniform(start: Double, step_size: Double, end: Double) : Vector[Double] = {
    Delite.run(OP_uniform(start, step_size, end))
  }

  def range(start: Int, end: Int, stride: Int = 1, is_row: Boolean = true) : Vector[Int] = {
    if ((end < start) || (stride < 1)) throw new IllegalArgumentException

    new RangeVector(start, end, stride, is_row)
  }

  def mrange(start: Int, end: Int, stride: Int = 1, is_row: Boolean = true) : Vector[Int] = {
    if ((end < start) || (stride < 1)) throw new IllegalArgumentException

    val v = Vector[Int](is_row, end - start)
    var i = start
    var j = 0
   
    while (i < end){
      v(j) = i
      i += stride
      j += 1
    }
    v
 }

  //////////////////////
  // vector construction

  private def emptyVector[A : ClassManifest]() : Vector[A] = newVector(0)

  private def newVector[A : ClassManifest](xs: Vector[A]): Vector[A] = { newVector[A](0, xs.is_row) ++= xs }

  private def newVector[A](len: Int, is_row: Boolean = true)(implicit m: ClassManifest[A]): Vector[A] = {
    m match {
      case ClassManifest.Double => new DoubleVectorImpl(is_row, len).asInstanceOf[Vector[A]]
      case ClassManifest.Float => new FloatVectorImpl(is_row, len).asInstanceOf[Vector[A]]
      case ClassManifest.Int => new IntVectorImpl(is_row, len).asInstanceOf[Vector[A]]
      case ClassManifest.Long => new LongVectorImpl(is_row, len).asInstanceOf[Vector[A]]
      case ClassManifest.Boolean => new BooleanVectorImpl(is_row, len).asInstanceOf[Vector[A]]
      case _ => new VectorImpl[A](is_row, len)
    }
  }

  private def newVector[A](implicit m: ClassManifest[A]): Vector[A] = {
    m match {
      case ClassManifest.Double => new DoubleVectorImpl().asInstanceOf[Vector[A]]
      case ClassManifest.Float => new FloatVectorImpl().asInstanceOf[Vector[A]]
      case ClassManifest.Int => new IntVectorImpl().asInstanceOf[Vector[A]]
      case ClassManifest.Long => new LongVectorImpl().asInstanceOf[Vector[A]]
      case ClassManifest.Boolean => new BooleanVectorImpl().asInstanceOf[Vector[A]]
      case _ => new VectorImpl[A]()
    }
  }

  private def newVector[A](xs: Array[A], len: Int, is_row: Boolean)(implicit m: ClassManifest[A]): Vector[A] = {
    m match {
      case ClassManifest.Double => new DoubleVectorImpl(xs.asInstanceOf[Array[Double]], is_row, len).asInstanceOf[Vector[A]]
	    case ClassManifest.Float => new FloatVectorImpl(xs.asInstanceOf[Array[Float]], is_row, len).asInstanceOf[Vector[A]]
      case ClassManifest.Int => new IntVectorImpl(xs.asInstanceOf[Array[Int]], is_row, len).asInstanceOf[Vector[A]]
      case ClassManifest.Long => new LongVectorImpl(xs.asInstanceOf[Array[Long]], is_row, len).asInstanceOf[Vector[A]]
      case ClassManifest.Boolean => new BooleanVectorImpl(xs.asInstanceOf[Array[Boolean]], is_row, len).asInstanceOf[Vector[A]]
      case _ => new VectorImpl[A]()
    }
  }

  private def newVectorView[A](implicit m: ClassManifest[A]): Vector[A] = {
    m match {
      case ClassManifest.Double => new DoubleVectorViewImpl().asInstanceOf[Vector[A]]
      case ClassManifest.Float => new FloatVectorViewImpl().asInstanceOf[Vector[A]]
      case ClassManifest.Int => new IntVectorViewImpl().asInstanceOf[Vector[A]]
      case ClassManifest.Long => new LongVectorViewImpl().asInstanceOf[Vector[A]]
      case ClassManifest.Boolean => new BooleanVectorViewImpl().asInstanceOf[Vector[A]]
      case _ => new VectorViewImpl[A]()
    }
  }

  private def newVectorFromSeq[A](data: Seq[A], is_row: Boolean = true, frozen: Boolean = false)(implicit m: ClassManifest[A]): Vector[A] = {
    val v = newVector[A](data.length, is_row)
    for (i <- 0 until data.length){
      v(i) = data(i)
    }
    if (frozen) v.freeze
    v
  }

  private def newVectorFromFunction[A : ClassManifest](func: Int => A)(len: Int): Vector[A] = {
    range(0, len).map(i => func(i))
  }


  //////////
  // delite
  /*
  class Proxy[T] extends DeliteProxy[Vector[T]] with Vector[T] {
    override def nodeAttr = "[shape=box]"
    override def toString = "Vector" + super.toString

    def elementByteSize = force.elementByteSize
    def apply(n: Int) = force.apply(n)
    def lifted_apply(n: Int) = force.lifted_apply(n)
    def indices = force.indices
    def view(start: Int, stride: Int, length: Int, is_row: Boolean) = force.view(start, stride, length, is_row)
    def mutableClone = force.mutableClone
    def update(n: Int, x: T) = force.update(n, x)
    def lifted_update(n: Int, x: T) = force.update(n, x)
    def +=[A <: T](x: A) = force.+=(x)
    def copyFrom[A <: T](pos: Int, xs: Vector[A]) = force.copyFrom(pos, xs)
    def insert[A <: T](pos: Int, x: A) = force.insert(pos, x)
    def insertAll[A <: T](pos: Int, xs: Vector[A]) = force.insertAll(pos, xs)
    def removeAll(pos: Int, len: Int) = force.removeAll(pos, len)
    def trim = force.trim
  }
  */

  class ProxyFactory[T: ClassManifest] extends DeliteProxyFactory[Vector[T]] {
    def newProxy = newVector[T]
  }

  class ViewProxyFactory[T: ClassManifest] extends DeliteProxyFactory[Vector[T]] {
    def newProxy = newVectorView[T]
  }

  class Builder extends DeliteCollectionFactory[Vector] {
    def newInstance[A](size: Int, shape: Option[Any])(implicit m: ClassManifest[A]) = {
      // TODO: how can we link the shape type parameters from the builder and collection
      // TODO: together without a lot of ugliness?
      assert(shape.isInstanceOf[Option[Boolean]])
      assert(shape.isEmpty == false)
      val s = shape.asInstanceOf[Option[Boolean]].get
      Vector[A](s, size)
    }
  }

  /*
   *  The following are the OPs that this DSL defines, the purpose of OPs is delayed execution and optimization
   */

  protected[optiml] case class OP_flatten[A : ClassManifest](pieces: Vector[Vector[A]])
    extends DeliteOP_SingleTask[Vector[A]](pieces) {

    def task = {
      if (pieces.length == 0){
        Vector[A](0)
      }
      else {
        val sizes = pieces.map(e => e.length)
        val (total,begins) = sizes.precumulate(0)((_: Int) + (_: Int))
        val result = Vector[A](total)
        for (i <- 0 until pieces.length) {
          result.copyFrom(begins(i), pieces(i))
        }
        result
      }
    }
  }

  protected[optiml] case class OP_ones(length: Int)
    extends DeliteOP_Map[Int,Double,Vector]{

    val coll = Vector[Int](length)
    val out = Vector[Double](length)
    def func = e => 1.
  }


  protected[optiml] case class OP_zeros(length: Int)
    extends DeliteOP_Map[Int,Double,Vector] {

    val coll = Vector[Int](length)
    val out = Vector[Double](length)
    def func = e => 0.
  }


  protected[optiml] case class OP_zerosf(length: Int)
    extends DeliteOP_Map[Int,Float,Vector] {

    val coll = Vector[Int](length)
    val out = Vector[Float](length)
    def func = e => 0
  }

  /* Generate a pseudo-random vector uniformly distributed on [0,1) */

  protected[optiml] case class OP_rand(length: Int)
    extends DeliteOP_Map[Int,Double,Vector]{

    val coll = Vector[Int](length)
    val out = Vector[Double](length)
    def func = e => random[Double]
  }

  protected[optiml] case class OP_randn(length: Int)
    extends DeliteOP_Map[Int,Double,Vector]{

    val coll = Vector[Int](length)
    val out = Vector[Double](length)
    def func = e => randomGaussian
  }

  /* Generate a vector with values from start to end at step_size intervals */
  protected[optiml] case class OP_uniform(start: Double, step_size: Double, end: Double)
    extends DeliteOP_Map[Int,Double,Vector]{

    val coll = range(0, Math.ceil((end-start)/step_size).asInstanceOf[Int])
    val out = Vector[Double](coll.length)
    def func = e => (e*step_size + start)
  }

  protected[optiml] case class OP_update[A](v: Vector[A], index: Int, x: A)
    extends DeliteOP_MutableSingleTask[Vector[A]]()(v) {

    def task = {
      //v._update(index,x)
      v.update(index,x)
      v
    }
  }

  // TODO: convert to a MapReduce?
  protected[optiml] case class OP_cmp[A](v1: Vector[A], v2: Vector[A])
    extends DeliteOP_SingleTask[DeliteBoolean](v1,v2) {

    def task = {
      v1.chkLength(v1, v2)
      var res = true
      if (v1.is_row != v2.is_row) res = false
      else{
        for (i <- 0 until v1.length){
          if (v1(i) != v2(i)) res = false
        }
      }
      DeliteBoolean(res)
    }
  }

  protected[optiml] case class OP_hash[A](v: Vector[A], s: Int, e: Int, val func: (A,A) => A)
    (implicit conv: A => DeliteLong) extends DeliteOP_Reduce[A,DeliteLong] {

    val coll = v.view(s,1,e-s,v.is_row)
  }

  protected[optiml] case class OP_slice[A : ClassManifest](val v: Vector[A], s: Int, e: Int)
    extends DeliteOP_SingleTask[Vector[A]](v) {

    def task = {
      v.chkRange(s, e)
      val nv = Vector[A](e-s)
      for (i <- s until e){
        nv(i-s) = v(i)
      }
      nv
    }
  }

  protected[optiml] case class OP_trans[A](val v: Vector[A])
    extends DeliteOP_SingleTask[Vector[A]](v) {

    def task = {
      if (v.frozen) (v.mutableClone.mtrans).freeze
      else v.mutableClone.mtrans
    }
  }

  /**
   * arithmetic ops
   */
  protected[optiml] case class OP_+[A](val collA: Vector[A], val collB: Vector[A], val out: Vector[A])(implicit ops: ArithOps[A], c: ClassManifest[A])
    extends DeliteOP_ZipWith2[A,A,A,Vector] {
    override val associative = true

    def func = (a,b) => ops.+(a,b)
  }

  protected[optiml] case class OP_+=[A](val collA: Vector[A], val collB: Vector[A])(implicit ops: ArithOps[A], c: ClassManifest[A])
    extends DeliteOP_ZipWith2[A,A,A,Vector]{
    val out = collA
    def func = (a,b) => ops.+(a,b)
  }

  protected[optiml] case class OP_-[A](val collA: Vector[A], val collB: Vector[A], val out: Vector[A])(implicit ops: ArithOps[A], c: ClassManifest[A])
    extends DeliteOP_ZipWith2[A,A,A,Vector] {

    def func = (a,b) => ops.-(a,b)
  }

  protected[optiml] case class OP_/[A](val collA: Vector[A], val collB: Vector[A], val out: Vector[A])(implicit ops: ArithOps[A], c: ClassManifest[A])
    extends DeliteOP_ZipWith2[A,A,A,Vector] {

    def func = (a,b) => ops./(a,b)
  }

  protected[optiml] case class OP_mtimes[A](v: Vector[A], m: Matrix[A])(implicit ops: ArithOps[A], c: ClassManifest[A])
    extends DeliteOP_SingleTask[Vector[A]](v,m) {

    def task = {
      v.chkVecMatAgree(v, m)
      val v_trans = v.trans
      m.trans.mapRowsToVec(a_row => a_row._dot(v_trans))
    }
  }

  /* Vector times sparse matrix multiplication */
  protected[optiml] case class OP_smtimes[A](v: Vector[A], sm: SparseMatrix[A])(implicit ops: ArithOps[A], c: ClassManifest[A])
    extends DeliteOP_SingleTask[Vector[A]](v,sm) {

    def task = {
      //v.chkVecMatAgree(v, sm)
      //TODO: take this out later
      if (v.length != sm.numRows){
        Console.printf("error: vector-matrix dimensions [1][%d]*[%d][%d] don't agree\n", v.length, sm.numRows, sm.numCols)
        throw new IllegalArgumentException
      }
      else if (!v.is_row && sm.numRows != 1){
        Console.printf("error: matrix-vector dimensions [%d][1]*[%d][%d] don't agree\n", v.length, sm.numRows, sm.numCols)
        throw new IllegalArgumentException
      }

      val v_trans = v.trans
      //m.trans.mapRowsToVec(a_row => a_row._dot(v_trans))
      val sm_trans = sm.trans
      val out = sm_trans * v_trans
      out.trans
    }
  }

  protected[optiml] case class OP_*[A](val collA: Vector[A], val collB: Vector[A], val out: Vector[A])(implicit ops: ArithOps[A], c: ClassManifest[A])
    extends DeliteOP_ZipWith2[A,A,A,Vector] {

    def func = (a,b) => ops.*(a,b)
  }

  protected[optiml] case class OP_outer[A](v1: Vector[A], v2: Vector[A])(implicit val ops: ArithOps[A], c: ClassManifest[A])
    extends DeliteOP_SingleTask[Matrix[A]](v1,v2) {

    def task = {
      if (v1.length != v2.length || v1.is_row == v2.is_row || v1.is_row) v1.vecDimError()
      Matrix.fromMap[A,A](v1, (a_row => v2*a_row))
    }
  }

  // TODO: this is really a ZipWithReduce
  // TODO: should this be a Delite data parallel type, or should we allow "nested" data parallel ops?
  protected[optiml] case class OP_dot[A,B <: DeliteDSLType](v1: Vector[A], v2: Vector[A])
    (implicit ops: ArithOps[A], conv: A => B) extends DeliteOP_SingleTask[B](v1, v2) {

    def task = {
      v1.chkLength(v1, v2)
      var acc : A = ops.*(v1(0),v2(0))
      for (i <- 1 until v1.length){
        acc = ops.+(acc, ops.*(v1(i),v2(i)))
      }
      conv(acc)
    }
  }

  /*
  protected[optiml] case class OP_sum[A](v: Vector[A], f: (A,A) => A)
    (implicit pFact: DeliteProxyFactory[DelitePrimitive[A]], ops: ArithOps[A], conv: A => DelitePrimitive[A])
    extends DeliteOP_SingleTask[DelitePrimitive[A]] {

    //val res = needs[A](OP_reduce(v, f))
    def task = {
      //res
      OP_reduce(v,f)(ops, conv).task()
      //v.reduce((a,b) => ops.+(a,b))
    }
  }
  */

  /*
   * TODO:
   * This method of lifting ops allows us to be more general and have better performance (we
   * can escape the DelitePrimitive world in a map using a conversion on the return type list this),
   * but the compiler cannot infer the type B which ends up causing us to lose transparency to the
   * Delite user.
   */
  protected[optiml] case class OP_sum[A,B <: DeliteDSLType](val coll: Vector[A])
    (implicit ops: ArithOps[A], conv: A => B) extends DeliteOP_Reduce[A,B] {

    def func = (a,b) => ops.+(a,b)
  }

  protected[optiml] case class OP_abs[A](val coll: Vector[A], val out: Vector[A])
    (implicit ops: ArithOps[A], pFact: Vector.ProxyFactory[A], c: ClassManifest[A])
    extends DeliteOP_Map[A,A,Vector]{

    def func = e => ops.abs(e)
  }

  protected[optiml] case class OP_exp[A](val coll: Vector[A], val out: Vector[A])
    (implicit ops: ArithOps[A], pFact: Vector.ProxyFactory[A], c: ClassManifest[A])
    extends DeliteOP_Map[A,A,Vector]{

    def func = e => ops.exp(e)
  }


  /**
   * ordering ops
   */

  protected[optiml] case class OP_min[A,B <: DeliteDSLType](val coll: Vector[A])
    (implicit cmp: A => Ordered[A], conv: A => B) extends DeliteOP_Reduce[A,B] {

    def func = (a,b) => if (cmp(a) < b) a else b
  }

  // TODO: how to represent this as a DeliteOP_Reduce?
  // TODO: we may want to explicitly expose element index for data parallel operations
  protected[optiml] case class OP_minIndex[A](v: Vector[A])
    (implicit cmp: A => Ordered[A]) extends DeliteOP_SingleTask[DeliteInt](v) {

    // this would be a slow lookup
    //implicit def conv : A => DeliteInt = e => coll.asInstanceOf[Vector[A]].indexOf(e)

    def task = {
      var smallest = v(0)
      var min_i = 0
      for (i <- 1 until v.length){
          if (v(i) < smallest){
            smallest = v(i)
            min_i = i
        }
      }
      DeliteInt(min_i)
    }
  }

  protected[optiml] case class OP_median[A,B <: DeliteDSLType](val coll: Vector[A])
    (implicit ops: ArithOps[A], cmp: A => Ordered[A], conv: A => B, pFact: DeliteProxyFactory[B], vecPfact: DeliteProxyFactory[Vector[A]])
    extends DeliteOP_SingleTask[B](coll) {

    def task = {
      coll.sort
      // TODO: this isn't the proper definition of median
      // need to figure out how to generalize with ArithOps. should median always return a Double?
      coll(coll.length / 2)
    }
  }

  protected[optiml] case class OP_max[A,B <: DeliteDSLType](val coll: Vector[A])
    (implicit cmp: A => Ordered[A], conv: A => B) extends DeliteOP_Reduce[A,B] {

    def func = (a,b) => if (cmp(a) > b) a else b
  }

  protected[optiml] case class OP_maxIndex[A](v: Vector[A])
    (implicit cmp: A => Ordered[A]) extends DeliteOP_SingleTask[DeliteInt](v) {

    def task = {
      var largest = v(0)
      var max_i = 0
      for (i <- 1 until v.length){
          if (v(i) > largest){
            largest = v(i)
            max_i = i
        }
      }
      DeliteInt(max_i)
    }
  }

  protected[optiml] case class OP_sort[A](val coll: Vector[A])
    (implicit cmp: A => Ordered[A]) extends DeliteOP_SingleTask[Vector[A]](coll) {

    //def task = throw new UnsupportedOperationException()
    // TODO: below is a really naive implementation of sort, replace it with quick-sort from scala/java api
    println("warning: calling Vector.sort (slow!!!)")
    def task = {
      val res = coll.clone
      for(i <- 0 until coll.length) {
        for(j <- i+1 until coll.length) {
          if(res(i) < res(j)) {
            val temp = res(i)
            res(i) = res(j)
            res(j) = temp
          }
        }
      }
      res
    }
  }

  /**
   * bulk ops
   */

  protected[optiml] case class OP_reduce[A, B <: DeliteDSLType](val coll: Vector[A], val func: (A,A) => A)
    (implicit conv: A => B) extends DeliteOP_Reduce[A,B]

  protected[optiml] case class OP_map[A, B : ClassManifest](val coll: Vector[A], val out: Vector[B], val func: A => B)
    extends DeliteOP_Map[A,B,Vector]

  protected[optiml] case class OP_forEach[A : ClassManifest](val coll: Vector[A], val func: A => Unit, mutable: DeliteDSLType*)
    extends DeliteOP_ForEach[A,Vector[A]]()(mutable: _*) { val out = coll }

  protected[optiml] case class OP_mutMap[A](val coll: Vector[A], val out: Vector[A], val func: A => A)
    extends DeliteOP_Map[A,A,Vector] {

    override def getMutableDeps = Seq(out)
  }

  protected [optiml] case class OP_dynMap[A, B: ClassManifest](val coll: Vector[A], val out: Vector[B], val func: A => B, mutable: DeliteDSLType*)
    extends DeliteOP_DynamicMap[A,B,Vector] {

    override def getMutableDeps = Seq(out) ++ mutable
  }




  protected[optiml] case class OP_alloc[A,B: ClassManifest](orig: Vector[A]) extends DeliteOP_SingleTask[Vector[B]](orig) {
    def task = {
      Vector[B](orig.is_row, orig.length)
    }
  }

  // TODO: how can we represent this using a DeliteOP_Reduce? (it is still tree structured)
  protected[optiml] case class OP_filter[A : ClassManifest](v: Vector[A], pred: A => Boolean)
    extends DeliteOP_SingleTask[Vector[A]](v) {

    def task = {
      val result = Vector[A](0)
      val elems = v.elements
      while (elems.hasNext) {
        val x = elems.next
        if (pred(x)) result += x
      }

      result
    }
 }

  protected[optiml] case class OP_flatMap_single[A,B](v: Vector[A], f: A => Vector[B])
    (implicit pfact: Vector.ProxyFactory[B], m : ClassManifest[B])
    extends DeliteOP_SingleTask[Vector[B]] {

    def task = {
      val pieces = v map {a => f(a)}
      Vector.flatten(pieces)
    }
  }

  /**
   * chaining ops
   */

  /*
  protected[optiml] case class OP_transOuter[A](v1: Vector[A], v2: Vector[A])
  (implicit ops: ArithOps[A], vFact: Vector.Factory[A],
   mFact: Matrix.Factory[A], mStrat: Matrix.Strategy) extends DeliteOP_Map[Matrix[A]](v1,v2) {
    def task = {
      // do not perform the transpose; since this is a vector-vector outer product, we know that it's unnecessary
      if (v1.length != v2.length)  v1.vecDimError()
      mFact.fromMap[A](v1, (a_row => v2*a_row))
    }
  }
  */


  //////////////////////////////////////////////////////////////
  ///////////////   GPU special MAP OPs    /////////////////////
  //////////////////////////////////////////////////////////////

  protected[delite] case class OP_mapNB[A, B : ClassManifest](val coll: Vector[A], val out: Vector[B], val func: A => B, val numTokens:Int, val numTrainDocs:Int, val compareWith:Int, val classifications:Vector[Double], val features:Matrix[Double], val weightedspamcount:DeliteDouble)
    extends DeliteOP_Map[A,B,Vector] {

    override def getMutableDeps = null

    override def getGPUInputs = List[AnyRef](features, classifications)
    override def getGPUConsts = List[AnyVal](numTokens, numTrainDocs, compareWith, weightedspamcount.value)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Vector[Double](elms, coll._is_row, coll._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    //override def getGPUKernelDims = List(coll._length, 16, 16, 16)
    //override def getGPUKernelId = List(DeliteCuda.MAP_NB, DeliteCuda.AsyncLaunch2D)
    //override def getGPUKernelDims = List(coll._length, 64)
    override def getGPUKernelDims = List(coll._length)
    override def getGPUKernelId = List(DeliteCuda.MAP_NB, DeliteCuda.AsyncLaunch1D)
	  //override def getGPUKernelId = {List(DeliteCuda.MAP_NB, DeliteCuda.AsyncMAP)}
  }

  protected[delite] case class OP_mapLR[A, B : ClassManifest](val coll: Vector[A], val out: Vector[B], val func: A => B, val x_cur:Double, val tau:Int)
    extends DeliteOP_Map[A,B,Vector] {

	  override def getMutableDeps = null

    override def getGPUInputs = List(coll)
    override def getGPUConsts = List[AnyVal](x_cur, tau, coll._length)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Vector[Double](elms, coll._is_row, coll._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(coll._length)
    override def getGPUKernelId = {List(DeliteCuda.MAP_LR, DeliteCuda.AsyncLaunch1D)}
	  //override def getGPUKernelId = {List(DeliteCuda.MAP_LR, DeliteCuda.AsyncMAPLR)}
  }

  protected[delite] case class OP_mapKM1[A, B : ClassManifest](val coll: Vector[A], val out: Vector[B], val func: A => B, val x:Matrix[Double], val mu:Matrix[Double])
    extends DeliteOP_Map[A,B,Vector] {

    override def getMutableDeps = null

    override def getGPUInputs = List(x, mu)
	  override def getGPUConsts = List(x._numRows, x._numCols, mu._numRows, mu._numCols)
    override def getGPUOutput = {
      val elms = new Array[Int](0)
      val gout = Vector[Int](elms, coll._is_row, coll._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    //override def getGPUKernelDims = List(x._numCols, x._numRows, x._numCols, 512/x._numCols)
    //override def getGPUKernelId = List(DeliteCuda.mapKM1, DeliteCuda.AsyncLaunch2D)
    override def getGPUKernelDims = List(x._numRows)
    override def getGPUKernelId = List(DeliteCuda.mapKM1, DeliteCuda.AsyncLaunch1D)
    //override def getGPUKernelId = List(DeliteCuda.mapKM1, DeliteCuda.AsyncKM1)
  }

  protected[delite] case class OP_mapKM2[A, B : ClassManifest](val coll: Vector[A], val out: Vector[B], val func: A => B, val x:Matrix[Double], val mu:Matrix[Double], val cc:Vector[Int])
    extends DeliteOP_Map[A,B,Vector] {

    //override def getMutableDeps = null
    override def getMutableDeps = Seq(mu)
    override def getGPUInputs = List[AnyRef](x, mu, cc)
    override def getGPUConsts = List(x._numRows, x._numCols, mu._numRows)
    override def getGPUOutput = {
      val elms = new Array[Int](0)
      val gout = Vector[Int](elms, coll._is_row, coll._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    //override def getGPUKernelDims = List(mu._numRows, 510, 1, 510)
    override def getGPUKernelDims = List(mu._numRows, x._numCols*(512/x._numCols), 1, x._numCols*(512/x._numCols))
    override def getGPUKernelId = List(DeliteCuda.mapKM2, DeliteCuda.AsyncLaunch2D)
    //override def getGPUKernelDims = List(mu._numRows, 1)
    //override def getGPUKernelId = List(DeliteCuda.mapKM2, DeliteCuda.AsyncLaunch1D)
	//override def getGPUKernelId = {List(DeliteCuda.mapKM2, DeliteCuda.AsyncKM2)}
  }

}

trait Vector[@specialized(Double,Float,Int) T] extends DeliteCollection[T] with Cloneable[Vector[T]] {
  import Vector._

  //type DSLType = Vector[T]
  type Shape = Boolean

  //implicit def proxyFactory[T: ClassManifest] = new ProxyFactory[T]

  ////////////////////////////////
  // DeliteCollection

  override def shape = Option(is_row)

  def size = _length

  def chunk(start: Int, end: Int) = new Iterator[T] {
    private var index = start
    private val ceil = if (end+1 <= Vector.this.length) end+1 else Vector.this.length+1
    def hasNext = (index < ceil)
    def next = {
      index += 1
      Vector.this(index-1)
    }
  }

  ////////////////////////////////
  // public (read-only) properties

  /* Real length of the vector; underlying raw data may be allocated more. */
  def length : Int

  /* Defines vector orientation (row-wise or column-wise) */
  def is_row : Boolean

  /* Checks popsicle immutability */
  def frozen: Boolean

  /////////////////////////
  // for internal use only
  // UNSAFE! Will throw a NPE if used on a proxy -- USE WITH CAUTION
  /*protected*/ def _length : Int
  /*protected*/ def _length_=(n: Int) : Unit
  /*protected*/ def _is_row : Boolean
  /*protected*/ def _is_row_=(b: Boolean) : Unit
  protected def _frozen: Boolean
  protected def _frozen_=(b: Boolean) : Unit

  ///////////////
  // conversions

  /* Returns a sequence containing the data for this vector */
  def toSeq : Seq[T] = new Seq[T] {
    def length = Vector.this.length
    def apply(index: Int) = Vector.this(index)
    def iterator = Vector.this.elements
  }
  def toBoolean(implicit conv: T => Boolean) =  map(e => conv(e))
  def toDouble(implicit conv: T => Double) =  map(e => conv(e))
  def toFloat(implicit conv: T => Float) = map(e => conv(e))
  def toInt(implicit conv: T => Int) = map(e => conv(e))
  def toLong(implicit conv: T => Long) = map(e => conv(e))

  /////////////
  // accessors

  /* Returns the nth element of the vector */
  //a.$(n)
  def $(n: Int) : T = lifted_apply(n)
  def lifted_apply(n: Int) : T
  def apply(n: Int) : T

  /* Returns a sequence of the indices of the vector */
  def indices : Vector[Int]

  /* Returns an iterator for the elements of the vector */
  def elements: Iterator[T] = new Iterator[T] {
    var pos = 0
    def hasNext = pos < _length
    def next = {
      pos += 1
      Vector.this(pos - 1)
    }
  }

  // should be frozen if vector is frozen
  def view(start: Int, stride: Int, length: Int, is_row: Boolean) : VectorView[T]

  def isEmpty = length == 0
  def first = this(0)
  def last = this(length - 1)

  def drop(count: Int)(implicit m : ClassManifest[T]): Vector[T] = slice(count, length)
  def take(count: Int)(implicit m : ClassManifest[T]): Vector[T] = slice(0, count)

  def slice(begin: Int, end: Int)(implicit m : ClassManifest[T]): Vector[T] = {
    run(OP_slice(this, begin, end))
  }

  //////////
  // general

  /* Vector transpose by copy */

  def trans(implicit c: ClassManifest[T]) : Vector[T] = {
    run(OP_trans(this))
  }

  /* Light-weight mutable vector transpose */
  def mtrans : Vector[T] = {
    if (frozen) throw new IllegalArgumentException
    _is_row = !is_row
    this
  }

  /* Pretty print */
  def pprint = {
    if (is_row){
      print("[ ")
      this.foreach(e => {  print(e)
                           print(" ")
      })
      print("]\n")
    }
    else{
      this.foreach(e => { print("[ ")
                          print(e)
                          print(" ]\n")
      })
    }
  }

  override def toString = {
    val string:String = ""
    if (is_row){
      string.concat("[ ")
      this.foreach(e => {  string.concat(e + " ")
      })
      string.concat("]\n")
    }
    else{
      this.foreach(e => { string.concat("[ " + e + " ]\n")
      })
    }
    string
  }

  //override def equals(that: Any) = that match {
  // TODO: fix hack, see Memoizer line 35 for explanation
  /*
  override def eqhack(that: Any) = that match {
    case v:Vector[T] => {
      if (v.length != length || v.is_row != is_row) false
      for (i <- 0 until length){
        if (v(i).equals(this(i)) == false) false
      }
      true
    }
    case _ => false
  }
  */

  //////////////
  // life cycle

  /* Freeze the vector, preventing future changes. */
  def freeze() : Vector[T] = {
    _frozen = true
    this
  }

  override def clone: Vector[T] = {
    val ret = mutableClone
    if (frozen) ret.freeze()
    ret
  }
  def mutableClone: Vector[T]

  ///////////////////
  // data operations

  /* Update the nth element of the vector with x */
  //def update(n: Int, x: => T) = _update(n, x)//run(OP_update(this, n,  x))
  // x := (n,x)
  def :=(n: Int, x: T) = lifted_update(n, x)
  def lifted_update(n: Int, x: T)
  def update(n: Int, x: T)

  def ++[A <: T](xs: Vector[A])(implicit m : ClassManifest[T]) : Vector[T] = {
    val buf = Vector[T](is_row, 0)
    buf ++= this
    buf ++= xs
    if (frozen) buf.freeze
    buf
  }

  /* Adds a single element to the vector */
  def +=[A <: T](x: A): Vector[T]

  /* Appends a vector to the end of the current vector */
  def ++=[A <: T](xs: Vector[A]): Vector[T] = insertAll(length, xs)

  /* Replaces elements in the current vector with elements from another vector */
  def copyFrom[A <: T](pos: Int, xs: Vector[A]): Vector[T]

  /* Inserts an element in the vector at pos */
  def insert[A <: T](pos: Int, x: A): Vector[T]

  /* Inserts all of the elements from the argument vector into this vector */
  def insertAll[A <: T](pos: Int, xs: Vector[A]): Vector[T]

  /* Removes an element from the vector at pos */
  def remove(pos: Int): Vector[T] = removeAll(pos, 1)

  /* Removes len elements from the vector at pos */
  def removeAll(pos: Int, len: Int): Vector[T]

  /* Reduces the vector's internal storage to be exactly the real length of the vector */
  def trim: Vector[T]

  def cmp(v: Vector[T]) : DeliteBoolean = {
    run(OP_cmp(this, v))
  }

  def hash(s: Int, e: Int, f: (T,T) => T)(implicit conv: T => DeliteLong) : DeliteLong  = {
    run(OP_hash(this, s, e, f))
  }

  /////////////////////////
  // arithmetic operations

  def +(v: Vector[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Vector[T] = {
    run(OP_+(this,v, run(OP_alloc[T,T](this))))
  }

  def +=(v: Vector[T])(implicit ops: ArithOps[T], c: ClassManifest[T]): Unit = {
    run(OP_+=(this,v))
  }

  def +(x: T)(implicit ops: ArithOps[T], c: ClassManifest[T]) : Vector[T] = {
    run(OP_map[T,T](this, run(OP_alloc[T,T](this)), ele => ops.+(ele,x)))
  }

  def -(v: Vector[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Vector[T] = {
    run(OP_-(this,v, run(OP_alloc[T,T](this))))
  }
  def -(x: T)(implicit ops: ArithOps[T], c: ClassManifest[T]) : Vector[T] = {
    run(OP_map[T,T](this, run(OP_alloc[T,T](this)), ele => ops.-(ele,x)))
  }

  def /(v: Vector[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Vector[T] = {
    run(OP_/(this,v, run(OP_alloc[T,T](this))))
  }
  def /(x: T)(implicit ops: ArithOps[T], c: ClassManifest[T]) : Vector[T] = {
    run(OP_map[T,T](this, run(OP_alloc[T,T](this)), ele => ops./(ele,x)))
  }

  /* vector-matrix */
  def *(m: Matrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Vector[T] = {
    run(OP_mtimes(this,m))
  }

  /* vector-sparse matrix */
  def *(m: SparseMatrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Vector[T] = {
    run(OP_smtimes(this,m))
  }

  /* vector-vector point-wise */
  def *(v: Vector[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Vector[T] = {
    run(OP_*(this,v, run(OP_alloc[T,T](this))))
  }

  /* vector-vector outer product */
  def outer(v: Vector[T])(implicit ops: ArithOps[T], pFact: Matrix.ProxyFactory[T], c: ClassManifest[T]) : Matrix[T] = {
    run[Matrix[T]](OP_outer(this,v))
  }

  /* vector-vector inner product */
  def dot[B <: DeliteDSLType](v: Vector[T])(implicit ops: ArithOps[T], conv: T => B, pfact: DeliteProxyFactory[B]) : B = {
    run(OP_dot(this, v)(ops, conv))(pfact)
  }

  // this is ugly, and is due to a deferred dot requiring a proxy type. if we are already executing
  // a deferred task, we know we want to execute the dot and don't care about deferring,
  // so we skip submission
  def _dot(v: Vector[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : T = {
    chkLength(this, v)
    var acc : T = ops.*(this(0),v(0))
    for (i <- 1 until length){
      acc = ops.+(acc, ops.*(this(i),v(i)))
    }
    acc
  }

  /* vector-scalar product */
  def *(x: T)(implicit ops: ArithOps[T], c: ClassManifest[T]) : Vector[T] = {
    run(OP_map[T,T](this, run(OP_alloc[T,T](this)), ele => ops.*(ele,x)))
  }

  /* Returns the summation of this vector */
  def sum[B <: DeliteDSLType](implicit pfact: DeliteProxyFactory[B], ops: ArithOps[T], conv: T => B): B = {
    run(OP_sum(this)(ops, conv))
  }

  def _sum(implicit ops: ArithOps[T]): T = {
    _reduce((a,b) => ops.+(a,b))
  }

  /*
  def sum2[B <: DeliteDSLType](implicit pFact: DeliteProxyFactory[B], ops: ArithOps[T], conv: T => B): B = {
    //run(OP_sum[T,B](this, (a,b) => ops.+(a,b)))
    Delite.run2(OP_sum[T,B](this, (a,b) => ops.+(a,b)))
    //reduce((a,b) => ops.+(a,b))
  }
  */

  def abs(implicit ops: ArithOps[T], pfact: DeliteProxyFactory[Vector[T]], c: ClassManifest[T]) : Vector[T] = {
    run(OP_abs(this, run(OP_alloc[T,T](this))))
  }

  def exp(implicit ops: ArithOps[T], pfact: DeliteProxyFactory[Vector[T]], c: ClassManifest[T]) : Vector[T] = {
    run(OP_exp(this, run(OP_alloc[T,T](this))))
  }

  /////////////////////////////
  // stuff requiring ordering

  def min[B <: DeliteDSLType](implicit cmp: T => Ordered[T], conv: T => B, pfact: DeliteProxyFactory[B]): B = {
    run(OP_min(this))
  }
  def _min(implicit cmp: T => Ordered[T]): T = {
    _reduce((a,b) => if (cmp(a) < b) a else b)
  }

  def minIndex(implicit cmp: T => Ordered[T]): DeliteInt = {
    run(OP_minIndex(this))
  }

  def median[B <: DeliteDSLType](implicit ops: ArithOps[T], cmp: T => Ordered[T], conv: T => B, pfact: DeliteProxyFactory[B], vecPfact: DeliteProxyFactory[Vector[T]]): B = {
    run(OP_median(this))
  }

  def max[B <: DeliteDSLType](implicit cmp: T => Ordered[T], conv: T => B, pfact: DeliteProxyFactory[B]): B = {
    run(OP_max(this))
  }
  def _max(implicit cmp: T => Ordered[T]): T = {
    _reduce((a,b) => if (cmp(a) > b) a else b)
  }

  def maxIndex(implicit cmp: T => Ordered[T]): DeliteInt = {
    run(OP_maxIndex(this))
  }

  def minMax[B <: DeliteDSLType](implicit cmp: T => Ordered[T], conv: T => B, pfact: DeliteProxyFactory[B]): (B,B) = {
    (min, max)
  }

  def sort(implicit cmp: T => Ordered[T], pfact: DeliteProxyFactory[Vector[T]]) : Vector[T] = {
    run(OP_sort(this))
  }

  //////////////////
  // bulk operations

  def map[B](f: T => B)(implicit pFact: DeliteProxyFactory[Vector[B]], c: ClassManifest[B]) : Vector[B] = {
    run[Vector[B]](OP_map[T,B](this, run(OP_alloc[T,B](this)), f))
  }


  def mmap(f: T => T)(implicit pFact: DeliteProxyFactory[Vector[T]], c: ClassManifest[T]) : Vector[T] = {
    run(OP_mutMap[T](this, this, f))
  }

  def dynMap[B](f: T => B)(mutable: DeliteDSLType*)(implicit pFact: DeliteProxyFactory[Vector[B]], c: ClassManifest[B]) : Vector[B] = {
    run[Vector[B]](OP_dynMap[T,B](this, run(OP_alloc[T,B](this)), f, mutable: _*))
  }

  def mforeach(f: T => Unit)(mutable: DeliteDSLType*)(implicit pFact: DeliteProxyFactory[Vector[T]], c: ClassManifest[T]): Vector[T] = {
    run(OP_forEach[T](this, f, mutable: _*))
  }

  def foreach(block: T => Unit) = {
    for (i <- 0 until length){
      block(this(i))
    }
  }

  def reduce[B <: DeliteDSLType](func: (T,T) => T)(implicit conv: T => B, pfact: DeliteProxyFactory[B]): B = {
    run(OP_reduce(this, func)(conv))
  }

  def _reduce(func: (T,T) => T): T = {
    val thresh = 10
    var acc = this(0)
    for (i <- 1 until length){
      acc = func(acc, this(i))
    }
    acc
  }

  def precumulate(identity: T)(func: (T,T) => T)(implicit m : ClassManifest[T]): (T,Vector[T]) = {
    if (this.length == 0) return (identity,Vector[T](0))
    val result = Vector[T](0)
    var accum = identity
    var i = 0
    while (i < length) {
      result += accum
      accum = func(accum, this(i))
      i += 1
    }
    (accum,result)
  }

  def filter(pred: T => Boolean)(implicit c: ClassManifest[T]): Vector[T] = {
    run(OP_filter(this, pred))
 }

  def flatMap[B](f: T => Vector[B])(implicit pfact: Vector.ProxyFactory[B], c: ClassManifest[B]) : Vector[B] = {
    val pieces = this map {a => f(a)}
    // TODO: Use reduce to run flatten?  May actually be slower in some cases
    Vector.flatten(pieces)
  }

  def flatMapSequential[B](f: T => Vector[B])(implicit pfact: Vector.ProxyFactory[B], c: ClassManifest[B]) : Vector[B] = {
    run(OP_flatMap_single(this, f))
  }


  def contains(x: T) : Boolean = {
    var i = 0
    while (i < length) {
      if (this(i) == x) return true
      i += 1
    }
    return false
  }

  def distinct(implicit m : ClassManifest[T]) : Vector[T] = {
    val result = Vector[T](0)
    var i = 0
    while (i < length) {
      if (!result.contains(this(i))) result += this(i)
      i += 1
    }
    result
  }
  
  def partition(pred: T => Boolean)(implicit m : ClassManifest[T]): (Vector[T],Vector[T]) = {
    val resultT = Vector[T](0)
    val resultF = Vector[T](0)
    var i = 0
    while (i < length) {
      val x = this(i)
      (if (pred(x)) resultT else resultF) += x
      i += 1
    }

    (resultT, resultF)
  }


  /////////////////////////////////////
  // Dirty hacked operations to run GPU

  // Map operation for Naive Bayes
  def mapNB[B](f: T => B)(numTokens:Int, numTrainDocs:Int, compareWith:Int)(classifications:Vector[Double], features:Matrix[Double], weightedspamcount:DeliteDouble)(implicit pFact: DeliteProxyFactory[Vector[B]], c: ClassManifest[B]) : Vector[B] = {
    if(Config.executorType == "gpu")
      run[Vector[B]](OP_mapNB[T,B](this, null, f, numTokens, numTrainDocs, compareWith, classifications, features, weightedspamcount))
    else
      run[Vector[B]](OP_map[T,B](this, run(OP_alloc[T,B](this)), f))
  }

  // Map operation for Linear Regression
  def mapLR[B](f: T => B)(x_cur:Double, tau:Int)(implicit pFact: DeliteProxyFactory[Vector[B]], c: ClassManifest[B]) : Vector[B] = {
    if(Config.executorType == "gpu")
      run[Vector[B]](OP_mapLR[T,B](this, null, f, x_cur, tau))
    else
      run[Vector[B]](OP_map[T,B](this, run(OP_alloc[T,B](this)), f))
  }

  // Map operation for K-means
  def mapKM1[B](f: T => B)(x:Matrix[Double], mu:Matrix[Double])(implicit pFact: DeliteProxyFactory[Vector[B]], c: ClassManifest[B]) : Vector[B] = {
    if(Config.executorType == "gpu")
      run[Vector[B]](OP_mapKM1[T,B](this, null, f, x, mu))
    else
      run[Vector[B]](OP_map[T,B](this, run(OP_alloc[T,B](this)), f))
  }

  // Map operation for K-means
  def mapKM2[B](f: T => B)(x:Matrix[Double], mu:Matrix[Double], cc:Vector[Int])(implicit pFact: DeliteProxyFactory[Vector[B]], c: ClassManifest[B]) : Vector[B] = {
    if(Config.executorType == "gpu")
      run[Vector[B]](OP_mapKM2[T,B](this, null, f, x, mu, cc))
    else
      run[Vector[B]](OP_map[T,B](this, run(OP_alloc[T,B](this)), f))
  }

  /*
  // Map operation for Naive Bayes
  def mapNB[B](f: T => B)(numTokens:Int, numTrainDocs:Int, compareWith:Int)(classifications:Vector[Double], features:Matrix[Double], weightedspamcount:DeliteDouble)(implicit pFact: DeliteProxyFactory[Vector[B]], c: ClassManifest[B]) : Vector[B] = {
    throw new RuntimeException("mapNB : This operation should be overriden")
  }

  // Map operation for Linear Regression
  /*
  def mapLR[B](f: T => B)(x_cur:T, tau:Int)(implicit pFact: DeliteProxyFactory[Vector[B]], c: ClassManifest[B]) : Vector[B] = {
    throw new RuntimeException("mapLR : This operation should be overriden")
  }
  */

  // 1'st Map operation for K-means
  def mapKM1[B](f: T => B)(x:Matrix[Double], mu:Matrix[Double])(implicit pFact: DeliteProxyFactory[Vector[B]], c: ClassManifest[B]) : Vector[B] = {
    throw new RuntimeException("mapKM1 : This operation should be overriden")
  }

  // 2'nd Map operation for K-means
  def mapKM2[B](f: T => B)(x:Matrix[Double], mu:Matrix[Double], cc:Vector[Int])(implicit pFact: DeliteProxyFactory[Vector[B]], c: ClassManifest[B]) : Vector[B] = {
    throw new RuntimeException("mapKM2 : This operation should be overriden")
  }
  */

  /////////////////////////////////////
  // RBM specific operations

  // RBM : operation that replicates the given matrix to generate a larger matrix
  def repmat(i: Int, j: Int): Matrix[T] = {
    throw new RuntimeException("repmat : This operation should be overriden")
  }


  /////////////////
  // error handlers

  protected def vecDimError(){
    throw new IllegalArgumentException("vector dimensions must agree")
  }

  protected def chkUpdate(){
    if (frozen) {
      throw new IllegalArgumentException("vector is frozen")
    }
  }

  protected def chkLength(v: Vector[T], v2: Vector[T]) = {
    if (v.length != v2.length) vecDimError()
  }

  protected def chkBounds(v: Vector[T], v2: Vector[T]) = {
    if (v.length != v2.length || v.is_row != v2.is_row) vecDimError()
  }

  protected def chkVecMatAgree(v: Vector[T], m: Matrix[T]) = {
    if (v.length != m.numRows){
      Console.printf("error: vector-matrix dimensions [1][%d]*[%d][%d] don't agree\n", length, m.numRows, m.numCols)
      throw new IllegalArgumentException
    }
    else if (!v.is_row && m.numRows != 1){
      Console.printf("error: matrix-vector dimensions [%d][1]*[%d][%d] don't agree\n", length, m.numRows, m.numCols)
      throw new IllegalArgumentException
    }
  }

  protected def chkRange(begin: Int, end: Int) {
    if (begin < 0 || end < begin || end > length) throw new IndexOutOfBoundsException
  }

  protected def chkIndex(index: Int) : Int = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException
    index
  }

  protected def chkPos(index: Int) : Int = {
    if (index < 0 || index > length) throw new IndexOutOfBoundsException
    index
  }

}
