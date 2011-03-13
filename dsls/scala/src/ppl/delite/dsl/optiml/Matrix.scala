/* Matrix trait
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: 2/24/10
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

package ppl.delite.dsl.optiml

import scala.reflect.ClassManifest
import scala.collection.mutable.Cloneable

import ppl.delite.core.ops._
import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.core._
import ppl.delite.core.ops.specialized._
import ppl.delite.dsl.primitive.DeliteBoolean
import ppl.delite.dsl.optiml.specialized._
import ppl.delite.cuda._
import ppl.delite.cnative._

object Matrix {

  val numTrunks = 1024

  // user visible methods
  def apply[A : ClassManifest](xs: Vector[Vector[A]]) = newMatrix(xs)
  def apply[A : ClassManifest](xs: Vector[A]*) = newMatrix(Vector(xs: _*))
  def apply[A : ClassManifest](numRows: Int, numCols: Int) = newMatrix[A](numRows,numCols)
  def apply[A : ClassManifest](numRows: Int, numCols: Int, block:(Int,Int) => A) = newStreamingMatrix[A](numRows,numCols,block)

  // special apply method for GPU use (Generate Impl with given array and given row/col 
  def apply[A : ClassManifest](xs: Array[A], numRows: Int, numCols: Int) = newMatrix[A](xs, numRows, numCols)

  def fromMap[A,T : ClassManifest]( v: Vector[A], f: A => Vector[T]) : Matrix[T] = {
    Delite.run(OP_fromMap(v, f))
  }

  def identity( w : Int ) : Matrix[Double] = {
    return diag(w, Vector.ones(w))
  }

  def diag( w : Int, vals : Vector[Double] ) : Matrix[Double] = {
    if(Config.executorType == "gpu") {
      Delite.run(DoubleMatrix.OPGPU_diag_single(w, vals.asInstanceOf[DoubleVector]))
    }
    else
      Delite.run(DoubleMatrix.OP_diag_single(w, vals.asInstanceOf[DoubleVector]))
  }

  def zeros( rows : Int, cols : Int) : Matrix[Double] = {
    Delite.run(OP_zeros(rows, cols))  
  }

  def zerosf( rows : Int, cols : Int) : Matrix[Float] = {
    Delite.run(OP_zerosf(rows, cols))
  }

  def ones( rows : Int, cols : Int) : Matrix[Double] = {
    Delite.run(OP_ones(rows, cols))
  }

  def randf( rows: Int, cols : Int) : Matrix[Float] = {
    if(Config.executorType == "gpu")
      Delite.run(OPGPU_randf(rows, cols))
    else {
	  //if(Config.useNativeLibs)
      //	Delite.run(OP_native_randf(rows, cols))
	  //else
      	Delite.run(OP_randf(rows, cols))
	}
  }
  def rand( rows : Int, cols : Int) : Matrix[Double] = {
    Delite.run(OP_rand(rows, cols))
  }
  def randn( rows : Int, cols : Int) : Matrix[Double] = {
    Delite.run(OP_randn(rows, cols))
  }

  def randnf( rows : Int, cols : Int) : Matrix[Float] = {
    Delite.run(OP_randnf(rows, cols))
  }

  def eig(m: Matrix[Double]) : Vector[Matrix[Double]] = {
    EigenOps.eigenSolver(m)
  }

  //////////////////////
  // Matrix construction

  private def newMatrix[A](numRows: Int, numCols: Int)(implicit m: ClassManifest[A]): Matrix[A] = {
    m match {
      case ClassManifest.Double => new DoubleMatrixImpl(numRows, numCols).asInstanceOf[Matrix[A]]
      case ClassManifest.Float => new FloatMatrixImpl(numRows, numCols).asInstanceOf[Matrix[A]]
      case ClassManifest.Int => new IntMatrixImpl(numRows, numCols).asInstanceOf[Matrix[A]]
      case ClassManifest.Long => new LongMatrixImpl(numRows, numCols).asInstanceOf[Matrix[A]]
      case ClassManifest.Boolean => new BooleanMatrixImpl(numRows, numCols).asInstanceOf[Matrix[A]]
      case _ => new MatrixImpl[A](numRows, numCols)
    }
  }

  private def newStreamingMatrix[A](numRows: Int, numCols: Int, block:(Int,Int) => A)(implicit m: ClassManifest[A]): Matrix[A] = {
    m match {
      case ClassManifest.Double => new StreamingDoubleMatrixImpl(numRows, numCols, block.asInstanceOf[(Int,Int)=>Double], numTrunks).asInstanceOf[Matrix[A]]
      case _=> throw new UnsupportedOperationException("No StreamingMatrixImpl for the given type")
    }
  }

  private def newMatrix[A](implicit m: ClassManifest[A]): Matrix[A] = {
    m match {
      case ClassManifest.Double => new DoubleMatrixImpl().asInstanceOf[Matrix[A]]
      case ClassManifest.Float => new FloatMatrixImpl().asInstanceOf[Matrix[A]]
      case ClassManifest.Int => new IntMatrixImpl().asInstanceOf[Matrix[A]]
      case ClassManifest.Long => new LongMatrixImpl().asInstanceOf[Matrix[A]]
      case ClassManifest.Boolean => new BooleanMatrixImpl().asInstanceOf[Matrix[A]]
      case _ => new MatrixImpl[A]()
    }
  }

  private def newMatrix[A](xs: Array[A], numRows: Int, numCols: Int)(implicit m: ClassManifest[A]): Matrix[A] = {
    m match {
      case ClassManifest.Double => new DoubleMatrixImpl(xs.asInstanceOf[Array[Double]], numRows, numCols).asInstanceOf[Matrix[A]]
      case ClassManifest.Float => new FloatMatrixImpl(xs.asInstanceOf[Array[Float]], numRows, numCols).asInstanceOf[Matrix[A]]
      case ClassManifest.Int => new IntMatrixImpl(xs.asInstanceOf[Array[Int]], numRows, numCols).asInstanceOf[Matrix[A]]
      case ClassManifest.Long => new LongMatrixImpl(xs.asInstanceOf[Array[Long]], numRows, numCols).asInstanceOf[Matrix[A]]
      case ClassManifest.Boolean => new BooleanMatrixImpl(xs.asInstanceOf[Array[Boolean]], numRows, numCols).asInstanceOf[Matrix[A]]
      case _ => new MatrixImpl[A]()
    }
  }

  // could be written as a map, but probably not enough work to be valuable
  // if update has a lot of overhead, then we may need to reconsider
  private def newMatrix[A : ClassManifest](data: Vector[Vector[A]], frozen: Boolean = false): Matrix[A] = {
    val numRows = data.length
    val numCols = if (data.length > 0) data(0).length else 0
    val out = newMatrix[A](numRows, numCols)
    for (i <- 0 until numRows){
      for (j <- 0 until numCols){
        out(i,j) = data(i)(j)
      }
    }
    if (frozen) out.freeze()
    out
  }

  // goal: construct a new matrix without copying. currently, flatten copies
  /*
  private def newMatrixFromView[A : ClassManifest](data: Vector[Vector[A]]) : Matrix[A] = {
    val numRows = data.length
    val numCols = if (data.length > 0) data(0).length else 0
    val out = newMatrix(numRows, numCols)
    // TODO: can we wire this up in a cheaper way?
    val v = Vector.flatten[A](data)
    out._data = v._data
  }
  */

  //////////
  // delite

  // This is the proxy object handed back whenever computation has been deferred and the result type is Matrix
  // Proxy wraps an op, which (if needed) uses a factory to construct and return a specialized type
  // Calling force uses DELITE magic to execute the op
  /*
  class Proxy[T] extends DeliteProxy[Matrix[T]] with Matrix[T] {
    override def nodeAttr = "[shape=box]"
    override def toString = "Matrix" + super.toString

    def elementByteSize: Long = force.elementByteSize

    def dc_update(i: Int, x: T) = force.dc_update(i,x)
    def dc_apply(i: Int) = force.dc_apply(i)
    
    def mutableClone = force.mutableClone
    def lifted_apply(i: Int, j: Int) = force.lifted_apply(i,j)
    def apply(i: Int, j: Int) = force.apply(i,j)
    def vview(start: Int, stride: Int, length: Int, is_row: Boolean) = force.vview(start,stride,length,is_row)
    def lifted_update(i: Int, j: Int, x: T) = force.lifted_update(i,j,x)
    def update(i: Int, j: Int, x: T) = force.update(i,j,x)
    def insertRow[A <: T](pos: Int, x: Vector[A])= force.insertRow(pos,x)
    def insertAllRows[A <: T](pos: Int, xs: Matrix[A]) = force.insertAllRows(pos,xs)
    def insertCol[A <: T](pos: Int, x: Vector[A]) = force.insertCol(pos,x)
    def insertAllCols[A <: T](pos: Int, xs: Matrix[A]) = force.insertAllCols(pos,xs)
    def removeRows(pos: Int, len: Int) = force.removeRows(pos,len)  
    def removeCols(pos:Int, len: Int) = force.removeCols(pos,len)
  }
  */

  class ProxyFactory[T: ClassManifest] extends DeliteProxyFactory[Matrix[T]] {
    def newProxy = newMatrix[T]
  }

  class Builder extends DeliteCollectionFactory[Matrix] {
    def newInstance[A](size: Int, shape: Option[Any])(implicit m: ClassManifest[A]) = {
      assert(shape.isInstanceOf[Option[Tuple2[Int,Int]]])
      assert(shape.isEmpty == false)
      val s = shape.asInstanceOf[Option[Tuple2[Int,Int]]].get
      assert(s._1*s._2 == size)
      Matrix[A](s._1, s._2)
    }
  }

  /*
   *  The following are the OPs that this DSL defines, the purpose of OPs is delayed execution and optimization
   */

  /* TODO: Not sure how to represent this with DeliteOP_Map
   * TODO: Vector[A] => Vector[Vector[T]] => Matrix[T]
   * TODO: could be done with an implicit conversion and another type parameter, but it's a bit ugly
   */
  protected[optiml] case class OP_fromMap[A,T : ClassManifest](v: Vector[A], f: A => Vector[T])
    extends DeliteOP_SingleTask[Matrix[T]](v) {

    def task = {
      assert(v.length > 0)

      val stride = f(v(0)).length
      val coll = Matrix[T](v.length, stride)

      var i = 0
      while (i != v.length) {
        val row = f(v(i))
        assert(row.length == stride)
        var j = 0
        while (j != row.length) {
          coll(i,j) = row(j)
          j += 1
        }
        i += 1
      }
      coll
    }

    override def toString = "Matrix OP_fromMap"//  + ":" + hex8(this)
    //override def toString = "Matrix OP_fromMap" + "(line " + getUserLineNumber + ")"
  }

  protected[optiml] case class OP_zeros( numRows : Int, numCols : Int)
    extends DeliteOP_Map[Double,Double,Matrix] {

    val coll = Matrix[Double](numRows,numCols)
    val out = coll
    def func = e => 0.0
  }

  protected[optiml] case class OP_zerosf( numRows : Int, numCols : Int)
    extends DeliteOP_Map[Float,Float,Matrix] {

    val coll = Matrix[Float](numRows,numCols)
    val out = coll
    def func = e => 0
  }

  protected[optiml] case class OP_ones( numRows : Int, numCols : Int)
    extends DeliteOP_Map[Double,Double,Matrix]{

    val coll = Matrix[Double](numRows, numCols)
    val out = coll
    def func = e => 1.0
  }

  protected[optiml] case class OP_rand( numRows : Int, numCols : Int)
    extends DeliteOP_Map[Double,Double,Matrix]{

    val coll = Matrix[Double](numRows, numCols)
    val out = coll
    def func = e => random[Double]
  }

  protected[optiml] case class OP_randf( numRows : Int, numCols : Int)
    extends DeliteOP_MapSpec[Float,Matrix]{

    val coll = Matrix[Float](numRows, numCols)
    val out = coll
    def func = e => random[Float]
  }

  protected[optiml] case class OP_native_randf( numRows : Int, numCols : Int)
    extends DeliteOP_NativeMapSpec[Float, Matrix]{

    val coll = Matrix[Float](numRows, numCols)
	val out = coll 
    def funcNative(start:Int, end:Int) = DeliteNative.vecRandFloat(out.asInstanceOf[FloatMatrix]._data, start, end)
    def func = null
  }

  protected[optiml] case class OP_randn( numRows : Int, numCols : Int)
    extends DeliteOP_Map[Double,Double,Matrix]{

    val coll = Matrix[Double](numRows, numCols)
    val out = coll
    def func = e => randomGaussian
  }

   protected[optiml] case class OP_randnf( numRows : Int, numCols : Int)
    extends DeliteOP_Map[Float,Float,Matrix]{

    val coll = Matrix[Float](numRows, numCols)
    val out = coll
    def func = e => randomGaussian.floatValue()
  }

  protected[optiml] case class OPGPU_randf( numRows : Int, numCols : Int)
    extends DeliteOP_SingleTask[Matrix[Float]]{

    def task = {
      import ppl.delite.core.executor.Profiler
      Profiler.start("RAND")
      val out = Matrix[Float](numRows, numCols)
      var i = 0
      var j = 0
      while(i < numRows) {
        j = 0
        while(j < numCols) {
          out(i,j) = random[Float]
          j += 1
        }
        i += 1
      }
      Profiler.stop("RAND")
      out
    }
    /*
    //override def getGPUInputs = null
    override def getGPUOutput = {
      val elms = new Array[Float](0)
      val gout = Matrix[Float](elms, m.numRows, m.numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelId = List(DeliteCuda.MatRandFloat, DeliteCuda.AsyncRand)
    */
  }

  // TODO: convert to a MapReduce?
  protected[optiml] case class OP_cmp[A](m1: Matrix[A], m2: Matrix[A])
    extends DeliteOP_SingleTask[DeliteBoolean](m1,m2) {

    def task = {
      m1.chkBounds(m2)
      var res = true
      for (i <- 0 until m1.numRows){
        for (j <- 0 until m1.numCols){
          if (m1(i,j) != m2(i,j)) res = false
        }
      }
      DeliteBoolean(res)
    }
  }

  protected[optiml] case class OP_getRow[A](m: Matrix[A], row: Int) extends DeliteOP_SingleTask[Vector[A]](m){
    def task = m.vview(row*m._numCols, 1, m._numCols, true)
  }

  protected[optiml] case class OP_getCol[A](m: Matrix[A], col: Int) extends DeliteOP_SingleTask[Vector[A]](m){
    def task = m.vview(col, m._numCols, m._numRows, false)
  }

  protected[optiml] case class OP_sliceRows[A](m: Matrix[A], begin: Int, end: Int)(implicit c: ClassManifest[A])
    extends DeliteOP_SingleTask[Matrix[A]] {

    def task = {
      m.chkRange(begin, end)
      val out = newMatrix[A](end-begin, m._numCols)
      var i = begin
      while (i < end) {
        var j = 0
        while (j < m._numCols) {
          out(i-begin, j) = m(i,j)
          j += 1
        }
        i += 1
      }
      out
    }
  }

  protected[optiml] case class OP_slice2d[A](m: Matrix[A], beginrow: Int, endrow: Int, begincol: Int, endcol: Int)(implicit c: ClassManifest[A])
    extends DeliteOP_SingleTask[Matrix[A]] {

    def task = {
      m.chkRange(beginrow, endrow)
      // Add check for col out of bounds
      val out = newMatrix[A](endrow-beginrow, endcol-begincol)
      var i = beginrow
      while (i < endrow) {
        var j = begincol
        while (j < endcol) {
          out(i-beginrow, j-begincol) = m(i,j)
          j += 1
        }
        i += 1
      }
      out
    }
  }

  protected[optiml] case class OP_updateRow[A](m: Matrix[A], row: Int, x: Vector[A])
    extends DeliteOP_MutableSingleTask[DeliteUnit](x)(m) {

    def task = {
      m.chkUpdate;
      m.chkEquals(x.length, m.numCols)
      val vv = m.vview(row*m.numCols, 1, m.numCols, true)
      // TODO: should be a mutable map on the view
      for (i <- 0 until x.length){
        vv(i) = x(i)
      }
    }
  }

  /**
   * arithmetic ops
   */

  protected[optiml] case class OP_trans[A](m: Matrix[A])(implicit c: ClassManifest[A])
    extends DeliteOP_SingleTask[Matrix[A]](m) {

    // naive, should block
    def task = {
      val out = Matrix[A](m.numCols, m.numRows)
      for (i <- 0 until out.numRows){
        for (j <- 0 until out.numCols){
          out(i,j) = m(j,i)
        }
      }
      out
    }
  }

  protected[optiml] case class OP_+[A](val collA: Matrix[A], val collB: Matrix[A], val out: Matrix[A])(implicit ops: ArithOps[A], c: ClassManifest[A])
    extends DeliteOP_ZipWith2[A,A,A,Matrix]{
    override val associative = true

    def func = (a,b) => ops.+(a,b)
  }

  protected[optiml] case class OP_+=[A](val collA: Matrix[A], val collB: Matrix[A])(implicit ops: ArithOps[A], c: ClassManifest[A])
    extends DeliteOP_ZipWith2[A,A,A,Matrix]{
    val out = collA
    def func = (a,b) => ops.+(a,b)
  }

  protected[optiml] case class OP_-[A](val collA: Matrix[A], val collB: Matrix[A], val out: Matrix[A])(implicit ops: ArithOps[A], c: ClassManifest[A])
    extends DeliteOP_ZipWith2[A,A,A,Matrix]{

    def func = (a,b) => ops.-(a,b)
  }

  protected[optiml] case class OP_/[A](val collA: Matrix[A], val collB: Matrix[A], val out: Matrix[A])(implicit ops: ArithOps[A], c: ClassManifest[A])
    extends DeliteOP_ZipWith2[A,A,A,Matrix]{

    def func = (a,b) => ops./(a,b)
  }

  protected[optiml] case class OP_mdot[A](val collA: Matrix[A], val collB: Matrix[A], val out: Matrix[A])(implicit ops: ArithOps[A], c: ClassManifest[A])
    extends DeliteOP_ZipWith2[A,A,A,Matrix]{

    def func = (a,b) => ops.*(a,b)
  }

  protected[optiml] case class OP_vdot[A](m: Matrix[A], v: Vector[A])(implicit ops: ArithOps[A], c: ClassManifest[A])
    extends DeliteOP_SingleTask[Matrix[A]]{

    /*
    var tcoll = Vector[Vector[A]](false, m.numRows)
    for (i <- 0 until m.numRows){
      tcoll(i) = m.getRow(i)
    }

    val collA = tcoll

    // result is Vector[Vector[A]], needs to be Matrix
    def func = (row,e) => row*e
    */
    def task = {
      m.chkEquals(m.numRows, v.length)
      Matrix.fromMap[Int, A](m.indices, e => m(e)*v(e))
    }

  }

  /*
  protected[optiml] case class OP_*[A](m1: Matrix[A], m2: Matrix[A])(implicit ops: ArithOps[A], pFact: Matrix.ProxyFactory[A], c: ClassManifest[A])
    extends DeliteOP_SingleTask[Matrix[A]](m1,m2){

    def task = {
      m1.chkEquals(m1.numCols, m2.numRows)
      val b_trans = m2.trans
      m1.mapRow(a_row => b_trans.mapToVec(b_row => b_row._dot(a_row)))
    }
  }
  */

  /*
  protected[optiml] case class OP_*[A](m1: Matrix[A], m2: Matrix[A])(implicit ops: ArithOps[A], pFact: Matrix.ProxyFactory[A], c: ClassManifest[A])
    extends DeliteOP_Map[Vector[A], Vector[A], Vector]{

    m1.chkEquals(m1.numCols, m2.numRows)
    val b_trans = m2.trans
    var tcoll = Vector[Vector[A]](false, m1.numRows)
    for (i <- 0 until m1.numRows){
      tcoll(i) = m1.vview(i*m1.numCols, 1, m1.numCols, true)
    }

    val out = Vector[Vector[A]](false, m1.numRows)
    val coll = tcoll
    def func = DeliteFunc(a_row => {
      val out = Vector[A](b_trans._numRows)
      var i = 0
      while (i < out._length){
        out(i) = b_trans(i)._dot(a_row)
        i += 1
      }
      out
    }, m1, m2, b_trans)
  }
  */

  protected[optiml] case class OP_*[A](m1: Matrix[A], m2: Matrix[A])(implicit ops: ArithOps[A], pFact: Matrix.ProxyFactory[A], c: ClassManifest[A])
    extends DeliteOP_ForEach[Int, Matrix[A]](m1, m2)(){

    // TODO: all this has to be lifted, -> can't be in the constructor
    m1.chkEquals(m1.numCols, m2.numRows)
    val b_trans = m2.trans

    val coll = Vector.range(0, m1.numRows)
    val out = Matrix[A](m1.numRows, m2.numCols)

    def func = DeliteFunc((row_idx => {
      var i = 0
      while (i < out._numCols){
        var j = 1
        var acc = ops.*(m1(row_idx,0),b_trans(i,0))
        while (j < b_trans._numCols){
          acc = ops.+(acc, ops.*(m1(row_idx,j),b_trans(i,j)))
          j += 1
        }
        out(row_idx, i) = acc
        i += 1
      }
    }), b_trans)
  }
  //TODO
  protected[optiml] case class OP_SMprod[A](m1: Matrix[A], m2: SparseMatrix[A])(implicit ops: ArithOps[A], pFact: Matrix.ProxyFactory[A], c: ClassManifest[A])
    extends DeliteOP_ForEach[Int, Matrix[A]](m1, m2)(){

    // TODO: all this has to be lifted, -> can't be in the constructor
    m1.chkEquals(m1.numCols, m2.numRows)
    //val b_trans = m2.trans

    val coll = Vector.range(0, m1.numRows)
    val out = Matrix[A](m1.numRows, m2.numCols)

    def func = DeliteFunc((row => {
      //u can use transpose to do this very easily
      var i = 0
      while (i < out._numCols){
        var row_idx = m2.CSC_col_idx_apply(i)
        var temp = i + 1
        var next_col = m2.CSC_col_idx_apply(temp)
        while (next_col == -1){
          temp += 1
          next_col = m2.CSC_col_idx_apply(temp)
        } 
        if (row_idx != -1){
          var j = 0
          var acc = ops.zero
          while (row_idx < next_col){
            j = m2.CSC_row_idx_apply(row_idx)
            acc = ops.+(acc, ops.*(m1(row, j),m2.dc_apply(row_idx)))
            row_idx += 1
          }
          out(row, i) = acc
        }
        i = temp
      }
    }))
  }

  protected[optiml] case class OP_vprod[A](m: Matrix[A], v: Vector[A], val out: Vector[A])(implicit ops: ArithOps[A], pFact: Vector.ProxyFactory[A], c: ClassManifest[A])
    extends DeliteOP_Map[Vector[A], A, Vector]{

    //TODO: all this has to be lifted, -> can't be in the constructor

    //if (v.is_row) throw new IllegalArgumentException

    var tcoll = Vector[Vector[A]](false, m.numRows)
    for (i <- 0 until m.numRows){
      tcoll(i) = m.getRow(i)
    }

    val coll = tcoll
    def func = row => row._dot(v)
  }

  protected[optiml] case class OP_unary_-[A](val coll: Matrix[A], val out: Matrix[A])(implicit ops: ArithOps[A], pFact: Matrix.ProxyFactory[A], c: ClassManifest[A])
    extends DeliteOP_Map[A, A, Matrix] {

    def func = e => ops.unary_-(e)
  }

  protected[optiml] case class OP_abs[A](val coll: Matrix[A], val out: Matrix[A])(implicit ops: ArithOps[A], pFact: Matrix.ProxyFactory[A], c: ClassManifest[A])
    extends DeliteOP_Map[A, A, Matrix] {

    def func = e => ops.abs(e)
  }

  protected[optiml] case class OP_exp[A](val coll: Matrix[A], val out: Matrix[A])(implicit ops: ArithOps[A], pFact: Matrix.ProxyFactory[A], c: ClassManifest[A])
    extends DeliteOP_Map[A, A, Matrix] {

    def func = e => ops.exp(e)
  }

  protected[optiml] case class OP_sum[A,B <: DeliteDSLType](val coll: Matrix[A])
    (implicit ops: ArithOps[A], conv: A => B, pfact: DeliteProxyFactory[B], c: ClassManifest[A])
    extends DeliteOP_Reduce[A,B] {

    def func = (a,b) => ops.+(a,b)
  }

  protected[optiml] case class OP_>[A](val collA: Matrix[A], val collB: Matrix[A], val out: Matrix[A])(implicit ops: ArithOps[A], c: ClassManifest[A])
    extends DeliteOP_ZipWith2[A,A,A,Matrix]{

    def func = (a,b) => ops.>(a,b)
  }

  protected[optiml] case class OP_<[A](val collA: Matrix[A], val collB: Matrix[A], val out: Matrix[A])(implicit ops: ArithOps[A], c: ClassManifest[A])
    extends DeliteOP_ZipWith2[A,A,A,Matrix]{

    def func = (a,b) => ops.<(a,b)
  }

  protected[optiml] case class OP_min[A,B <: DeliteDSLType](val coll: Matrix[A])
    (implicit cmp: A => Ordered[A], conv: A => B, pfact: DeliteProxyFactory[B]) extends DeliteOP_Reduce[A,B] {

    def func = (a,b) => if (cmp(a) < b) a else b
  }

  protected[optiml] case class OP_max[A,B <: DeliteDSLType](val coll: Matrix[A])
    (implicit cmp: A => Ordered[A], conv: A => B, pfact: DeliteProxyFactory[B]) extends DeliteOP_Reduce[A,B] {

    def func = (a,b) => if (cmp(a) > b) a else b
  }

  protected[delite] case class OP_inv[A](m: Matrix[A])(implicit conv: A => Double) extends DeliteOP_SingleTask[Matrix[Double]](m) {
    def task = {
      m.chkEquals(m.numCols, m.numRows)

      var tmpMat = m.mutableClone

      // augment the Matrix with the identity Matrix of the same size
      val id = Matrix.identity(m.numCols)
      val augMat = tmpMat.toDouble.insertAllCols(tmpMat.numCols, id)
      try{
        // perform row reductions
        val redMat = augMat.rreduce(e => e)
        // strip off the augmented Matrix
        redMat.removeCols(0, m.numCols)
        if (m.frozen) redMat.freeze
        redMat
      }catch{
        case e: Exception => {
          println("Error: Matrix appears to be singular")
          throw new IllegalArgumentException
        }
      }
    }

    // Here, we assume that the matrix size is only (2*2) : Used by LinReg App
    override def getGPUInputs = List(m)
    override def getGPUConsts = List(0, 0)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Matrix[Double](elms, m._numRows, m._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(m._numRows*m._numCols)
    override def getGPUKernelId = List(DeliteCuda.MatInv, DeliteCuda.AsyncLaunch1D)
    //override def getGPUKernelId = {List(DeliteCuda.MatInv, DeliteCuda.Async3D2I)}
  }

  protected[optiml] case class OP_rreduce[A](m: Matrix[A])(implicit conv: A => Double) extends DeliteOP_SingleTask[Matrix[Double]](m) {
    private def refreeze(old_m: Matrix[A], new_m: Matrix[Double]) = {
      if (old_m.frozen) new_m.freeze()
      new_m
    }

    def task : Matrix[Double] = {
      var currentMat = m.mutableClone.toDouble
      var lead: Int = 0

      for (r <- 0 until m.numRows) {
        if (m.numRows <= lead)
          return refreeze(m, currentMat)

        var i = r
        while (currentMat(i, lead) == 0.0){
          i += 1
          if (m.numCols == i){
            i = r
            lead += 1
            if (m.numRows == lead)
              return refreeze(m, currentMat)
          }
        }

        val tmpRow = currentMat(i)
        currentMat(i) = currentMat(r)
        currentMat(r) = tmpRow

        currentMat(r) = currentMat(r) / currentMat(r,lead)

        for (i <- 0 until m.numRows){
          if (i != r)
            currentMat(i) = currentMat(i) - currentMat(r)*currentMat(i,lead)
        }
        lead += 1
      }

      refreeze(m, currentMat)
    }
  }

  protected[optiml] case class OP_map[A, B : ClassManifest](val coll: Matrix[A], val out: Matrix[B], val func: A => B)
    extends DeliteOP_Map[A,B,Matrix]

  protected[optiml] case class OP_mutMap[A](val coll: Matrix[A], val out: Matrix[A], val func: A => A)
    extends DeliteOP_Map[A,A,Matrix] {

    override def getMutableDeps = Seq(out) 
  }

  protected[optiml] case class OP_zipWith[A, B : ClassManifest](val collA: Matrix[A], val collB: Matrix[A], val out: Matrix[B], val func: (A,A) => B)
    extends DeliteOP_ZipWith2[A,A,B,Matrix]


  /* A convenience method equivalent for Vector.fromMap */
  protected[optiml] case class OP_mapToVec[A,B](m: Matrix[A], val out: Vector[B], val func: Vector[A] => B, is_row: Boolean)(implicit pFact: Vector.ProxyFactory[B], c: ClassManifest[B])
    extends DeliteOP_Map[Vector[A],B,Vector] {

    var tcoll = Vector[Vector[A]](false, m.numRows)
    for (i <- 0 until m.numRows){
      tcoll(i) = m.getRow(i)
    }

    val coll = tcoll
  }

  protected[optiml] case class OP_alloc[A,B: ClassManifest](m: Matrix[A]) extends DeliteOP_SingleTask[Matrix[B]](m) {
    def task = {
      Matrix[B](m.numRows, m.numCols)
    }
  }

  protected[optiml] case class OP_allocVec[A,B: ClassManifest](m: Matrix[A], is_row: Boolean) extends DeliteOP_SingleTask[Vector[B]](m) {
    def task = {
      Vector[B](is_row, m.numRows)
    }
  }
  
  protected[optiml] case class OP_allocSize[A: ClassManifest](numRows:Int, numCols: Int) extends DeliteOP_SingleTask[Matrix[A]] {
    def task = {
      Matrix[A](numRows, numCols)
    }
  }
  
  /*
  * The following are optimization artifacts that DELITE can use to apply Domain
  * specific optimization to the resulting DAG from
  */
  /*
  trait opt[A] extends DelitePatternMatchingOptimizer[Matrix[A]] {

    def optimize(op: DeliteOP[Matrix[A]]) = {
      op match {
        case outerOp@Vector.OP_outer(P(transOp@Vector.OP_trans(v1)), v2) => {
          //transOp.proxy.isKilled = true
          //OPsExecutor.pendgOPs -= 1
          //println("[Matrix.optimize] Found vector trans and outer product.")
          //val outerOp = op.asInstanceOf[Vector.OP_outer[A]]
          //Vector.OP_transOuter(v1, v2)(outerOp.ops, outerOp.vFact, outerOp.mFact, outerOp.mStrat)
          op
        }

        case _ => {
          //println("[Matrix.optimize] Found op instance: " + op)
          op
        }
      }
    }
  }
  */
}

trait Matrix[@specialized(Double,Float,Int) T] extends DeliteCollection[T] with Cloneable[Matrix[T]] {
  import Matrix._

  //type DSLType = Matrix[T]
  type Shape = Tuple2[Int,Int]

  //implicit def proxyFactory[T: ClassManifest] = new ProxyFactory[T]
  //implicit def vecProxyFactory[T: ClassManifest] = new Vector.ProxyFactory[T]
  //implicit def vecVecProxyFactory[T: ClassManifest] = new Vector.ProxyFactory[Vector[T]]

  //override val optimizer = Some(new opt[T] {})

  ////////////////////////////////
  // DeliteCollection

  def size = numRows*numCols

  override def shape = Option((numRows,numCols))

  def chunk(start: Int, end: Int) = new Iterator[T] {
    private var index = start
    private val ceil = if (end+1 <= Matrix.this.size) end+1 else Matrix.this.size+1  //this is an error
    def hasNext = (index < ceil)
    def next = {
      index += 1
      dc_apply(index-1)
    }
  }

  // public (read-only) properties
  def numRows : Int
  def numCols : Int
  def frozen: Boolean

  // for internal use only
  // UNSAFE! Will throw a NPE if used on a proxy -- USE WITH CAUTION
  /*protected*/ def _numRows : Int
  /*protected*/ def _numRows_=(n: Int) : Unit
  /*protected*/ def _numCols : Int
  /*protected*/ def _numCols_=(n: Int) : Unit
  protected def _frozen : Boolean
  protected def _frozen_=(b: Boolean) : Unit

  /////////////
  // accessors

  def apply(i: Int) = getRow(i)
  def $(i: Int, j: Int) = lifted_apply(i,j)
  def lifted_apply(i: Int, j: Int) : T
  def apply(i: Int, j: Int) : T

  def initialize(row:Int, col:Int):T = {
    throw new UnsupportedOperationException("initialize of Matrix[T]")
  }
  
  def flattened(i: Int) : T = dc_apply(i)

  // should be frozen if matrix is frozen
  def vview(start: Int, stride: Int, length: Int, is_row: Boolean) : Vector[T]

  def indices : Vector[Int] = {
    return Vector.range(0, numRows)
  }

  /*
   * getRow and getCol return *views*, which are backed by the actual
   * Matrix data. To get a separate copy, use e.g. getRow.clone
   */
  def getRow(row: Int) : Vector[T] = {
    vview(row*_numCols, 1, _numCols, true)
  }
  def $(i: Int)(implicit c: ClassManifest[T]) = lifted_getRow(i)
  def lifted_getRow(i: Int)(implicit c: ClassManifest[T]) : Vector[T] = {
    run(OP_getRow(this,i))(new Vector.ViewProxyFactory)
  }
  def getCol(col: Int) : Vector[T] = {
    vview(col, numCols, numRows, false)
  }
  def $c(j: Int)(implicit c: ClassManifest[T]) = lifted_getCol(j)
  def lifted_getCol(j: Int)(implicit c: ClassManifest[T]) : Vector[T] = {
    run(OP_getCol(this,j))(new Vector.ViewProxyFactory)
  }

  //def takeBottomRows(count: Int): Matrix[T] = sliceRows(count, h)
  //def takeTopRows(count: Int): Matrix[T] = sliceRows(0, count)

  /* Get rows from [begin, end) */
  def sliceRows(begin: Int, end: Int)(implicit c: ClassManifest[T]): Matrix[T] = {
    run(OP_sliceRows(this,begin,end))
  }

  def slice2d(beginrow: Int, endrow: Int, begincol: Int, endcol: Int)(implicit c: ClassManifest[T]): Matrix[T] = {
    run(OP_slice2d(this,beginrow,endrow,begincol,endcol))
  }

  //def sliceRows(indices: IndexVector) : Matrix[T]

  ///////////////
  // conversions

  def toDouble(implicit conv: T => Double) = map(e => conv(e))
  def toFloat(implicit conv: T => Float) = map(e => conv(e))
  def toInt(implicit conv: T => Int) = map(e => conv(e))
  def toLong(implicit conv: T => Long) = map(e => conv(e))

  ///////////
  // general

  /* transpose */
  def trans(implicit c: ClassManifest[T]) : Matrix[T] = {
    run(OP_trans(this))
  }

  /* Pretty print */
  def pprint = {
    for (i <- 0 until numRows){
      print("[ ")
      for (j <- 0 until numCols){
        print(this(i,j))
        print(" ")
      }
      print("]\n")
    }
  }

  //override def equals(that: Any) = that match {
  // TODO: fix hack, see Memoizer line 35 for explanation
  /*
  override def eqhack(that: Any) = that match {
    case m:Matrix[T] => {
      if (m.numRows != numRows || m.numCols != numCols) false
      for (i <- 0 until numRows){
        if (m(i).equals(this(i)) == false) false
      }
      true
    }
    case _ => false
  }
  */

  //////////////
  // life cycle

  def freeze() : Matrix[T] = {
    _frozen = true
    this
  }

  override def clone: Matrix[T] = {
    val ret = mutableClone
    if (frozen) ret.freeze()
    return ret
  }
  def mutableClone: Matrix[T]


  ///////////////////
  // data operations

  def :=(row: Int, x: Vector[T]) = lifted_updateRow(row,x)
  def lifted_updateRow(row: Int, x: Vector[T]) = {
    run(OP_updateRow(this,row,x))
  }
  def updateRow(row: Int, x: Vector[T]) {
    //chkUpdate;
    //chkEquals(x.length, numCols)
    
    // TODO: could be parallelized using a view
    var j = 0
    while(j < _numCols){
      this(row,j) = x(j)
      j += 1
    }
  }

  def :=(i: Int, j: Int, x: T) = lifted_update(i, j, x)
  def lifted_update(i: Int, j: Int, x: T)
  def update(i: Int, j: Int, x: T)
  def update(i: Int, x: Vector[T]) = updateRow(i, x)

  def +=(x: Vector[T]): Matrix[T] = insertRow(numRows, x)
  def ++=[A <: T](xs: Matrix[A]): Matrix[T] = insertAllRows(numRows, xs)

  def insertRow[A <: T](pos: Int, x: Vector[A]): Matrix[T]
  def insertAllRows[A <: T](pos: Int, xs: Matrix[A]): Matrix[T]

  def insertCol[A <: T](pos: Int, x: Vector[A]): Matrix[T]
  def insertAllCols[A <: T](pos: Int, xs: Matrix[A]): Matrix[T]

  def removeRow(pos: Int): Matrix[T] = removeRows(pos, 1)
  def removeRows(pos: Int, len: Int): Matrix[T]

  def removeCol(pos: Int): Matrix[T] = removeCols(pos, 1)
  def removeCols(pos:Int, len: Int): Matrix[T]

  def cmp(m: Matrix[T]) : DeliteBoolean = {
    run(OP_cmp(this, m))
  }

  ////////////////////////
  // arithmetic operations

  /* point-wise operations */
  def +(m: Matrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_+(this,m, run(OP_alloc[T,T](this))))
  }

  def +(m: SparseMatrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    var out = m.toMatrix
    run(OP_+(this, out, out))
  }

  def +(d: T)(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_map[T,T](this, run(OP_alloc[T,T](this)), e => ops.+(e,d)))
  }

  def +=(m: Matrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]): Unit = {
    run(OP_+=(this,m))
  }

  def -(m: Matrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_-(this,m, run(OP_alloc[T,T](this))));
  }
  def -(m: SparseMatrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    var out = m.toMatrix
    run(OP_-(this, out, out));
  }
  def -(d: T)(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_map[T,T](this, run(OP_alloc[T,T](this)), e => ops.-(e,d)))
  }

  def /(m: Matrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_/(this,m, run(OP_alloc[T,T](this))))
  }
  def /(d: T)(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_map[T,T](this, run(OP_alloc[T,T](this)), e => ops./(e,d)))
  }

  /* scalar multiplication */
  def *(d: T)(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_map[T,T](this, run(OP_alloc[T,T](this)), e => ops.*(e,d)))
  }

  def *=(d: T)(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_map[T,T](this, this, e => ops.*(e,d)))
  }

  /* point-wise multiplication */
  def dot(m: Matrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_mdot(this,m, run(OP_alloc[T,T](this))))
  }
  def dot(m: SparseMatrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    var out = m.toMatrix
    run(OP_mdot(this, out, out))
  }

  def dot(v: Vector[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_vdot(this,v))
  }

  /* Matrix-vector multiplication */
  def *(v: Vector[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Vector[T] = {
    run[Vector[T]](OP_vprod(this, v, run(OP_allocVec[T,T](this, false))))
  }

  /* Matrix-Matrix multiplication */
  def *(b: Matrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_*(this, b))
    //Matrix(run(OP_*(this, b)))
  }
  /* Matrix-Sparse Matrix multiplication */
  def *(sm: SparseMatrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_SMprod(this, sm))
  }

  def unary_-(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_unary_-(this, run(OP_alloc[T,T](this))))
  }

  def abs(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_abs(this, run(OP_alloc[T,T](this))))
  }

  def exp(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_exp(this, run(OP_alloc[T,T](this))))
  }

  def sum[B <: DeliteDSLType](implicit ops: ArithOps[T], conv: T => B, pfact: DeliteProxyFactory[B], c: ClassManifest[T]): B = {
    run(OP_sum[T,B](this))
  }

  def >(m: Matrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_>(this,m, run(OP_alloc[T,T](this))))
  }
  def >(m: SparseMatrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    var out = m.toMatrix
    run(OP_>(this, out, out))
  }
  def >(d: T)(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_map[T,T](this, run(OP_alloc[T,T](this)), e => ops.>(e,d)))
  }

  def <(m: Matrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_<(this,m, run(OP_alloc[T,T](this))))
  }
  def <(m: SparseMatrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    var out = m.toMatrix
    run(OP_<(this, out, out))
  }
  def <(d: T)(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_map[T,T](this, run(OP_alloc[T,T](this)), e => ops.<(e,d)))
  }

  /* ordering */
  def min[B <: DeliteDSLType](implicit cmp: T => Ordered[T], conv: T => B, pfact: DeliteProxyFactory[B]) : B = {
    run(OP_min(this))
  }

  def minRow[B <: DeliteDSLType](implicit ops: ArithOps[T], cmp: B => Ordered[B], conv: T => B, pfact: DeliteProxyFactory[B], c: ClassManifest[B]) : Vector[T] = {
    // TODO: nested op, will force
    val sumRows = mapRowsToVec[B](row => row.sum[B])
    val idx = sumRows.minIndex
    this(idx).clone
  }

  def max[B <: DeliteDSLType](implicit cmp: T => Ordered[T], conv: T => B, pfact: DeliteProxyFactory[B]) : B = {
    run(OP_max(this))
  }

  def maxRow[B <: DeliteDSLType](implicit ops: ArithOps[T], cmp: B => Ordered[B], conv: T => B, pfact: DeliteProxyFactory[B], c: ClassManifest[B]) : Vector[T] = {
    val sumRows = mapRowsToVec[B](row => row.sum[B])
    val idx = sumRows.maxIndex
    this(idx).clone
  }

  /* inverse, using gauss-jordan elimination */
  def inv(implicit conv: T => Double) : Matrix[Double] = {
    run[Matrix[Double]](OP_inv(this))
  }

  def rreduce(implicit conv: T => Double) : Matrix[Double] = {
    run[Matrix[Double]](OP_rreduce(this))
  }

  ///////////////////
  // bulk operations

  def map[B](f: T => B)(implicit pFact: Matrix.ProxyFactory[B], c: ClassManifest[B]) : Matrix[B] =  {
    run[Matrix[B]](OP_map(this, run(OP_alloc[T,B](this)), f))
  }

  def mmap(f: T => T)(implicit pFact: Matrix.ProxyFactory[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_mutMap[T](this, this, f))
  }

  def zipWith[B](that: Matrix[T], f: (T,T) => B)(implicit pFact: Matrix.ProxyFactory[B], c: ClassManifest[B]) : Matrix[B] =  {
    run[Matrix[B]](OP_zipWith(this, that, run(OP_alloc[T,B](this)), f))
  }

  def mapRow[B : ClassManifest](f: Vector[T] => Vector[B]) : Matrix[B] = {
    val first = f(getRow(0))
    val out = Matrix[B](numRows, first.length)
    out.updateRow(0, first)

    // TODO: how do we represent data parallelism across the rows?
    // TODO: could be done using task parallelism if updateRow is an op...

    /*
    This returns a mapped Vector[Vector[B]], but how can we re-wire it
    back into a matrix efficiently?

    val v = Vector[Vector[T]](numRows)
    for (i <- 0 until numRows){
      v(i) = vview(i*numCols, 1, numCols, true)
    }
    v.map( v => f(v) )    
    */

    for (i <- 1 until numRows){
      out.updateRow(i, f(getRow(i)))
    }
    out
  }

  def mapRowsToVec[B](f: Vector[T] => B, is_row: Boolean = true)(implicit pFact: Vector.ProxyFactory[B], c: ClassManifest[B]) : Vector[B] = {
    run[Vector[B]](OP_mapToVec(this, run(OP_allocVec[T,B](this, is_row)), f, is_row))
  }

  def foreach(block: T => Unit) = {
    for (i <- 0 until size){
      block(dc_apply(i))
    }
  }

  def foreachRow(block: Vector[T] => Unit) {
    for (i <- 0 until numRows){
      block(getRow(i))
    }
  }

  def foreachRow(block: (Vector[T],Int) => Unit) {
    for (i <- 0 until numRows){
      block(getRow(i), i)
    }
  }

  def initRow(idx: Int) {
    throw new RuntimeException("initRow : This operation should be overriden")
  }

  /*
  def reduce(func: (Vector[T],Vector[T]) => Vector[T]): Vector[T] = {
    val v = Vector[Vector[T]](numRows)
    for (i <- 0 until numRows){
      v(i) = getRow(i)
    }
    v.reduce[Vector[T]](func)
  }
  */

  def filterRows(pred: Vector[T] => Boolean)(implicit c: ClassManifest[T]): Matrix[T] = {
    val v = Vector[Vector[T]](0)
    for (i <- 0 until numRows){
      val vv = getRow(i)
      if (pred(vv))
        v += vv
    }
    Matrix[T](v)
  }

  def filter(pred: T => Boolean)(implicit c: ClassManifest[T]): Matrix[Boolean] = {
    val m = Matrix[Boolean](numRows, numCols)
/*
    for(i <- 0 until numRows){
      val row = getRow(i)
      for(j <- 0 until numCols){
        if (pred(row(j)))
          m(i,j) = true
        else
          m(i,j) = false
      }
    }
*/
    (0::numRows) { i=>
      (0::numCols){ j=>
        m(i,j) = pred(this(i,j))
      }
    }
    m
  }

//  def convolve[B <: DeliteDSLType](kernel: Matrix[T])(implicit ops: ArithOps[T], conv: T => B, pfact: DeliteProxyFactory[B], c: ClassManifest[T], cb: ClassManifest[B]): Matrix[B] = {
//    windowedFilter(kernel.numRows, kernel.numCols) { slice =>
//      (slice dot kernel).sum[B]
//    }
//  }

  def windowedFilter[B](sliceRows: Int, sliceCols: Int)(block: Matrix[T] => B)(implicit c: ClassManifest[T], cb: ClassManifest[B]) : Matrix[B] = {
    // Need to enforce odd values for sliceRows and sliceCols
    val rowOffset = (sliceRows - 1) / 2
    val colOffset = (sliceCols - 1) / 2
    (0 :: numRows, 0 :: numCols) { (row, col) =>
      if ((row >= rowOffset) && (row < numRows - rowOffset) && (col >= colOffset) && (col < numCols - colOffset)) {
        block(slice2d(row - rowOffset, row + rowOffset + 1, col - colOffset, col + colOffset + 1))
      } else {
        0.asInstanceOf[B]
      }
    }
  }

  //def precumulate(identity: Vector[T])(func: (Vector[T],Vector[T]) => Vector[T]): (Vector[T],Vector[Vector[T]]) =
  //  _data.precumulate(identity)(func)

  //def partition(pred: Vector[T] => Boolean)(implicit c: ClassManifest[T]): (Matrix[T],Matrix[T]) = {
  //  val (m1, m2) = (_data.partition(pred))
  //  (Matrix(m1), Matrix(m2))
  //}


  /////////////////////////////////////
  // RBM specific operations

  // RBM : SUM operation on Matrix that adds up each column (returns a row vector) 
  def sumCol: Vector[T] = {
    throw new RuntimeException("sumCol : This operation should be overriden")
  }

  // RBM : SUM operation on Matrix that adds up each row (returns a column vector)
  def sumRow: Vector[T] = {
    throw new RuntimeException("sumRow : This operation should be overriden")
  }

  def gaussian(norm: Double, sigma: Double): Matrix[T] = {
    throw new RuntimeException("gaussian : this operation should be overridden!")
  }
  
  // RBM : operation that replicates the given matrix to generate a larger matrix
  def repmat(i: Int, j: Int): Matrix[T] = {
    throw new RuntimeException("repmat : This operation should be overriden")
  }

  // RBM : reciprocal of each elemnts (1/elms)
  def reciprocal: Matrix[T] = {

    throw new RuntimeException("reciprocal : This operation should be overriden")
  }

  def sigmoid: Matrix[T] = {
    throw new RuntimeException("sigmoid : This opperation should be overriden")
  }

  def addNNZvaluesOnly(m : Matrix[T]): Matrix[T] = {
    throw new RuntimeException("addNNZvaluesOnly: This operation should be overidden")
  }

  def cutoffAndReturn(threshold: Double) : Vector[Double] = {
    throw new RuntimeException("cutoffAndReturn: This operation should be overridden")
  }

  def dist(i:Int, j:Int): T = {
    throw new UnsupportedOperationException("dist of Matrix[T]")
  }

  //////////////////
  // error handlers

  protected def chkUpdate{
    if (frozen){
      println("error: Matrix is frozen")
      throw new IllegalArgumentException
    }
  }

  protected def chkEquals(x: Int, y: Int) {
    if (x != y) throw new IllegalArgumentException
  }

  protected def chkRange(begin: Int, end: Int) {
    if (begin < 0 || end < begin || end > numRows) throw new IndexOutOfBoundsException
  }

  protected def chkBounds(m: Matrix[T]) = {
    if (numCols != m.numCols || numRows != m.numRows){
      println("error: Matrix dimensions must agree")
      throw new IllegalArgumentException
    }
  }




}

