/* Specialized operations for DoubleMatrices.
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  May 5, 2010
 * modified: May 5, 2010
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
package ppl.delite.dsl.optiml.specialized


import ppl.delite.cuda._
import ppl.delite.metrics._
import ppl.delite.core._
import ppl.delite.core.appinclude._
import ppl.delite.core.ops._
import ppl.delite.core.ops.specialized._
import ppl.delite.cnative._
import ppl.delite.dsl.optiml.{SparseMatrix, ArithOps, Matrix, Vector}
import ppl.delite.dsl.primitive.{DeliteInt, DeliteDouble}

object DoubleMatrix{

  protected[optiml] case class OP_diag_single(w: Int, vals: DoubleVector) extends DeliteOP_SingleTask[Matrix[Double]](vals) {
    def task = {
      if (vals._length != w) throw new Exception("diag: dimensions don't agree")
      val out = new DoubleMatrixImpl(w,w)
      var i = 0
      while (i < w) {
        out(i,i) = vals(i)
        i += 1
      }
      out
    }
  }

  protected[optiml] case class OP_alloc[B: ClassManifest](m: DoubleMatrix) extends DeliteOP_SingleTask[Matrix[B]](m) {
    def task = {
      Matrix[B](m.numRows, m.numCols)
    }
  }

  protected[optiml] case class OP_allocVec[B: ClassManifest](m: DoubleMatrix, is_row: Boolean) extends DeliteOP_SingleTask[Vector[B]](m) {
    def task = {
      Vector[B](is_row, m.numRows)
    }
  }

  protected[optiml] case class OP_apply(val collA: DoubleMatrix, i: Int, j: Int)
    extends DeliteOP_SingleTask[DeliteDouble](collA){

    def task = {
      collA(i,j)
    }
  }

  protected[optiml] case class OP_update(val collA: DoubleMatrix, i: Int, j: Int, x: Double)
    extends DeliteOP_SingleTask[DeliteUnit](collA){

    def task = {
      collA(i,j) = x
    }
  }


  /**
   * Single task versions of arithmetic ops
   */
  protected[delite] case class OP_plus_single(val collA: DoubleMatrix, val collB: DoubleMatrix)
    extends DeliteOP_SingleTask[Matrix[Double]](collA, collB) {

    def task = {
      val out = new DoubleMatrixImpl(collA._numRows, collA._numCols)
      var i = 0
      var j = 0
      while (i < out._numRows){
        while (j < out._numCols){
          out(i,j) = collA(i,j)+collB(i,j)
          j += 1
        }
        j = 0
        i += 1
      }
      out
    }
  }

  protected[delite] case class OP_minus_single(val collA: DoubleMatrix, val collB: DoubleMatrix)
    extends DeliteOP_SingleTask[Matrix[Double]](collA, collB) {

    def task = {
      val out = new DoubleMatrixImpl(collA._numRows, collA._numCols)
      var i = 0
      var j = 0
      while (i < out._numRows){
        while (j < out._numCols){
          out(i,j) = collA(i,j)-collB(i,j)
          j += 1
        }
        j = 0
        i += 1
      }
      out
    }
  }

  protected[optiml] case class OP_plusEquals_single(collA: DoubleMatrix, collB: DoubleMatrix)
    extends DeliteOP_MutableSingleTask[Matrix[Double]](collB)(collA) {

    def task = {
      val out = collA
      var i = 0
      var j = 0
      while (i < out._numRows){
        while (j < out._numCols){
          out(i,j) = collA(i,j)+collB(i,j)
          j += 1
        }
        j = 0
        i += 1
      }
      out
    }
  }

  protected[delite] case class OP_mult_single(collA: DoubleMatrix, collB: DoubleMatrix)
    extends DeliteOP_SingleTask[Matrix[Double]](collA, collB) {

    def task = {
      collA.chkEquals(collA._numCols, collB._numRows)
      val out = new DoubleMatrixImpl(collA._numRows, collB._numCols)
      var rl = 0
      while (rl != collA._numRows) {
        var cr = 0
        while (cr != collB._numCols) {
          var c = 0
          var acc : Double = 0
          while (c != collA._numCols) {
            acc += (collA(rl,c) * collB(c,cr))
            c += 1
          }
          out(rl,cr) = acc
          cr += 1
        }
        rl += 1
      }
      out
    }
  }

  protected[optiml] case class OP_native_vmult_single(m: DoubleMatrix, v: DoubleVector)
    extends DeliteOP_SingleTask[Vector[Double]](m,v){

    val out : DoubleVector = Vector[Double](false, m.numRows).asInstanceOf[DoubleVector]

    override def task = {
      //DeliteNative.matrixMultDouble(m._data, v._data, out._data, m._numRows, m._numCols, 1)
      if(v.isInstanceOf[DoubleVectorViewImpl])
        DeliteNative.matVMultDouble(m._data, v._data, out._data, m._numRows, m._numCols, v.asInstanceOf[DoubleVectorViewImpl].start, v.asInstanceOf[DoubleVectorViewImpl].stride)
      else
        DeliteNative.matVMultDouble(m._data, v._data, out._data, m._numRows, m._numCols, 0, 1)
      out
    }
  }

  protected[delite] case class OP_vmult(m: DoubleMatrix, v: DoubleVector)
    extends DeliteOP_ForEachSpec[Int, Vector[Double]](m, v)(){

    m.chkEquals(m.numCols, v.length)

    val coll = Vector.range(0, m.numRows)
    val out : DoubleVector = Vector[Double](false, m.numRows).asInstanceOf[DoubleVector]

    def func = idx => {
      var c = 0
      var acc : Double = 0
      while (c != m._numCols) {
        acc += m(idx,c) * v(c)
        c += 1
      }
      out(idx) = acc
    }
  }

  protected[delite] case class OP_vmult_single(m: DoubleMatrix, v: DoubleVector)
    extends DeliteOP_SingleTask[Vector[Double]](m,v) {

    def task = {
      m.chkEquals(m._numCols, v.length)
      val out = new DoubleVectorImpl(false, m._numRows)
      var rl = 0
      while (rl != m._numRows) {
        var c = 0
        var acc : Double = 0
        while (c != m._numCols) {
          acc += m(rl,c) * v(c)
          c += 1
        }
        out(rl) = acc
        rl += 1
      }
      out
    }
  }

  protected[delite] case class OP_smult_single(m: DoubleMatrix, s: Double)
    extends DeliteOP_SingleTask[Matrix[Double]](m) {

    def task = {
      val out = new DoubleMatrixImpl(m._numRows, m._numCols)
      var i = 0
      while (i != m._numRows) {
        var j = 0
        while (j != m._numCols) {
          out(i,j) = m(i,j) * s
          j += 1
        }
        i += 1
      }
      out
    }
  }


  protected[optiml] case class OP_smult_single_mutable(m: DoubleMatrix, s: Double)
    extends DeliteOP_MutableSingleTask[Matrix[Double]]()(m) {

    def task = {
      var i = 0
      while (i != m._numRows) {
        var j = 0
        while (j != m._numCols) {
          m(i,j) = m(i,j) * s
          j += 1
        }
        i += 1
      }
      m
    }
  }


  protected[delite] case class OP_vdot_single(m: DoubleMatrix, v: DoubleVector)
    extends DeliteOP_SingleTask[Matrix[Double]](m,v) {

    def task = {
      m.chkEquals(m._numRows, v.length)
      val out = new DoubleMatrixImpl(m._numRows, m._numCols)
      var r = 0
      while (r != m._numRows) {
        var c = 0
        while (c != m._numCols) {
          out(r,c) = m(r,c) * v(r)
          c += 1
        }
        r += 1
      }
      out
    }
  }

  protected[delite] case class OP_trans(m: DoubleMatrix)
    extends DeliteOP_SingleTask[Matrix[Double]](m) {

    def task = {
      val out = new DoubleMatrixImpl(m._numCols,m._numRows)
      var i = 0
      while (i != m._numCols) {
        var j = 0
        while (j != m._numRows) {
          out(i,j) = m(j,i)
          j += 1
        }
        i += 1
      }
      out
    }
  }

  /**
   * arithmetic ops
   */


  protected[optiml] case class OP_+(val collA: DoubleMatrix, val collB: DoubleMatrix, val out: DoubleMatrix)
    extends DeliteOP_ZipWith2Spec[Double,Matrix]{
    override val associative = true

    def func = (a,b) => a+b
  }

  protected[optiml] case class OP_+=(val collA: DoubleMatrix, val collB: DoubleMatrix)
    extends DeliteOP_ZipWith2Spec[Double,Matrix]{
    
    val out = collA
    final def func = (a,b) => a+b
  }

  protected[optiml] case class OP_-(val collA: DoubleMatrix, val collB: DoubleMatrix, val out: DoubleMatrix)
    extends DeliteOP_ZipWith2Spec[Double,Matrix]{

    def func = (a,b) => a-b

  }

  protected[optiml] case class OP_/[A](val collA: DoubleMatrix, val collB: DoubleMatrix, val out: DoubleMatrix)
    extends DeliteOP_ZipWith2Spec[Double,Matrix]{

    def func = (a,b) => a/b
  }

  protected[optiml] case class OP_mdot(val collA: DoubleMatrix, val collB: DoubleMatrix, val out: DoubleMatrix)
    extends DeliteOP_ZipWith2Spec[Double,Matrix]{

    def func = (a,b) => a*b
  }


  protected[optiml] case class OP_native_*(m1: DoubleMatrix, m2: DoubleMatrix)
    extends DeliteOP_SingleTask[Matrix[Double]](m1, m2){

    val out : DoubleMatrix = Matrix[Double](m1.numRows, m2.numCols).asInstanceOf[DoubleMatrix]

    override def task = {
      DeliteNative.matrixMultDouble(m1._data, m2._data, out._data, m1._numRows, m1._numCols, m2._numCols)
      out
    }
  }


  protected[optiml] case class OP_*(m1: DoubleMatrix, m2: DoubleMatrix)
    extends DeliteOP_ForEachSpec[Int, Matrix[Double]](m1, m2)(){

    m1.chkEquals(m1.numCols, m2.numRows)
    val b_trans : DoubleMatrix = m2.trans.asInstanceOf[DoubleMatrix]

    val coll = Vector.range(0, m1.numRows)
    val out : DoubleMatrix = Matrix[Double](m1.numRows, m2.numCols).asInstanceOf[DoubleMatrix]

    // TODO: could operate on block indices instead of row indices
    def func = DeliteFunc((row_idx => {
      var i = 0
      while (i < out._numCols){
        var j = 0
        var acc : Double = 0
        while (j < b_trans._numCols){
          acc += m1(row_idx, j)*b_trans(i, j)
          j += 1
        }
        out(row_idx, i) = acc
        i += 1
      }
    }), b_trans)
  }

  protected[optiml] case class OP_smMult(m1: DoubleMatrix, m2: SparseMatrix[Double])
    extends DeliteOP_ForEach[Int, Matrix[Double]](m1, m2)(){

    // TODO: all this has to be lifted, -> can't be in the constructor
    m1.chkEquals(m1.numCols, m2.numRows)
    //val b_trans = m2.trans

    val coll = Vector.range(0, m2.numCols)
    val out : DoubleMatrix = Matrix[Double](m1.numRows, m2.numCols).asInstanceOf[DoubleMatrix]

    def func = DeliteFunc((col => {

      val thisColI = m2.CSC_col_idx_apply(col)
      if(thisColI != -1){
        //find next col's index
        var nextCol = col + 1
        var nextColI = m2.CSC_col_idx_apply(nextCol)
        while(nextColI == -1){
          nextCol += 1
          nextColI = m2.CSC_col_idx_apply(nextCol)
        }

        //now, calculate out's values for this column (col)
        var rI = 0
        while (rI < out._numRows){

          var sum = 0.0
          var cI = thisColI

          while(cI < nextColI){
            sum += m1(rI, m2.CSC_row_idx_apply(cI)) * m2.CSC_dc_apply(cI)
            cI += 1
          }

          out(rI, col) = sum
          rI += 1
        }
      }
    }))
  }

  /*
  protected[optiml] case class OP_vprod[A](m: Matrix[A], v: Vector[A], val out: Vector[A])(implicit ops: ArithOps[A], pFact: Vector.ProxyFactory[A], c: ClassManifest[A])
    extends DeliteOP_Map[Vector[A], A, Vector]{

    if (v.is_row) throw new IllegalArgumentException

    var tcoll = Vector[Vector[A]](false, m.numRows)
    for (i <- 0 until m.numRows){
      tcoll(i) = m.getRow(i)
    }

    val coll = tcoll
    def func = row => row._dot(v)
  }
  */

  protected[optiml] case class OP_unary_-(val coll: DoubleMatrix, val out: DoubleMatrix)
    extends DeliteOP_MapSpec[Double,Matrix] {

    def func = e => doubleArithOps.unary_-(e)
  }

  protected[optiml] case class OP_abs(val coll: DoubleMatrix, val out: DoubleMatrix)
    extends DeliteOP_MapSpec[Double,Matrix] {

    def func = e => doubleArithOps.abs(e)
  }

  protected[optiml] case class OP_exp(val coll: DoubleMatrix, val out: DoubleMatrix)
    extends DeliteOP_MapSpec[Double,Matrix] {

    def func = e => doubleArithOps.exp(e)
  }

  protected[optiml] case class OP_native_exp(val coll: DoubleMatrix)
    extends DeliteOP_SingleTask[Matrix[Double]](coll) {
	
	def task = {
		val out = new DoubleMatrixImpl(coll._numRows, coll._numCols)
		DeliteNative.vecExpDouble(coll._data, out._data, coll._numRows*coll._numCols)
		out
	}
  }

  protected[optiml] case class OP_native_sigmoid(val coll: DoubleMatrix, val out: DoubleMatrix)
    extends DeliteOP_NativeMapSpec[Double, Matrix] {

	  def func = null
    def funcNative(start: Int, end: Int) = DeliteNative.vecSigmoidDouble(coll._data, out._data, start, end)
	
	/*
	def task = {
		val out = new DoubleMatrixImpl(coll._numRows, coll._numCols)
		DeliteNative.vecSigmoidDouble(coll._data, out._data, coll._numRows*coll._numCols)
		out
	}
	*/
  }

  protected[optiml] case class OP_>[A](val collA: DoubleMatrix, val collB: DoubleMatrix, val out: DoubleMatrix)
    extends DeliteOP_ZipWith2Spec[Double,Matrix]{

    def func = (a,b) => if (a > b) 1 else 0
  }

  protected[optiml] case class OP_<[A](val collA: DoubleMatrix, val collB: DoubleMatrix, val out: DoubleMatrix)
    extends DeliteOP_ZipWith2Spec[Double,Matrix]{

    def func = (a,b) => if (a < b) 1 else 0
  }

  protected[optiml] case class OP_sum[B <: DeliteDSLType](val coll: DoubleMatrix)
    (implicit conv: Double => B, pfact: DeliteProxyFactory[B])
    extends DeliteOP_ReduceSpec[Double,B] {

    def func = (a,b) => a+b
  }

  protected[optiml] case class OP_min[B <: DeliteDSLType](val coll: DoubleMatrix)
    (implicit conv: Double => B, pfact: DeliteProxyFactory[B]) extends DeliteOP_ReduceSpec[Double,B] {

    def func = (a,b) => if (a < b) a else b
  }


  protected[optiml] case class OP_max[B <: DeliteDSLType](val coll: DoubleMatrix)
    (implicit conv: Double => B, pfact: DeliteProxyFactory[B]) extends DeliteOP_ReduceSpec[Double,B] {

    def func = (a,b) => if (a > b) a else b
  }

  protected[optiml] case class OP_map[B](val coll: DoubleMatrix, val out: Matrix[B], val func: Double => B)(implicit c1: ClassManifest[B])
    extends DeliteOP_Map[Double,B,Matrix]


  protected[optiml] case class OP_mapDouble(val coll: DoubleMatrix, val out: DoubleMatrix, val func: Double => Double)
    extends DeliteOP_MapSpec[Double,Matrix]

  
  /////////////////////////////////////////////////////////////////////////////////////
  ///////////////////            GPU OPS        ///////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////

  protected[delite] case class OPGPU_plus_single(val collA: DoubleMatrix, val collB: DoubleMatrix)
    extends DeliteOP_SingleTask[Matrix[Double]](collA, collB) {

    def task = {
      val out = new DoubleMatrixImpl(collA._numRows, collA._numCols)
      var i = 0
      var j = 0
      while (i < out._numRows){
        while (j < out._numCols){
          out(i,j) = collA(i,j)+collB(i,j)
          j += 1
        }
        j = 0
        i += 1
      }
      out
    }

    override def getGPUInputs = {List(collA, collB)}
    override def getGPUConsts = {List(collA._numRows*collA._numCols)}
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Matrix[Double](elms, collA.numRows, collA.numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._numRows*collA._numCols)
	  override def getGPUKernelId = {List(DeliteCuda.DVectPlusDouble, DeliteCuda.AsyncLaunch1D)}
	  //override def getGPUKernelId = {List(DeliteCuda.DVectPlusDouble, DeliteCuda.Async2I1D)}
  }

  /*
  protected[optiml] case class OPGPU_plusEquals_single(collA: DoubleMatrix, collB: DoubleMatrix)
    extends DeliteOP_MutableSingleTask[Matrix[Double]](collB)(collA) {

    def task = {
      val out = collA
      var i = 0
      var j = 0
      while (i < out._numRows){
        while (j < out._numCols){
          out(i,j) = collA(i,j)+collB(i,j)
          j += 1
        }
        j = 0
        i += 1
      }
      out
    }

    override def getGPUInputs = {List(collA, collB)}
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Matrix[Double](elms, collA.numRows, collA.numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
	  override def getGPUKernelId = {List(DeliteCuda.DVectPlusDouble, DeliteCuda.Async2I1D)}
  }
  */

  protected[delite] case class OPGPU_mult_single(collA: DoubleMatrix, collB: DoubleMatrix)
    extends DeliteOP_SingleTask[Matrix[Double]](collA, collB) {

    def task = {
      collA.chkEquals(collA._numCols, collB._numRows)
      val out = new DoubleMatrixImpl(collA._numRows, collB._numCols)
      var rl = 0
      while (rl != collA._numRows) {
        var cr = 0
        while (cr != collB._numCols) {
          var c = 0
          var acc : Double = 0
          while (c != collA._numCols) {
            acc += (collA(rl,c) * collB(c,cr))
            c += 1
          }
          out(rl,cr) = acc
          cr += 1
        }
        rl += 1
      }
      out
    }

    override def getGPUInputs = List(collA, collB)
    override def getGPUConsts = List(collA._numCols, collB._numCols, collA._numRows)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Matrix[Double](elms, collA._numRows, collB._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collB._numCols, collA._numRows, 16, 16)
	  override def getGPUKernelId = {if((collA._numRows<4)||(collA._numCols<4)||(collB._numRows<4)||(collB._numCols<4)) List(DeliteCuda.MatMulDoubleReg,DeliteCuda.AsyncLaunch2D) else List(DeliteCuda.MatMulDouble,DeliteCuda.Async3D3I)}
  }

  protected[delite] case class OPGPU_vmult_single(m: DoubleMatrix, v: DoubleVector)
    extends DeliteOP_SingleTask[Vector[Double]](m,v) {

    def task = {
      m.chkEquals(m._numCols, v.length)
      val out = new DoubleVectorImpl(false, m._numRows)
      var rl = 0
      while (rl != m._numRows) {
        var c = 0
        var acc : Double = 0
        while (c != m._numCols) {
          acc += m(rl,c) * v(c)
          c += 1
        }
        out(rl) = acc
        rl += 1
      }
      out
    }

    //TODO: Check whether column vector
    override def getGPUInputs = List[AnyRef](m, v)
    override def getGPUConsts = List(m._numCols, m._numRows)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Vector[Double](elms, false, m._numRows)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(16, m._numRows, 16, 32)
    //override def getGPUKernelDims = List(4, m._numRows, 4, 128)
    override def getGPUKernelId = List(DeliteCuda.MatProdV, DeliteCuda.AsyncLaunch2D)
	  //override def getGPUKernelId = {List(DeliteCuda.MatProdV, DeliteCuda.AsyncMdotV)}
  }

  protected[delite] case class OPGPU_smult_single(m: DoubleMatrix, s: Double)
    extends DeliteOP_SingleTask[Matrix[Double]](m) {

    def task = {
      val out = new DoubleMatrixImpl(m._numRows, m._numCols)
      var i = 0
      while (i != m._numRows) {
        var j = 0
        while (j != m._numCols) {
          out(i,j) = m(i,j) * s
          j += 1
        }
        i += 1
      }
      out
    }

    override def getGPUInputs = List(m)
    override def getGPUConsts = List(s, m._numRows*m._numCols)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Matrix[Double](elms, m._numRows, m._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(m._numRows*m._numCols)
    override def getGPUKernelId = List(DeliteCuda.VectMultDouble_S, DeliteCuda.AsyncLaunch1D)
	  //override def getGPUKernelId = List(DeliteCuda.VectMultDouble_S, DeliteCuda.AsyncRBM_1I1S)
  }

  protected[delite] case class OPGPU_vdot_single(m: DoubleMatrix, v: DoubleVector)
    extends DeliteOP_SingleTask[Matrix[Double]](m,v) {

    def task = {
      m.chkEquals(m._numRows, v.length)
      val out = new DoubleMatrixImpl(m._numRows, m._numCols)
      var r = 0
      while (r != m._numRows) {
        var c = 0
        while (c != m._numCols) {
          out(r,c) = m(r,c) * v(r)
          c += 1
        }
        r += 1
      }
      out
    }

    override def getGPUInputs = List[AnyRef](m, v)
    override def getGPUConsts = List(m._numCols, m._numRows)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Matrix[Double](elms, m._numRows, m._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(m._numCols, m._numRows, 256, 1)
	  override def getGPUKernelId = List(DeliteCuda.MatDotV, DeliteCuda.AsyncLaunch2D)
    //override def getGPUKernelId = {List(DeliteCuda.MatDotV, DeliteCuda.AsyncMatDotV)}
  }

  protected[delite] case class OPGPU_trans(m: DoubleMatrix)
    extends DeliteOP_SingleTask[Matrix[Double]](m) {

    def task = {
      val out = new DoubleMatrixImpl(m._numCols,m._numRows)
      var i = 0
      while (i != m._numCols) {
        var j = 0
        while (j != m._numRows) {
          out(i,j) = m(j,i)
          j += 1
        }
        i += 1
      }
      out
    }

    override def getGPUInputs = List(m)
    override def getGPUConsts = List(m._numCols, m._numRows)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Matrix[Double](elms, m._numCols, m._numRows)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(m._numCols, m._numRows, 16, 16)
	  override def getGPUKernelId = List(DeliteCuda.MatTransDouble, DeliteCuda.AsyncLaunch2D)
    //override def getGPUKernelId = {List(DeliteCuda.MatTransDouble, DeliteCuda.Async3D3I)}
  }

  protected[optiml] case class OPGPU_mdot(val collA: DoubleMatrix, val collB: DoubleMatrix)
    extends DeliteOP_SingleTask[Matrix[Double]](collA, collB){

      def task = {
        val numRows = collA.numRows
        val numCols = collA.numCols
        val out = new DoubleMatrixImpl(numRows,numCols)
        var i = 0
        var j = 0
        while (i < numRows) {
          j = 0
          while(j < numCols) {
            out(i,j) = collA(i,j) * collB(i,j)
            j += 1
          }
          i += 1
        }
        out
	    }

    override def getGPUInputs = List(collA, collB)
    override def getGPUConsts = List(collA._numRows*collA._numCols)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Matrix[Double](elms, collA._numRows, collA._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._numRows*collA._numCols)
	  override def getGPUKernelId = List(DeliteCuda.DVectMultDouble, DeliteCuda.AsyncLaunch1D)
    //override def getGPUKernelId = {List(DeliteCuda.DVectMultDouble, DeliteCuda.Async2I1D)}
  }

  protected[optiml] case class OPGPU_exp(val coll: DoubleMatrix)
    extends DeliteOP_SingleTask[Matrix[Double]](coll) {

      def task = {
        val numRows = coll.numRows
        val numCols = coll.numCols
        val out = new DoubleMatrixImpl(numRows,numCols)
        var i = 0
        var j = 0
        while (i < numRows) {
          j = 0
          while(j < numCols) {
            out(i,j) = doubleArithOps.exp(coll(i,j))
            j += 1
          }
          i += 1
        }
        out
	    }

    override def getGPUInputs = List(coll)
    override def getGPUConsts = List(coll._numRows*coll._numCols)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Matrix[Double](elms, coll.numRows, coll.numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(coll._numRows*coll._numCols)
	  override def getGPUKernelId = List(DeliteCuda.VectExpDouble, DeliteCuda.AsyncLaunch1D)
    //override def getGPUKernelId = List(DeliteCuda.VectExpDouble, DeliteCuda.AsyncRBM_1I)
  }

  protected[optiml] case class OPGPU_>[A](val collA: DoubleMatrix, val collB: DoubleMatrix)
    extends DeliteOP_SingleTask[Matrix[Double]](collA, collB){

    def task = {
        val numRows = collA.numRows
        val numCols = collA.numCols
        val out = new DoubleMatrixImpl(numRows,numCols)
        var i = 0
        var j = 0
        while (i < numRows) {
          j = 0
          while(j < numCols) {
            if(collA(i,j) > collB(i,j))
              out(i,j) = 1
            else
              out(i,j) = 0
            j += 1
          }
          i += 1
        }
        out
	    }

    override def getGPUInputs = List(collA, collB)
    override def getGPUConsts = List(collA._numRows*collA._numCols)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Matrix[Double](elms, collA._numRows, collA._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._numRows*collA._numCols)
    override def getGPUKernelId = List(DeliteCuda.VectGTDouble, DeliteCuda.AsyncLaunch1D)
	  //override def getGPUKernelId = List(DeliteCuda.VectGTDouble, DeliteCuda.AsyncRBM_2I)
  }

    protected[optiml] case class OPGPU_-(val collA: DoubleMatrix, val collB: DoubleMatrix)
    extends DeliteOP_SingleTask[Matrix[Double]](collA, collB){

      def task = {
        val numRows = collA.numRows
        val numCols = collA.numCols
        val out = new DoubleMatrixImpl(numRows,numCols)
        var i = 0
        var j = 0
        while (i < numRows) {
          j = 0
          while(j < numCols) {
            out(i,j) = collA(i,j) - collB(i,j)
            j += 1
          }
          i += 1
        }
        out
	    }

    override def getGPUInputs = List(collA, collB)
    override def getGPUConsts = List(collA._numRows*collA._numCols)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Matrix[Double](elms, collA._numRows, collA._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._numRows*collA._numCols)
    override def getGPUKernelId = List(DeliteCuda.DVectMinusDouble, DeliteCuda.AsyncLaunch1D)
	  //override def getGPUKernelId = {List(DeliteCuda.DVectMinusDouble, DeliteCuda.Async2I1D)}
  }

  protected[optiml] case class OPGPU_DoublePlus(val coll: DoubleMatrix, val d: Double)
    extends DeliteOP_SingleTask[Matrix[Double]](coll) {

      def task = {
        val numRows = coll.numRows
        val numCols = coll.numCols
        val out = new DoubleMatrixImpl(numRows,numCols)
        var i = 0
        var j = 0
        while (i < numRows) {
          j = 0
          while(j < numCols) {
            out(i,j) = coll(i,j) + d
            j += 1
          }
          i += 1
        }
        out
	    }
    
    override def getGPUInputs = List(coll)
    override def getGPUConsts = List(d, coll._numRows*coll._numCols)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Matrix[Double](elms, coll._numRows, coll._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(coll._numRows*coll._numCols)
    override def getGPUKernelId = List(DeliteCuda.VectPlusDouble_S, DeliteCuda.AsyncLaunch1D)
	  //override def getGPUKernelId = List(DeliteCuda.VectPlusDouble_S, DeliteCuda.AsyncRBM_1I1S)
  }

  protected[optiml] case class OPGPU_sumCol(m: DoubleMatrix) extends DeliteOP_SingleTask[Vector[Double]](m) {
    def task = {
		  // assume the array is initialized to all 0
		  val width = m.numCols
	 	  val height = m.numRows
      val out = new DoubleVectorImpl(true, m.numCols)
		  var i = 0
		  var j = 0
		  while (i != height) {
			  j = 0
			  while(j != width) {
				  out(j) = out(j) + m(i,j)
				  j += 1
			  }
			  i += 1
		  }
      out
	  }

    override def getGPUInputs = List(m)
    override def getGPUConsts = List(m._numRows, m._numCols)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Vector[Double](elms, true, m._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(m._numCols)
    override def getGPUKernelId = List(DeliteCuda.sumColsDouble, DeliteCuda.AsyncLaunch1D)
	  //override def getGPUKernelId = List(DeliteCuda.sumColsDouble, DeliteCuda.AysncRBM_1I2D)
	}

  protected[optiml] case class OPGPU_recip(m: DoubleMatrix) extends DeliteOP_SingleTask[Matrix[Double]](m) {
		def task = {
      val out = new DoubleMatrixImpl(m._numRows, m._numCols)
      val width = m._numCols
      val height = m._numRows
			var i = 0
      var j = 0
      while(i < height) {
        j = 0
        while(j < width) {
          out(i,j) = 1 / m(i,j)
          j += 1
        }
        i += 1
      }
			out
		}

    override def getGPUInputs = List(m)
    override def getGPUConsts = List(m._numRows*m._numCols)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Matrix[Double](elms, m.numRows, m.numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(m._numRows*m._numCols)
    override def getGPUKernelId = List(DeliteCuda.VectRecipDouble, DeliteCuda.AsyncLaunch1D)
	  //override def getGPUKernelId = List(DeliteCuda.VectRecipDouble, DeliteCuda.AsyncRBM_1I)
  }

  protected[optiml] case class OPGPU_diag_single(w: Int, vals: DoubleVector) extends DeliteOP_SingleTask[Matrix[Double]](vals) {
    def task = {
      if (vals._length != w) throw new Exception("diag: dimensions don't agree")
      val out = new DoubleMatrixImpl(w,w)
      var i = 0
      while (i < w) {
        out(i,i) = vals(i)
        i += 1
      }
      out
    }
    override def getGPUInputs = List(vals)
    override def getGPUConsts = List(w)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Matrix[Double](elms, w, w)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(w, w)
	  override def getGPUKernelId = List(DeliteCuda.matDiagDouble, DeliteCuda.AsyncLaunch2D)
  }

  /////////////////////////////////////////////////////////////////////////////////////
  ///////////////////         END OF GPU OPS        ///////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////


  /////////////////////
  //  RBM functions

  /*protected[optiml] case class OP_sumCol(m: DoubleMatrix) extends DeliteOP_SingleTask[Vector[Double]](m) {
    def task = {
		  // assume the array is initialized to all 0
		  val width = m.numCols
	 	  val height = m.numRows
      val out = new DoubleVectorImpl(true, m.numCols)
		  var i = 0
		  var j = 0
		  while (i != height) {
			  j = 0
			  while(j != width) {
				  out(j) = out(j) + m(i,j)
				  j += 1
			  }
			  i += 1
		  }
      out
	  }
	}*/

  protected[optiml] case class OP_sumCol(m: DoubleMatrix) extends DeliteOP_ForEach[Int, Vector[Double]](m)(){
    val coll = Vector.range(0, m.numCols)
    val out = new DoubleVectorImpl(true, m.numCols)

    def func = col => {
      var i = 0
      var toStop = m.numRows
      while(i < toStop){
        out(col) += m(i, col)
        i += 1
      }
    }

  }

  protected[optiml] case class OP_sumRow(m: DoubleMatrix) extends DeliteOP_SingleTask[Vector[Double]](m) {
    def task = {
		  // assume the array is initialized to all 0
      val out = new DoubleVectorImpl(false, m.numRows)
		  var i = 0
		  var j = 0
		  while (i != m.numRows) {
			  j = 0
			  while(j != m.numCols) {
				  out(i) = out(i) + m(i,j)
				  j += 1
			  }
			  i += 1
		  }
      out
	  }
	}

  protected[optiml] case class OP_gaussian(m: DoubleMatrix, norm: Double, sigma: Double) extends DeliteOP_SingleTask[Matrix[Double]](m) {
		def task = {
      import java.util.Random

      val r = new Random()
      val out = new DoubleMatrixImpl(m.numRows, m.numCols)
      val numRows = m.numRows
      val numCols = m.numCols
      var row = 0
      var col = 0
      while(row < numRows){
        col = 0
        while(col < numCols){
          val rand = r.nextGaussian()
          out(row, col) = (sigma*rand + m(row,col))

          col += 1
        }
        row += 1
      }

      out
    }
  }

	protected[optiml] case class OP_repmat(m: DoubleMatrix, iRep: Int, jRep: Int) extends DeliteOP_SingleTask[Matrix[Double]](m) {
		def task = {
      val out = new DoubleMatrixImpl(iRep*m._numRows, jRep*m._numCols)
			val height = m.numRows
			val width = m.numCols
			//var index = 0
			var i = 0
			var j = 0
			var ii = 0
			var jj = 0
			while(ii != iRep) {
				i = 0
				while(i != height) {
					jj = 0
					while(jj != jRep) {
						j = 0
						while(j != width) {
							out(ii*height+i, jj*width+j) = m(i,j)
							//index += 1
							j += 1
						}
						jj += 1
					}
					i += 1
				}
				ii += 1
			}
			out
		}
	}

 
  protected[delite] case class OP_sigmoid(val collA: DoubleMatrix)
    extends DeliteOP_SingleTask[Matrix[Double]](collA) {

    def task = {
      val out = new DoubleMatrixImpl(collA._numRows, collA._numCols)
      var i = 0
      var j = 0
      while (i < out._numRows){
        while (j < out._numCols){
          out(i,j) = (1.0/(1.0+math.exp(-collA(i,j)))).asInstanceOf[Double]
          j += 1
        }
        j = 0
        i += 1
      }
      out
    }
  }

  protected[delite] case class OP_addNNZvaluesOnly(val collA: Matrix[Double], val collB: Matrix[Double])
    extends DeliteOP_ZipWith2[Double, Double, Double, Matrix]{

    val out = collA
    def func = (a,b) => {
      if(a == 0) 0.0
      else a+b
    }
  }

  protected[optiml] case class OP_cutoffAndReturn(m: Matrix[Double], threshold_squared: Double)
    extends DeliteOP_ForEach[Int, Vector[Double]](threshold_squared)(m){

    val coll = Vector.range(0, m.numRows)
    val out = Vector.zeros(m.numRows)

    def func = row => {
      var cI = m.numCols - 1
      
      while(cI > -1){
        if(m(row, cI)*m(row,cI) < threshold_squared){
          out(row) += 1.0
          m(row, cI) = 0.0
        }

        cI -= 1
      }
    }
  }

}


trait DoubleMatrix extends Matrix[Double] {
  import DoubleMatrix._

  protected[optiml] var _data: Array[Double]

  override def apply(i: Int, j: Int) : Double
  override def lifted_apply(i: Int, j: Int) : Double  = {
    run(OP_apply(this,i,j))
  }

  override def update(row: Int, col: Int, x: Double)
  override def lifted_update(row: Int, col:Int, x: Double) : Unit = {
    run(OP_update(this,row,col,x))
  }
  
  override def dist(i:Int, j:Int): Double

  override def dc_update(i: Int, x: Double)
  override def dc_apply(i: Int) : Double    

  //////////
  // arith

  // gda, nb, linreg ops
  override def +(m: Matrix[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
    if(Config.executorType == "gpu")
      run(OPGPU_plus_single(this,m.asInstanceOf[DoubleMatrix]))
    else
      //run(OP_plus_single(this,m.asInstanceOf[DoubleMatrix]))
    run(OP_+(this,m.asInstanceOf[DoubleMatrix],run(OP_alloc[Double](this)).asInstanceOf[DoubleMatrix]))
  }

  // for performance testing only
  def plusEqualsZipWith(m: Matrix[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Unit = {
    run(OP_+=(this,m.asInstanceOf[DoubleMatrix]))
  }

  def plusEqualsSingle(m: Matrix[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]): Unit = {
    //if(Config.executorType == "gpu")
    //  run(OPGPU_plusEquals_single(this,m.asInstanceOf[DoubleMatrix]))
    //else
      run(OP_plusEquals_single(this,m.asInstanceOf[DoubleMatrix]))
  }

  override def +=(m: Matrix[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]): Unit = {
    //run(OP_plusEquals_single(this,m.asInstanceOf[DoubleMatrix]))
    run(OP_+=(this,m.asInstanceOf[DoubleMatrix]))
  }

  override def *(sm: SparseMatrix[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
      run(OP_smMult(this, sm))
  }

  override def *(m: Matrix[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
    if(Config.executorType == "gpu") {
      run(OPGPU_mult_single(this, m.asInstanceOf[DoubleMatrix]))
    }
    else{
      if (Config.useNativeLibs){
        run(OP_native_*(this, m.asInstanceOf[DoubleMatrix]))
      }
      else{
        run(OP_*(this, m.asInstanceOf[DoubleMatrix]))
      }
    }
  }

  override def *(v: Vector[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Vector[Double] = {
    if(Config.executorType == "gpu")
      run[Vector[Double]](OPGPU_vmult_single(this, v.asInstanceOf[DoubleVector]))
    else{
      if (Config.useNativeLibs){
        run(OP_native_vmult_single(this, v.asInstanceOf[DoubleVector]))
      }
      else{
        //run(OP_vmult_single(this, v.asInstanceOf[DoubleVector]))
        run(OP_vmult(this, v.asInstanceOf[DoubleVector]))
      }
    }
  }

  override def *(d: Double)(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
    //run(OP_smult_single(this, d))
    if(Config.executorType == "gpu")
      run(OPGPU_smult_single(this, d))
    else
      run(OP_mapDouble(this, run(OP_alloc[Double](this)).asInstanceOf[DoubleMatrix], e => e*d))
  }

  override def *=(d: Double)(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
    //run(OP_smult_single_mutable(this, d))
    run(OP_mapDouble(this, this, e => e*d))    
  }

  override def dot(v: Vector[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
    if(Config.executorType == "gpu")
      run(OPGPU_vdot_single(this,v.asInstanceOf[DoubleVector]))
    else
      run(OP_vdot_single(this,v.asInstanceOf[DoubleVector]))
  }

  override def trans(implicit c: ClassManifest[Double]) : Matrix[Double] = {
    if(Config.executorType == "gpu")
      run(OPGPU_trans(this))
    else
      run(OP_trans(this))
  }

  // others
  
  override def +(d: Double)(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
    if(Config.executorType == "gpu")
      run(OPGPU_DoublePlus(this, d))
    else
      run(OP_mapDouble(this, run(OP_alloc[Double](this)).asInstanceOf[DoubleMatrix], e => e+d))
  }

  override def -(m: Matrix[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
    if(Config.executorType == "gpu")
      run(OPGPU_-(this, m.asInstanceOf[DoubleMatrix]));
    else{
      //run(OP_minus_single(this,m.asInstanceOf[DoubleMatrix]))
      run(OP_-(this, m.asInstanceOf[DoubleMatrix], run(OP_alloc[Double](this)).asInstanceOf[DoubleMatrix]));
    }
  }

  override def -(d: Double)(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
    run(OP_mapDouble(this, run(OP_alloc[Double](this)).asInstanceOf[DoubleMatrix], e => e-d))
  }

  override def /(m: Matrix[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
    run(OP_/(this, m.asInstanceOf[DoubleMatrix], run(OP_alloc[Double](this)).asInstanceOf[DoubleMatrix]))
  }
  override def /(d: Double)(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
      run(OP_mapDouble(this, run(OP_alloc[Double](this)).asInstanceOf[DoubleMatrix], e => e/d))
  }

  override def dot(m: Matrix[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
    if(Config.executorType == "gpu")
      run(OPGPU_mdot(this, m.asInstanceOf[DoubleMatrix]))
    else
      run(OP_mdot(this, m.asInstanceOf[DoubleMatrix], run(OP_alloc[Double](this)).asInstanceOf[DoubleMatrix]))
  }

  override def unary_-(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
    run(OP_unary_-(this, run(OP_alloc[Double](this)).asInstanceOf[DoubleMatrix]))
  }

  override def abs(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
    run(OP_abs(this, run(OP_alloc[Double](this)).asInstanceOf[DoubleMatrix]))
  }

  override def exp(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
    if(Config.executorType == "gpu")
      run(OPGPU_exp(this))
    else {
		if(Config.useNativeLibs)
      		run(OP_native_exp(this))
		else
      		run(OP_exp(this, run(OP_alloc[Double](this)).asInstanceOf[DoubleMatrix]))
	}
  }

  override def >(m: Matrix[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
    if(Config.executorType == "gpu")
      run(OPGPU_>(this, m.asInstanceOf[DoubleMatrix]))
    else
      run(OP_>(this, m.asInstanceOf[DoubleMatrix], run(OP_alloc[Double](this)).asInstanceOf[DoubleMatrix]))
  }

  override def >(d: Double)(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
    run(OP_mapDouble(this, run(OP_alloc[Double](this)).asInstanceOf[DoubleMatrix], e => if (e > d) 1 else 0))
  }

  override def <(m: Matrix[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
    run(OP_<(this, m.asInstanceOf[DoubleMatrix], run(OP_alloc[Double](this)).asInstanceOf[DoubleMatrix]))
  }
  override def <(d: Double)(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
    run(OP_mapDouble(this, run(OP_alloc[Double](this)).asInstanceOf[DoubleMatrix], e => if (e < d) 1 else 0))
  }

  override def sum[B <: DeliteDSLType](implicit ops: ArithOps[Double], conv: Double => B, pfact: DeliteProxyFactory[B], c: ClassManifest[Double]): B = {
    run(OP_sum[B](this))
  }

  override def min[B <: DeliteDSLType](implicit cmp: Double => Ordered[Double], conv: Double => B, pfact: DeliteProxyFactory[B]) : B = {
    run(OP_min[B](this))
  }

  override def max[B <: DeliteDSLType](implicit cmp: Double => Ordered[Double], conv: Double => B, pfact: DeliteProxyFactory[B]) : B = {
    run(OP_max[B](this))
  }

  ///////////////////
  // bulk operations

  override def map[B](f: Double => B)(implicit pFact: Matrix.ProxyFactory[B], c: ClassManifest[B]) : Matrix[B] =  {
    run[Matrix[B]](OP_map(this, run(OP_alloc[B](this)), f))
  }

  ////////////////////
  // RBM oeprations
  
  // RBM : SUM operation on Matrix that adds up each column (returns a row vector)
  override def sumCol: Vector[Double] = {
    if(Config.executorType == "gpu")
      run(OPGPU_sumCol(this))
    else
      run(OP_sumCol(this))
  }

  override def sumRow: Vector[Double] = {
    run(OP_sumRow(this))
  }

  // RBM : operation that replicates the given matrix to generate a larger matrix
  override def repmat(i: Int, j: Int): Matrix[Double] = {
    run(OP_repmat(this, i, j))
  }

  override def gaussian(norm: Double, sigma: Double) : Matrix[Double] = {
    run(OP_gaussian(this, norm, sigma))
  }

  // RBM : reciprocal of each elemnts (1/elms)
  // This will be replaced with map operation
  override def reciprocal: Matrix[Double] = {
    if(Config.executorType == "gpu")
      run(OPGPU_recip(this))
    else
//      run(OP_recip(this))
        run(OP_mapDouble(this, this, e => 1.0/e))
  }

  override def sigmoid: Matrix[Double] = {
	if(Config.useNativeLibs)
    	run(OP_native_sigmoid(this, run(OP_alloc[Double](this)).asInstanceOf[DoubleMatrix]))
	else
    	run(OP_mapDouble(this, run(OP_alloc[Double](this)).asInstanceOf[DoubleMatrix], e => (1.0/(1.0+math.exp(-e))).asInstanceOf[Double]))
    //run(OP_sigmoid(this))
  }

  override def addNNZvaluesOnly(m: Matrix[Double]): Matrix[Double] = {
    run(OP_addNNZvaluesOnly(this, m))
  }

  override def cutoffAndReturn(threshold: Double) : Vector[Double] = {
    run(OP_cutoffAndReturn(this, threshold*threshold))
  }
}
