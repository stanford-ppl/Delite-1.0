/* Specialized operations for LongMatrices.
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  May 5, 2010
 * modified: May 5, 2010
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
package ppl.delite.dsl.optiml.specialized


import ppl.delite.dsl.primitive.DeliteLong
import ppl.delite.cuda._
import ppl.delite.metrics._
import ppl.delite.core._
import ppl.delite.core.appinclude._
import ppl.delite.core.ops._
import ppl.delite.core.ops.specialized._
import ppl.delite.cnative._
import ppl.delite.dsl.optiml.{ArithOps, Matrix, Vector}


object LongMatrix{

  protected[optiml] case class OP_diag_single(w: Int, vals: LongVector) extends DeliteOP_SingleTask[Matrix[Long]](vals) {
    def task = {
      if (vals._length != w) throw new Exception("diag: dimensions don't agree")
      val out = new LongMatrixImpl(w,w)
      var i = 0
      while (i < w) {
        out(i,i) = vals(i)
        i += 1
      }
      out
    }
  }

  protected[optiml] case class OP_alloc[B: ClassManifest](m: LongMatrix) extends DeliteOP_SingleTask[Matrix[B]](m) {
    def task = {
      Matrix[B](m.numRows, m.numCols)
    }
  }

  protected[optiml] case class OP_allocVec[B: ClassManifest](m: LongMatrix, is_row: Boolean) extends DeliteOP_SingleTask[Vector[B]](m) {
    def task = {
      Vector[B](is_row, m.numRows)
    }
  }

  protected[optiml] case class OP_apply(val collA: LongMatrix, i: Int, j: Int)
    extends DeliteOP_SingleTask[DeliteLong](collA){

    def task = {
      collA(i,j)
    }
  }

  protected[optiml] case class OP_update(val collA: LongMatrix, i: Int, j: Int, x: Long)
    extends DeliteOP_SingleTask[DeliteUnit](collA){

    def task = {
      collA(i,j) = x
    }
  }


  /**
   * Single task versions of arithmetic ops
   */
  protected[delite] case class OP_plus_single(val collA: LongMatrix, val collB: LongMatrix)
    extends DeliteOP_SingleTask[Matrix[Long]](collA, collB) {

    def task = {
      val out = new LongMatrixImpl(collA._numRows, collA._numCols)
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

  protected[delite] case class OP_minus_single(val collA: LongMatrix, val collB: LongMatrix)
    extends DeliteOP_SingleTask[Matrix[Long]](collA, collB) {

    def task = {
      val out = new LongMatrixImpl(collA._numRows, collA._numCols)
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

  protected[optiml] case class OP_plusEquals_single(collA: LongMatrix, collB: LongMatrix)
    extends DeliteOP_MutableSingleTask[Matrix[Long]](collB)(collA) {

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

  protected[delite] case class OP_mult_single(collA: LongMatrix, collB: LongMatrix)
    extends DeliteOP_SingleTask[Matrix[Long]](collA, collB) {

    def task = {
      collA.chkEquals(collA._numCols, collB._numRows)
      val out = new LongMatrixImpl(collA._numRows, collB._numCols)
      var rl = 0
      while (rl != collA._numRows) {
        var cr = 0
        while (cr != collB._numCols) {
          var c = 0
          var acc : Long = 0
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

  protected[optiml] case class OP_native_vmult_single(m: LongMatrix, v: LongVector)
    extends DeliteOP_SingleTask[Vector[Long]](m,v){

    val out : LongVector = Vector[Long](false, m.numRows).asInstanceOf[LongVector]

    override def task = {
      //throw new UnsupportedOperationException
//DeliteNative.matrixMultLong(m._data, v._data, out._data, m._numRows, m._numCols, 1)
      if(v.isInstanceOf[LongVectorViewImpl])
        throw new UnsupportedOperationException
//DeliteNative.matVMultLong(m._data, v._data, out._data, m._numRows, m._numCols, v.asInstanceOf[LongVectorViewImpl].start, v.asInstanceOf[LongVectorViewImpl].stride)
      else
        throw new UnsupportedOperationException
//DeliteNative.matVMultLong(m._data, v._data, out._data, m._numRows, m._numCols, 0, 1)
      out
    }
  }

  protected[delite] case class OP_vmult(m: LongMatrix, v: LongVector)
    extends DeliteOP_ForEachSpec[Int, Vector[Long]](m, v)(){

    m.chkEquals(m.numCols, v.length)

    val coll = Vector.range(0, m.numRows)
    val out : LongVector = Vector[Long](false, m.numRows).asInstanceOf[LongVector]

    def func = idx => {
      var c = 0
      var acc : Long = 0
      while (c != m._numCols) {
        acc += m(idx,c) * v(c)
        c += 1
      }
      out(idx) = acc
    }
  }

  protected[delite] case class OP_vmult_single(m: LongMatrix, v: LongVector)
    extends DeliteOP_SingleTask[Vector[Long]](m,v) {

    def task = {
      m.chkEquals(m._numCols, v.length)
      val out = new LongVectorImpl(false, m._numRows)
      var rl = 0
      while (rl != m._numRows) {
        var c = 0
        var acc : Long = 0
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

  protected[delite] case class OP_smult_single(m: LongMatrix, s: Long)
    extends DeliteOP_SingleTask[Matrix[Long]](m) {

    def task = {
      val out = new LongMatrixImpl(m._numRows, m._numCols)
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


  protected[optiml] case class OP_smult_single_mutable(m: LongMatrix, s: Long)
    extends DeliteOP_MutableSingleTask[Matrix[Long]]()(m) {

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


  protected[delite] case class OP_vdot_single(m: LongMatrix, v: LongVector)
    extends DeliteOP_SingleTask[Matrix[Long]](m,v) {

    def task = {
      m.chkEquals(m._numRows, v.length)
      val out = new LongMatrixImpl(m._numRows, m._numCols)
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

  protected[delite] case class OP_trans(m: LongMatrix)
    extends DeliteOP_SingleTask[Matrix[Long]](m) {

    def task = {
      val out = new LongMatrixImpl(m._numCols,m._numRows)
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


  protected[optiml] case class OP_+(val collA: LongMatrix, val collB: LongMatrix, val out: LongMatrix)
    extends DeliteOP_ZipWith2Spec[Long,Matrix]{
    override val associative = true

    def func = (a,b) => a+b
  }

  protected[optiml] case class OP_+=(val collA: LongMatrix, val collB: LongMatrix)
    extends DeliteOP_ZipWith2Spec[Long,Matrix]{
    
    val out = collA
    final def func = (a,b) => a+b
  }

  protected[optiml] case class OP_-(val collA: LongMatrix, val collB: LongMatrix, val out: LongMatrix)
    extends DeliteOP_ZipWith2Spec[Long,Matrix]{

    def func = (a,b) => a-b

  }

  protected[optiml] case class OP_/[A](val collA: LongMatrix, val collB: LongMatrix, val out: LongMatrix)
    extends DeliteOP_ZipWith2Spec[Long,Matrix]{

    def func = (a,b) => a/b
  }

  protected[optiml] case class OP_mdot(val collA: LongMatrix, val collB: LongMatrix, val out: LongMatrix)
    extends DeliteOP_ZipWith2Spec[Long,Matrix]{

    def func = (a,b) => a*b
  }


  protected[optiml] case class OP_native_*(m1: LongMatrix, m2: LongMatrix)
    extends DeliteOP_SingleTask[Matrix[Long]](m1, m2){

    val out : LongMatrix = Matrix[Long](m1.numRows, m2.numCols).asInstanceOf[LongMatrix]

    override def task = {
      throw new UnsupportedOperationException
//DeliteNative.matrixMultLong(m1._data, m2._data, out._data, m1._numRows, m1._numCols, m2._numCols)
      out
    }
  }


  protected[optiml] case class OP_*(m1: LongMatrix, m2: LongMatrix)
    extends DeliteOP_ForEachSpec[Int, Matrix[Long]](m1, m2)(){

    m1.chkEquals(m1.numCols, m2.numRows)
    val b_trans : LongMatrix = m2.trans.asInstanceOf[LongMatrix]

    val coll = Vector.range(0, m1.numRows)
    val out : LongMatrix = Matrix[Long](m1.numRows, m2.numCols).asInstanceOf[LongMatrix]

    // TODO: could operate on block indices instead of row indices
    def func = DeliteFunc((row_idx => {
      var i = 0
      while (i < out._numCols){
        var j = 0
        var acc : Long = 0
        while (j < b_trans._numCols){
          acc += m1(row_idx, j)*b_trans(i, j)
          j += 1
        }
        out(row_idx, i) = acc
        i += 1
      }
    }), b_trans)
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

  protected[optiml] case class OP_unary_-(val coll: LongMatrix, val out: LongMatrix)
    extends DeliteOP_MapSpec[Long,Matrix] {

    def func = e => longArithOps.unary_-(e)
  }

  protected[optiml] case class OP_abs(val coll: LongMatrix, val out: LongMatrix)
    extends DeliteOP_MapSpec[Long,Matrix] {

    def func = e => longArithOps.abs(e)
  }

  protected[optiml] case class OP_exp(val coll: LongMatrix, val out: LongMatrix)
    extends DeliteOP_MapSpec[Long,Matrix] {

    def func = e => longArithOps.exp(e)
  }

  protected[optiml] case class OP_native_exp(val coll: LongMatrix)
    extends DeliteOP_SingleTask[Matrix[Long]](coll) {
	
	def task = {
		val out = new LongMatrixImpl(coll._numRows, coll._numCols)
		throw new UnsupportedOperationException
//DeliteNative.vecExpLong(coll._data, out._data, coll._numRows*coll._numCols)
		out
	}
  }

  protected[optiml] case class OP_native_sigmoid(val coll: LongMatrix, val out: LongMatrix)
    extends DeliteOP_NativeMapSpec[Long, Matrix] {

	  def func = null
    def funcNative(start: Int, end: Int) = throw new UnsupportedOperationException
//DeliteNative.vecSigmoidLong(coll._data, out._data, start, end)
	
	/*
	def task = {
		val out = new LongMatrixImpl(coll._numRows, coll._numCols)
		throw new UnsupportedOperationException
//DeliteNative.vecSigmoidLong(coll._data, out._data, coll._numRows*coll._numCols)
		out
	}
	*/
  }

  protected[optiml] case class OP_>[A](val collA: LongMatrix, val collB: LongMatrix, val out: LongMatrix)
    extends DeliteOP_ZipWith2Spec[Long,Matrix]{

    def func = (a,b) => if (a > b) 1 else 0
  }

  protected[optiml] case class OP_<[A](val collA: LongMatrix, val collB: LongMatrix, val out: LongMatrix)
    extends DeliteOP_ZipWith2Spec[Long,Matrix]{

    def func = (a,b) => if (a < b) 1 else 0
  }

  protected[optiml] case class OP_sum[B <: DeliteDSLType](val coll: LongMatrix)
    (implicit conv: Long => B, pfact: DeliteProxyFactory[B])
    extends DeliteOP_ReduceSpec[Long,B] {

    def func = (a,b) => a+b
  }

  protected[optiml] case class OP_min[B <: DeliteDSLType](val coll: LongMatrix)
    (implicit conv: Long => B, pfact: DeliteProxyFactory[B]) extends DeliteOP_ReduceSpec[Long,B] {

    def func = (a,b) => if (a < b) a else b
  }


  protected[optiml] case class OP_max[B <: DeliteDSLType](val coll: LongMatrix)
    (implicit conv: Long => B, pfact: DeliteProxyFactory[B]) extends DeliteOP_ReduceSpec[Long,B] {

    def func = (a,b) => if (a > b) a else b
  }

  protected[optiml] case class OP_map[B](val coll: LongMatrix, val out: Matrix[B], val func: Long => B)(implicit c1: ClassManifest[B])
    extends DeliteOP_Map[Long,B,Matrix]


  protected[optiml] case class OP_mapLong(val coll: LongMatrix, val out: LongMatrix, val func: Long => Long)
    extends DeliteOP_MapSpec[Long,Matrix]

  
  /////////////////////////////////////////////////////////////////////////////////////
  ///////////////////            GPU OPS        ///////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////

  protected[delite] case class OPGPU_plus_single(val collA: LongMatrix, val collB: LongMatrix)
    extends DeliteOP_SingleTask[Matrix[Long]](collA, collB) {

    def task = {
      val out = new LongMatrixImpl(collA._numRows, collA._numCols)
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

    /*override def getGPUInputs = {List(collA, collB)}
    override def getGPUConsts = {List(collA._numRows*collA._numCols)}
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Matrix[Long](elms, collA.numRows, collA.numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._numRows*collA._numCols)
	  *///override def getGPUKernelId// = {List(DeliteCuda.DVectPlusLong, DeliteCuda.AsyncLaunch1D)}
	  //*///override def getGPUKernelId// = {List(DeliteCuda.DVectPlusLong, DeliteCuda.Async2I1D)}
  }

  /*
  protected[optiml] case class OPGPU_plusEquals_single(collA: LongMatrix, collB: LongMatrix)
    extends DeliteOP_MutableSingleTask[Matrix[Long]](collB)(collA) {

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

    /*override def getGPUInputs = {List(collA, collB)}
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Matrix[Long](elms, collA.numRows, collA.numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
	  *///override def getGPUKernelId// = {List(DeliteCuda.DVectPlusLong, DeliteCuda.Async2I1D)}
  }
  */

  protected[delite] case class OPGPU_mult_single(collA: LongMatrix, collB: LongMatrix)
    extends DeliteOP_SingleTask[Matrix[Long]](collA, collB) {

    def task = {
      collA.chkEquals(collA._numCols, collB._numRows)
      val out = new LongMatrixImpl(collA._numRows, collB._numCols)
      var rl = 0
      while (rl != collA._numRows) {
        var cr = 0
        while (cr != collB._numCols) {
          var c = 0
          var acc : Long = 0
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

    /*override def getGPUInputs = List(collA, collB)
    override def getGPUConsts = List(collA._numCols, collB._numCols, collA._numRows)
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Matrix[Long](elms, collA._numRows, collB._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collB._numCols, collA._numRows, 16, 16)
	  *///override def getGPUKernelId// = {if((collA._numRows<4)||(collA._numCols<4)||(collB._numRows<4)||(collB._numCols<4)) List(DeliteCuda.MatMulLongReg,DeliteCuda.AsyncLaunch2D) else List(DeliteCuda.MatMulLong,DeliteCuda.Async3D3I)}
  }

  protected[delite] case class OPGPU_vmult_single(m: LongMatrix, v: LongVector)
    extends DeliteOP_SingleTask[Vector[Long]](m,v) {

    def task = {
      m.chkEquals(m._numCols, v.length)
      val out = new LongVectorImpl(false, m._numRows)
      var rl = 0
      while (rl != m._numRows) {
        var c = 0
        var acc : Long = 0
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
    /*override def getGPUInputs = List[AnyRef](m, v)
    override def getGPUConsts = List(m._numCols, m._numRows)
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Vector[Long](elms, false, m._numRows)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(16, m._numRows, 16, 32)
    //override def getGPUKernelDims = List(4, m._numRows, 4, 128)
    *///override def getGPUKernelId// = List(DeliteCuda.MatProdV, DeliteCuda.AsyncLaunch2D)
	  //*///override def getGPUKernelId// = {List(DeliteCuda.MatProdV, DeliteCuda.AsyncMdotV)}
  }

  protected[delite] case class OPGPU_smult_single(m: LongMatrix, s: Long)
    extends DeliteOP_SingleTask[Matrix[Long]](m) {

    def task = {
      val out = new LongMatrixImpl(m._numRows, m._numCols)
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

    /*override def getGPUInputs = List(m)
    override def getGPUConsts = List(s, m._numRows*m._numCols)
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Matrix[Long](elms, m._numRows, m._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(m._numRows*m._numCols)
    *///override def getGPUKernelId// = List(DeliteCuda.VectMultLong_S, DeliteCuda.AsyncLaunch1D)
	  //*///override def getGPUKernelId// = List(DeliteCuda.VectMultLong_S, DeliteCuda.AsyncRBM_1I1S)
  }

  protected[delite] case class OPGPU_vdot_single(m: LongMatrix, v: LongVector)
    extends DeliteOP_SingleTask[Matrix[Long]](m,v) {

    def task = {
      m.chkEquals(m._numRows, v.length)
      val out = new LongMatrixImpl(m._numRows, m._numCols)
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

    /*override def getGPUInputs = List[AnyRef](m, v)
    override def getGPUConsts = List(m._numCols, m._numRows)
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Matrix[Long](elms, m._numRows, m._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(m._numCols, m._numRows, 256, 1)
	  *///override def getGPUKernelId// = List(DeliteCuda.MatDotV, DeliteCuda.AsyncLaunch2D)
    //*///override def getGPUKernelId// = {List(DeliteCuda.MatDotV, DeliteCuda.AsyncMatDotV)}
  }

  protected[delite] case class OPGPU_trans(m: LongMatrix)
    extends DeliteOP_SingleTask[Matrix[Long]](m) {

    def task = {
      val out = new LongMatrixImpl(m._numCols,m._numRows)
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

    /*override def getGPUInputs = List(m)
    override def getGPUConsts = List(m._numCols, m._numRows)
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Matrix[Long](elms, m._numCols, m._numRows)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(m._numCols, m._numRows, 16, 16)
	  *///override def getGPUKernelId// = List(DeliteCuda.MatTransLong, DeliteCuda.AsyncLaunch2D)
    //*///override def getGPUKernelId// = {List(DeliteCuda.MatTransLong, DeliteCuda.Async3D3I)}
  }

  protected[optiml] case class OPGPU_mdot(val collA: LongMatrix, val collB: LongMatrix)
    extends DeliteOP_SingleTask[Matrix[Long]](collA, collB){

      def task = {
        val numRows = collA.numRows
        val numCols = collA.numCols
        val out = new LongMatrixImpl(numRows,numCols)
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

    /*override def getGPUInputs = List(collA, collB)
    override def getGPUConsts = List(collA._numRows*collA._numCols)
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Matrix[Long](elms, collA._numRows, collA._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._numRows*collA._numCols)
	  *///override def getGPUKernelId// = List(DeliteCuda.DVectMultLong, DeliteCuda.AsyncLaunch1D)
    //*///override def getGPUKernelId// = {List(DeliteCuda.DVectMultLong, DeliteCuda.Async2I1D)}
  }

  protected[optiml] case class OPGPU_exp(val coll: LongMatrix)
    extends DeliteOP_SingleTask[Matrix[Long]](coll) {

      def task = {
        val numRows = coll.numRows
        val numCols = coll.numCols
        val out = new LongMatrixImpl(numRows,numCols)
        var i = 0
        var j = 0
        while (i < numRows) {
          j = 0
          while(j < numCols) {
            out(i,j) = longArithOps.exp(coll(i,j))
            j += 1
          }
          i += 1
        }
        out
	    }

    /*override def getGPUInputs = List(coll)
    override def getGPUConsts = List(coll._numRows*coll._numCols)
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Matrix[Long](elms, coll.numRows, coll.numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(coll._numRows*coll._numCols)
	  *///override def getGPUKernelId// = List(DeliteCuda.VectExpLong, DeliteCuda.AsyncLaunch1D)
    //*///override def getGPUKernelId// = List(DeliteCuda.VectExpLong, DeliteCuda.AsyncRBM_1I)
  }

  protected[optiml] case class OPGPU_>[A](val collA: LongMatrix, val collB: LongMatrix)
    extends DeliteOP_SingleTask[Matrix[Long]](collA, collB){

    def task = {
        val numRows = collA.numRows
        val numCols = collA.numCols
        val out = new LongMatrixImpl(numRows,numCols)
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

    /*override def getGPUInputs = List(collA, collB)
    override def getGPUConsts = List(collA._numRows*collA._numCols)
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Matrix[Long](elms, collA._numRows, collA._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._numRows*collA._numCols)
    *///override def getGPUKernelId// = List(DeliteCuda.VectGTLong, DeliteCuda.AsyncLaunch1D)
	  //*///override def getGPUKernelId// = List(DeliteCuda.VectGTLong, DeliteCuda.AsyncRBM_2I)
  }

    protected[optiml] case class OPGPU_-(val collA: LongMatrix, val collB: LongMatrix)
    extends DeliteOP_SingleTask[Matrix[Long]](collA, collB){

      def task = {
        val numRows = collA.numRows
        val numCols = collA.numCols
        val out = new LongMatrixImpl(numRows,numCols)
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

    /*override def getGPUInputs = List(collA, collB)
    override def getGPUConsts = List(collA._numRows*collA._numCols)
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Matrix[Long](elms, collA._numRows, collA._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._numRows*collA._numCols)
    *///override def getGPUKernelId// = List(DeliteCuda.DVectMinusLong, DeliteCuda.AsyncLaunch1D)
	  //*///override def getGPUKernelId// = {List(DeliteCuda.DVectMinusLong, DeliteCuda.Async2I1D)}
  }

  protected[optiml] case class OPGPU_LongPlus(val coll: LongMatrix, val d: Long)
    extends DeliteOP_SingleTask[Matrix[Long]](coll) {

      def task = {
        val numRows = coll.numRows
        val numCols = coll.numCols
        val out = new LongMatrixImpl(numRows,numCols)
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
    
    /*override def getGPUInputs = List(coll)
    override def getGPUConsts = List(d, coll._numRows*coll._numCols)
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Matrix[Long](elms, coll._numRows, coll._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(coll._numRows*coll._numCols)
    *///override def getGPUKernelId// = List(DeliteCuda.VectPlusLong_S, DeliteCuda.AsyncLaunch1D)
	  //*///override def getGPUKernelId// = List(DeliteCuda.VectPlusLong_S, DeliteCuda.AsyncRBM_1I1S)
  }

  protected[optiml] case class OPGPU_sumCol(m: LongMatrix) extends DeliteOP_SingleTask[Vector[Long]](m) {
    def task = {
		  // assume the array is initialized to all 0
		  val width = m.numCols
	 	  val height = m.numRows
      val out = new LongVectorImpl(true, m.numCols)
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

    /*override def getGPUInputs = List(m)
    override def getGPUConsts = List(m._numRows, m._numCols)
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Vector[Long](elms, true, m._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(m._numCols)
    *///override def getGPUKernelId// = List(DeliteCuda.sumColsLong, DeliteCuda.AsyncLaunch1D)
	  //*///override def getGPUKernelId// = List(DeliteCuda.sumColsLong, DeliteCuda.AysncRBM_1I2D)
	}

  protected[optiml] case class OPGPU_recip(m: LongMatrix) extends DeliteOP_SingleTask[Matrix[Long]](m) {
		def task = {
      val out = new LongMatrixImpl(m._numRows, m._numCols)
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

    /*override def getGPUInputs = List(m)
    override def getGPUConsts = List(m._numRows*m._numCols)
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Matrix[Long](elms, m.numRows, m.numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(m._numRows*m._numCols)
    *///override def getGPUKernelId// = List(DeliteCuda.VectRecipLong, DeliteCuda.AsyncLaunch1D)
	  //*///override def getGPUKernelId// = List(DeliteCuda.VectRecipLong, DeliteCuda.AsyncRBM_1I)
  }

  protected[optiml] case class OPGPU_diag_single(w: Int, vals: LongVector) extends DeliteOP_SingleTask[Matrix[Long]](vals) {
    def task = {
      if (vals._length != w) throw new Exception("diag: dimensions don't agree")
      val out = new LongMatrixImpl(w,w)
      var i = 0
      while (i < w) {
        out(i,i) = vals(i)
        i += 1
      }
      out
    }
    /*override def getGPUInputs = List(vals)
    override def getGPUConsts = List(w)
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Matrix[Long](elms, w, w)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(w, w)
	  *///override def getGPUKernelId// = List(DeliteCuda.matDiagLong, DeliteCuda.AsyncLaunch2D)
  }

  /////////////////////////////////////////////////////////////////////////////////////
  ///////////////////         END OF GPU OPS        ///////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////


  /////////////////////
  //  RBM functions

  protected[optiml] case class OP_sumCol(m: LongMatrix) extends DeliteOP_SingleTask[Vector[Long]](m) {
    def task = {
		  // assume the array is initialized to all 0
		  val width = m.numCols
	 	  val height = m.numRows
      val out = new LongVectorImpl(true, m.numCols)
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
	}

	protected[optiml] case class OP_repmat(m: LongMatrix, iRep: Int, jRep: Int) extends DeliteOP_SingleTask[Matrix[Long]](m) {
		def task = {
      val out = new LongMatrixImpl(iRep*m._numRows, jRep*m._numCols)
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

	protected[optiml] case class OP_recip(m: LongMatrix) extends DeliteOP_SingleTask[Matrix[Long]](m) {
		def task = {
      val out = new LongMatrixImpl(m._numRows, m._numCols)
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
  }
 
  protected[delite] case class OP_sigmoid(val collA: LongMatrix)
    extends DeliteOP_SingleTask[Matrix[Long]](collA) {

    def task = {
      val out = new LongMatrixImpl(collA._numRows, collA._numCols)
      var i = 0
      var j = 0
      while (i < out._numRows){
        while (j < out._numCols){
          out(i,j) = (1.0/(1.0+math.exp(-collA(i,j)))).asInstanceOf[Long]
          j += 1
        }
        j = 0
        i += 1
      }
      out
    }
  }

}


trait LongMatrix extends Matrix[Long] {
  import LongMatrix._

  protected[optiml] var _data: Array[Long]

  override def apply(i: Int, j: Int) : Long
  override def lifted_apply(i: Int, j: Int) : Long  = {
    run(OP_apply(this,i,j))
  }

  override def update(row: Int, col: Int, x: Long)
  override def lifted_update(row: Int, col:Int, x: Long) : Unit = {
    run(OP_update(this,row,col,x))
  }

  override def dc_update(i: Int, x: Long)
  override def dc_apply(i: Int) : Long    

  //////////
  // arith

  // gda, nb, linreg ops
  override def +(m: Matrix[Long])(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Matrix[Long] = {
    if(Config.executorType == "gpu")
      run(OPGPU_plus_single(this,m.asInstanceOf[LongMatrix]))
    else
      //run(OP_plus_single(this,m.asInstanceOf[LongMatrix]))
    run(OP_+(this,m.asInstanceOf[LongMatrix],run(OP_alloc[Long](this)).asInstanceOf[LongMatrix]))
  }

  // for performance testing only
  def plusEqualsZipWith(m: Matrix[Long])(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Unit = {
    run(OP_+=(this,m.asInstanceOf[LongMatrix]))
  }

  def plusEqualsSingle(m: Matrix[Long])(implicit ops: ArithOps[Long], c: ClassManifest[Long]): Unit = {
    //if(Config.executorType == "gpu")
    //  run(OPGPU_plusEquals_single(this,m.asInstanceOf[LongMatrix]))
    //else
      run(OP_plusEquals_single(this,m.asInstanceOf[LongMatrix]))
  }

  override def +=(m: Matrix[Long])(implicit ops: ArithOps[Long], c: ClassManifest[Long]): Unit = {
    //run(OP_plusEquals_single(this,m.asInstanceOf[LongMatrix]))
    run(OP_+=(this,m.asInstanceOf[LongMatrix]))
  }

  override def *(m: Matrix[Long])(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Matrix[Long] = {
    if(Config.executorType == "gpu")
      run(OPGPU_mult_single(this, m.asInstanceOf[LongMatrix]))
    else{
      if (Config.useNativeLibs){
        run(OP_native_*(this, m.asInstanceOf[LongMatrix]))
      }
      else{
        run(OP_*(this, m.asInstanceOf[LongMatrix]))
      }
    }
  }

  override def *(v: Vector[Long])(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Vector[Long] = {
    if(Config.executorType == "gpu")
      run[Vector[Long]](OPGPU_vmult_single(this, v.asInstanceOf[LongVector]))
    else{
      if (Config.useNativeLibs){
        run(OP_native_vmult_single(this, v.asInstanceOf[LongVector]))
      }
      else{
        //run(OP_vmult_single(this, v.asInstanceOf[LongVector]))
        run(OP_vmult(this, v.asInstanceOf[LongVector]))
      }
    }
  }

  override def *(d: Long)(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Matrix[Long] = {
    //run(OP_smult_single(this, d))
    if(Config.executorType == "gpu")
      run(OPGPU_smult_single(this, d))
    else
      run(OP_mapLong(this, run(OP_alloc[Long](this)).asInstanceOf[LongMatrix], e => e*d))
  }

  override def *=(d: Long)(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Matrix[Long] = {
    //run(OP_smult_single_mutable(this, d))
    run(OP_mapLong(this, this, e => e*d))    
  }

  override def dot(v: Vector[Long])(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Matrix[Long] = {
    if(Config.executorType == "gpu")
      run(OPGPU_vdot_single(this,v.asInstanceOf[LongVector]))
    else
      run(OP_vdot_single(this,v.asInstanceOf[LongVector]))
  }

  override def trans(implicit c: ClassManifest[Long]) : Matrix[Long] = {
    if(Config.executorType == "gpu")
      run(OPGPU_trans(this))
    else
      run(OP_trans(this))
  }

  // others
  
  override def +(d: Long)(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Matrix[Long] = {
    if(Config.executorType == "gpu")
      run(OPGPU_LongPlus(this, d))
    else
      run(OP_mapLong(this, run(OP_alloc[Long](this)).asInstanceOf[LongMatrix], e => e+d))
  }

  override def -(m: Matrix[Long])(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Matrix[Long] = {
    if(Config.executorType == "gpu")
      run(OPGPU_-(this, m.asInstanceOf[LongMatrix]));
    else{
      //run(OP_minus_single(this,m.asInstanceOf[LongMatrix]))
      run(OP_-(this, m.asInstanceOf[LongMatrix], run(OP_alloc[Long](this)).asInstanceOf[LongMatrix]));
    }
  }

  override def -(d: Long)(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Matrix[Long] = {
    run(OP_mapLong(this, run(OP_alloc[Long](this)).asInstanceOf[LongMatrix], e => e-d))
  }

  override def /(m: Matrix[Long])(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Matrix[Long] = {
    run(OP_/(this, m.asInstanceOf[LongMatrix], run(OP_alloc[Long](this)).asInstanceOf[LongMatrix]))
  }
  override def /(d: Long)(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Matrix[Long] = {
      run(OP_mapLong(this, run(OP_alloc[Long](this)).asInstanceOf[LongMatrix], e => e/d))
  }

  override def dot(m: Matrix[Long])(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Matrix[Long] = {
    if(Config.executorType == "gpu")
      run(OPGPU_mdot(this, m.asInstanceOf[LongMatrix]))
    else
      run(OP_mdot(this, m.asInstanceOf[LongMatrix], run(OP_alloc[Long](this)).asInstanceOf[LongMatrix]))
  }

  override def unary_-(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Matrix[Long] = {
    run(OP_unary_-(this, run(OP_alloc[Long](this)).asInstanceOf[LongMatrix]))
  }

  override def abs(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Matrix[Long] = {
    run(OP_abs(this, run(OP_alloc[Long](this)).asInstanceOf[LongMatrix]))
  }

  override def exp(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Matrix[Long] = {
    if(Config.executorType == "gpu")
      run(OPGPU_exp(this))
    else {
		
      		run(OP_exp(this, run(OP_alloc[Long](this)).asInstanceOf[LongMatrix]))
	}
  }

  override def >(m: Matrix[Long])(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Matrix[Long] = {
    if(Config.executorType == "gpu")
      run(OPGPU_>(this, m.asInstanceOf[LongMatrix]))
    else
      run(OP_>(this, m.asInstanceOf[LongMatrix], run(OP_alloc[Long](this)).asInstanceOf[LongMatrix]))
  }

  override def >(d: Long)(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Matrix[Long] = {
    run(OP_mapLong(this, run(OP_alloc[Long](this)).asInstanceOf[LongMatrix], e => if (e > d) 1 else 0))
  }

  override def <(m: Matrix[Long])(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Matrix[Long] = {
    run(OP_<(this, m.asInstanceOf[LongMatrix], run(OP_alloc[Long](this)).asInstanceOf[LongMatrix]))
  }
  override def <(d: Long)(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Matrix[Long] = {
    run(OP_mapLong(this, run(OP_alloc[Long](this)).asInstanceOf[LongMatrix], e => if (e < d) 1 else 0))
  }

  override def sum[B <: DeliteDSLType](implicit ops: ArithOps[Long], conv: Long => B, pfact: DeliteProxyFactory[B], c: ClassManifest[Long]): B = {
    run(OP_sum[B](this))
  }

  override def min[B <: DeliteDSLType](implicit cmp: Long => Ordered[Long], conv: Long => B, pfact: DeliteProxyFactory[B]) : B = {
    run(OP_min[B](this))
  }

  override def max[B <: DeliteDSLType](implicit cmp: Long => Ordered[Long], conv: Long => B, pfact: DeliteProxyFactory[B]) : B = {
    run(OP_max[B](this))
  }

  ///////////////////
  // bulk operations

  override def map[B](f: Long => B)(implicit pFact: Matrix.ProxyFactory[B], c: ClassManifest[B]) : Matrix[B] =  {
    run[Matrix[B]](OP_map(this, run(OP_alloc[B](this)), f))
  }

  ////////////////////
  // RBM oeprations
  
  // RBM : SUM operation on Matrix that adds up each column (returns a row vector)
  override def sumCol: Vector[Long] = {
    if(Config.executorType == "gpu")
      run(OPGPU_sumCol(this))
    else
      run(OP_sumCol(this))
  }

  // RBM : operation that replicates the given matrix to generate a larger matrix
  override def repmat(i: Int, j: Int): Matrix[Long] = {
    run(OP_repmat(this, i, j))
  }

  // RBM : reciprocal of each elemnts (1/elms)
  // This will be replaced with map operation
  override def reciprocal: Matrix[Long] = {
    if(Config.executorType == "gpu")
      run(OPGPU_recip(this))
    else
      run(OP_recip(this))
  }

  override def sigmoid: Matrix[Long] = {
	
    	run(OP_mapLong(this, run(OP_alloc[Long](this)).asInstanceOf[LongMatrix], e => (1.0/(1.0+math.exp(-e))).asInstanceOf[Long]))
    //run(OP_sigmoid(this))
  }
  
}
