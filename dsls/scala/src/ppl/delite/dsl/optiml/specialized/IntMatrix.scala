/* Specialized operations for IntMatrices.
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  May 5, 2010
 * modified: May 5, 2010
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
package ppl.delite.dsl.optiml.specialized


import ppl.delite.dsl.primitive.DeliteInt
import ppl.delite.cuda._
import ppl.delite.metrics._
import ppl.delite.core._
import ppl.delite.core.appinclude._
import ppl.delite.core.ops._
import ppl.delite.core.ops.specialized._
import ppl.delite.cnative._
import ppl.delite.dsl.optiml.{ArithOps, Matrix, Vector}


object IntMatrix{

  protected[optiml] case class OP_diag_single(w: Int, vals: IntVector) extends DeliteOP_SingleTask[Matrix[Int]](vals) {
    def task = {
      if (vals._length != w) throw new Exception("diag: dimensions don't agree")
      val out = new IntMatrixImpl(w,w)
      var i = 0
      while (i < w) {
        out(i,i) = vals(i)
        i += 1
      }
      out
    }
  }

  protected[optiml] case class OP_alloc[B: ClassManifest](m: IntMatrix) extends DeliteOP_SingleTask[Matrix[B]](m) {
    def task = {
      Matrix[B](m.numRows, m.numCols)
    }
  }

  protected[optiml] case class OP_allocVec[B: ClassManifest](m: IntMatrix, is_row: Boolean) extends DeliteOP_SingleTask[Vector[B]](m) {
    def task = {
      Vector[B](is_row, m.numRows)
    }
  }

  protected[optiml] case class OP_apply(val collA: IntMatrix, i: Int, j: Int)
    extends DeliteOP_SingleTask[DeliteInt](collA){

    def task = {
      collA(i,j)
    }
  }

  protected[optiml] case class OP_update(val collA: IntMatrix, i: Int, j: Int, x: Int)
    extends DeliteOP_SingleTask[DeliteUnit](collA){

    def task = {
      collA(i,j) = x
    }
  }


  /**
   * Single task versions of arithmetic ops
   */
  protected[delite] case class OP_plus_single(val collA: IntMatrix, val collB: IntMatrix)
    extends DeliteOP_SingleTask[Matrix[Int]](collA, collB) {

    def task = {
      val out = new IntMatrixImpl(collA._numRows, collA._numCols)
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

  protected[delite] case class OP_minus_single(val collA: IntMatrix, val collB: IntMatrix)
    extends DeliteOP_SingleTask[Matrix[Int]](collA, collB) {

    def task = {
      val out = new IntMatrixImpl(collA._numRows, collA._numCols)
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

  protected[optiml] case class OP_plusEquals_single(collA: IntMatrix, collB: IntMatrix)
    extends DeliteOP_MutableSingleTask[Matrix[Int]](collB)(collA) {

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

  protected[delite] case class OP_mult_single(collA: IntMatrix, collB: IntMatrix)
    extends DeliteOP_SingleTask[Matrix[Int]](collA, collB) {

    def task = {
      collA.chkEquals(collA._numCols, collB._numRows)
      val out = new IntMatrixImpl(collA._numRows, collB._numCols)
      var rl = 0
      while (rl != collA._numRows) {
        var cr = 0
        while (cr != collB._numCols) {
          var c = 0
          var acc : Int = 0
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

  protected[optiml] case class OP_native_vmult_single(m: IntMatrix, v: IntVector)
    extends DeliteOP_SingleTask[Vector[Int]](m,v){

    val out : IntVector = Vector[Int](false, m.numRows).asInstanceOf[IntVector]

    override def task = {
      //throw new UnsupportedOperationException
//DeliteNative.matrixMultInt(m._data, v._data, out._data, m._numRows, m._numCols, 1)
      if(v.isInstanceOf[IntVectorViewImpl])
        throw new UnsupportedOperationException
//DeliteNative.matVMultInt(m._data, v._data, out._data, m._numRows, m._numCols, v.asInstanceOf[IntVectorViewImpl].start, v.asInstanceOf[IntVectorViewImpl].stride)
      else
        throw new UnsupportedOperationException
//DeliteNative.matVMultInt(m._data, v._data, out._data, m._numRows, m._numCols, 0, 1)
      out
    }
  }

  protected[delite] case class OP_vmult(m: IntMatrix, v: IntVector)
    extends DeliteOP_ForEachSpec[Int, Vector[Int]](m, v)(){

    m.chkEquals(m.numCols, v.length)

    val coll = Vector.range(0, m.numRows)
    val out : IntVector = Vector[Int](false, m.numRows).asInstanceOf[IntVector]

    def func = idx => {
      var c = 0
      var acc : Int = 0
      while (c != m._numCols) {
        acc += m(idx,c) * v(c)
        c += 1
      }
      out(idx) = acc
    }
  }

  protected[delite] case class OP_vmult_single(m: IntMatrix, v: IntVector)
    extends DeliteOP_SingleTask[Vector[Int]](m,v) {

    def task = {
      m.chkEquals(m._numCols, v.length)
      val out = new IntVectorImpl(false, m._numRows)
      var rl = 0
      while (rl != m._numRows) {
        var c = 0
        var acc : Int = 0
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

  protected[delite] case class OP_smult_single(m: IntMatrix, s: Int)
    extends DeliteOP_SingleTask[Matrix[Int]](m) {

    def task = {
      val out = new IntMatrixImpl(m._numRows, m._numCols)
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


  protected[optiml] case class OP_smult_single_mutable(m: IntMatrix, s: Int)
    extends DeliteOP_MutableSingleTask[Matrix[Int]]()(m) {

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


  protected[delite] case class OP_vdot_single(m: IntMatrix, v: IntVector)
    extends DeliteOP_SingleTask[Matrix[Int]](m,v) {

    def task = {
      m.chkEquals(m._numRows, v.length)
      val out = new IntMatrixImpl(m._numRows, m._numCols)
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

  protected[delite] case class OP_trans(m: IntMatrix)
    extends DeliteOP_SingleTask[Matrix[Int]](m) {

    def task = {
      val out = new IntMatrixImpl(m._numCols,m._numRows)
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


  protected[optiml] case class OP_+(val collA: IntMatrix, val collB: IntMatrix, val out: IntMatrix)
    extends DeliteOP_ZipWith2Spec[Int,Matrix]{
    override val associative = true

    def func = (a,b) => a+b
  }

  protected[optiml] case class OP_+=(val collA: IntMatrix, val collB: IntMatrix)
    extends DeliteOP_ZipWith2Spec[Int,Matrix]{
    
    val out = collA
    final def func = (a,b) => a+b
  }

  protected[optiml] case class OP_-(val collA: IntMatrix, val collB: IntMatrix, val out: IntMatrix)
    extends DeliteOP_ZipWith2Spec[Int,Matrix]{

    def func = (a,b) => a-b

  }

  protected[optiml] case class OP_/[A](val collA: IntMatrix, val collB: IntMatrix, val out: IntMatrix)
    extends DeliteOP_ZipWith2Spec[Int,Matrix]{

    def func = (a,b) => a/b
  }

  protected[optiml] case class OP_mdot(val collA: IntMatrix, val collB: IntMatrix, val out: IntMatrix)
    extends DeliteOP_ZipWith2Spec[Int,Matrix]{

    def func = (a,b) => a*b
  }


  protected[optiml] case class OP_native_*(m1: IntMatrix, m2: IntMatrix)
    extends DeliteOP_SingleTask[Matrix[Int]](m1, m2){

    val out : IntMatrix = Matrix[Int](m1.numRows, m2.numCols).asInstanceOf[IntMatrix]

    override def task = {
      throw new UnsupportedOperationException
//DeliteNative.matrixMultInt(m1._data, m2._data, out._data, m1._numRows, m1._numCols, m2._numCols)
      out
    }
  }


  protected[optiml] case class OP_*(m1: IntMatrix, m2: IntMatrix)
    extends DeliteOP_ForEachSpec[Int, Matrix[Int]](m1, m2)(){

    m1.chkEquals(m1.numCols, m2.numRows)
    val b_trans : IntMatrix = m2.trans.asInstanceOf[IntMatrix]

    val coll = Vector.range(0, m1.numRows)
    val out : IntMatrix = Matrix[Int](m1.numRows, m2.numCols).asInstanceOf[IntMatrix]

    // TODO: could operate on block indices instead of row indices
    def func = DeliteFunc((row_idx => {
      var i = 0
      while (i < out._numCols){
        var j = 0
        var acc : Int = 0
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

  protected[optiml] case class OP_unary_-(val coll: IntMatrix, val out: IntMatrix)
    extends DeliteOP_MapSpec[Int,Matrix] {

    def func = e => intArithOps.unary_-(e)
  }

  protected[optiml] case class OP_abs(val coll: IntMatrix, val out: IntMatrix)
    extends DeliteOP_MapSpec[Int,Matrix] {

    def func = e => intArithOps.abs(e)
  }

  protected[optiml] case class OP_exp(val coll: IntMatrix, val out: IntMatrix)
    extends DeliteOP_MapSpec[Int,Matrix] {

    def func = e => intArithOps.exp(e)
  }

  protected[optiml] case class OP_native_exp(val coll: IntMatrix)
    extends DeliteOP_SingleTask[Matrix[Int]](coll) {
	
	def task = {
		val out = new IntMatrixImpl(coll._numRows, coll._numCols)
		throw new UnsupportedOperationException
//DeliteNative.vecExpInt(coll._data, out._data, coll._numRows*coll._numCols)
		out
	}
  }

  protected[optiml] case class OP_native_sigmoid(val coll: IntMatrix, val out: IntMatrix)
    extends DeliteOP_NativeMapSpec[Int, Matrix] {

	  def func = null
    def funcNative(start: Int, end: Int) = throw new UnsupportedOperationException
//DeliteNative.vecSigmoidInt(coll._data, out._data, start, end)
	
	/*
	def task = {
		val out = new IntMatrixImpl(coll._numRows, coll._numCols)
		throw new UnsupportedOperationException
//DeliteNative.vecSigmoidInt(coll._data, out._data, coll._numRows*coll._numCols)
		out
	}
	*/
  }

  protected[optiml] case class OP_>[A](val collA: IntMatrix, val collB: IntMatrix, val out: IntMatrix)
    extends DeliteOP_ZipWith2Spec[Int,Matrix]{

    def func = (a,b) => if (a > b) 1 else 0
  }

  protected[optiml] case class OP_<[A](val collA: IntMatrix, val collB: IntMatrix, val out: IntMatrix)
    extends DeliteOP_ZipWith2Spec[Int,Matrix]{

    def func = (a,b) => if (a < b) 1 else 0
  }

  protected[optiml] case class OP_sum[B <: DeliteDSLType](val coll: IntMatrix)
    (implicit conv: Int => B, pfact: DeliteProxyFactory[B])
    extends DeliteOP_ReduceSpec[Int,B] {

    def func = (a,b) => a+b
  }

  protected[optiml] case class OP_min[B <: DeliteDSLType](val coll: IntMatrix)
    (implicit conv: Int => B, pfact: DeliteProxyFactory[B]) extends DeliteOP_ReduceSpec[Int,B] {

    def func = (a,b) => if (a < b) a else b
  }


  protected[optiml] case class OP_max[B <: DeliteDSLType](val coll: IntMatrix)
    (implicit conv: Int => B, pfact: DeliteProxyFactory[B]) extends DeliteOP_ReduceSpec[Int,B] {

    def func = (a,b) => if (a > b) a else b
  }

  protected[optiml] case class OP_map[B](val coll: IntMatrix, val out: Matrix[B], val func: Int => B)(implicit c1: ClassManifest[B])
    extends DeliteOP_Map[Int,B,Matrix]


  protected[optiml] case class OP_mapInt(val coll: IntMatrix, val out: IntMatrix, val func: Int => Int)
    extends DeliteOP_MapSpec[Int,Matrix]

  
  /////////////////////////////////////////////////////////////////////////////////////
  ///////////////////            GPU OPS        ///////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////

  protected[delite] case class OPGPU_plus_single(val collA: IntMatrix, val collB: IntMatrix)
    extends DeliteOP_SingleTask[Matrix[Int]](collA, collB) {

    def task = {
      val out = new IntMatrixImpl(collA._numRows, collA._numCols)
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
      val elms = new Array[Int](0)
      val gout = Matrix[Int](elms, collA.numRows, collA.numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._numRows*collA._numCols)
	  *///override def getGPUKernelId// = {List(DeliteCuda.DVectPlusInt, DeliteCuda.AsyncLaunch1D)}
	  //*///override def getGPUKernelId// = {List(DeliteCuda.DVectPlusInt, DeliteCuda.Async2I1D)}
  }

  /*
  protected[optiml] case class OPGPU_plusEquals_single(collA: IntMatrix, collB: IntMatrix)
    extends DeliteOP_MutableSingleTask[Matrix[Int]](collB)(collA) {

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
      val elms = new Array[Int](0)
      val gout = Matrix[Int](elms, collA.numRows, collA.numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
	  *///override def getGPUKernelId// = {List(DeliteCuda.DVectPlusInt, DeliteCuda.Async2I1D)}
  }
  */

  protected[delite] case class OPGPU_mult_single(collA: IntMatrix, collB: IntMatrix)
    extends DeliteOP_SingleTask[Matrix[Int]](collA, collB) {

    def task = {
      collA.chkEquals(collA._numCols, collB._numRows)
      val out = new IntMatrixImpl(collA._numRows, collB._numCols)
      var rl = 0
      while (rl != collA._numRows) {
        var cr = 0
        while (cr != collB._numCols) {
          var c = 0
          var acc : Int = 0
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
      val elms = new Array[Int](0)
      val gout = Matrix[Int](elms, collA._numRows, collB._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collB._numCols, collA._numRows, 16, 16)
	  *///override def getGPUKernelId// = {if((collA._numRows<4)||(collA._numCols<4)||(collB._numRows<4)||(collB._numCols<4)) List(DeliteCuda.MatMulIntReg,DeliteCuda.AsyncLaunch2D) else List(DeliteCuda.MatMulInt,DeliteCuda.Async3D3I)}
  }

  protected[delite] case class OPGPU_vmult_single(m: IntMatrix, v: IntVector)
    extends DeliteOP_SingleTask[Vector[Int]](m,v) {

    def task = {
      m.chkEquals(m._numCols, v.length)
      val out = new IntVectorImpl(false, m._numRows)
      var rl = 0
      while (rl != m._numRows) {
        var c = 0
        var acc : Int = 0
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
      val elms = new Array[Int](0)
      val gout = Vector[Int](elms, false, m._numRows)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(16, m._numRows, 16, 32)
    //override def getGPUKernelDims = List(4, m._numRows, 4, 128)
    *///override def getGPUKernelId// = List(DeliteCuda.MatProdV, DeliteCuda.AsyncLaunch2D)
	  //*///override def getGPUKernelId// = {List(DeliteCuda.MatProdV, DeliteCuda.AsyncMdotV)}
  }

  protected[delite] case class OPGPU_smult_single(m: IntMatrix, s: Int)
    extends DeliteOP_SingleTask[Matrix[Int]](m) {

    def task = {
      val out = new IntMatrixImpl(m._numRows, m._numCols)
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
      val elms = new Array[Int](0)
      val gout = Matrix[Int](elms, m._numRows, m._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(m._numRows*m._numCols)
    *///override def getGPUKernelId// = List(DeliteCuda.VectMultInt_S, DeliteCuda.AsyncLaunch1D)
	  //*///override def getGPUKernelId// = List(DeliteCuda.VectMultInt_S, DeliteCuda.AsyncRBM_1I1S)
  }

  protected[delite] case class OPGPU_vdot_single(m: IntMatrix, v: IntVector)
    extends DeliteOP_SingleTask[Matrix[Int]](m,v) {

    def task = {
      m.chkEquals(m._numRows, v.length)
      val out = new IntMatrixImpl(m._numRows, m._numCols)
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
      val elms = new Array[Int](0)
      val gout = Matrix[Int](elms, m._numRows, m._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(m._numCols, m._numRows, 256, 1)
	  *///override def getGPUKernelId// = List(DeliteCuda.MatDotV, DeliteCuda.AsyncLaunch2D)
    //*///override def getGPUKernelId// = {List(DeliteCuda.MatDotV, DeliteCuda.AsyncMatDotV)}
  }

  protected[delite] case class OPGPU_trans(m: IntMatrix)
    extends DeliteOP_SingleTask[Matrix[Int]](m) {

    def task = {
      val out = new IntMatrixImpl(m._numCols,m._numRows)
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
      val elms = new Array[Int](0)
      val gout = Matrix[Int](elms, m._numCols, m._numRows)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(m._numCols, m._numRows, 16, 16)
	  *///override def getGPUKernelId// = List(DeliteCuda.MatTransInt, DeliteCuda.AsyncLaunch2D)
    //*///override def getGPUKernelId// = {List(DeliteCuda.MatTransInt, DeliteCuda.Async3D3I)}
  }

  protected[optiml] case class OPGPU_mdot(val collA: IntMatrix, val collB: IntMatrix)
    extends DeliteOP_SingleTask[Matrix[Int]](collA, collB){

      def task = {
        val numRows = collA.numRows
        val numCols = collA.numCols
        val out = new IntMatrixImpl(numRows,numCols)
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
      val elms = new Array[Int](0)
      val gout = Matrix[Int](elms, collA._numRows, collA._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._numRows*collA._numCols)
	  *///override def getGPUKernelId// = List(DeliteCuda.DVectMultInt, DeliteCuda.AsyncLaunch1D)
    //*///override def getGPUKernelId// = {List(DeliteCuda.DVectMultInt, DeliteCuda.Async2I1D)}
  }

  protected[optiml] case class OPGPU_exp(val coll: IntMatrix)
    extends DeliteOP_SingleTask[Matrix[Int]](coll) {

      def task = {
        val numRows = coll.numRows
        val numCols = coll.numCols
        val out = new IntMatrixImpl(numRows,numCols)
        var i = 0
        var j = 0
        while (i < numRows) {
          j = 0
          while(j < numCols) {
            out(i,j) = intArithOps.exp(coll(i,j))
            j += 1
          }
          i += 1
        }
        out
	    }

    /*override def getGPUInputs = List(coll)
    override def getGPUConsts = List(coll._numRows*coll._numCols)
    override def getGPUOutput = {
      val elms = new Array[Int](0)
      val gout = Matrix[Int](elms, coll.numRows, coll.numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(coll._numRows*coll._numCols)
	  *///override def getGPUKernelId// = List(DeliteCuda.VectExpInt, DeliteCuda.AsyncLaunch1D)
    //*///override def getGPUKernelId// = List(DeliteCuda.VectExpInt, DeliteCuda.AsyncRBM_1I)
  }

  protected[optiml] case class OPGPU_>[A](val collA: IntMatrix, val collB: IntMatrix)
    extends DeliteOP_SingleTask[Matrix[Int]](collA, collB){

    def task = {
        val numRows = collA.numRows
        val numCols = collA.numCols
        val out = new IntMatrixImpl(numRows,numCols)
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
      val elms = new Array[Int](0)
      val gout = Matrix[Int](elms, collA._numRows, collA._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._numRows*collA._numCols)
    *///override def getGPUKernelId// = List(DeliteCuda.VectGTInt, DeliteCuda.AsyncLaunch1D)
	  //*///override def getGPUKernelId// = List(DeliteCuda.VectGTInt, DeliteCuda.AsyncRBM_2I)
  }

    protected[optiml] case class OPGPU_-(val collA: IntMatrix, val collB: IntMatrix)
    extends DeliteOP_SingleTask[Matrix[Int]](collA, collB){

      def task = {
        val numRows = collA.numRows
        val numCols = collA.numCols
        val out = new IntMatrixImpl(numRows,numCols)
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
      val elms = new Array[Int](0)
      val gout = Matrix[Int](elms, collA._numRows, collA._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._numRows*collA._numCols)
    *///override def getGPUKernelId// = List(DeliteCuda.DVectMinusInt, DeliteCuda.AsyncLaunch1D)
	  //*///override def getGPUKernelId// = {List(DeliteCuda.DVectMinusInt, DeliteCuda.Async2I1D)}
  }

  protected[optiml] case class OPGPU_IntPlus(val coll: IntMatrix, val d: Int)
    extends DeliteOP_SingleTask[Matrix[Int]](coll) {

      def task = {
        val numRows = coll.numRows
        val numCols = coll.numCols
        val out = new IntMatrixImpl(numRows,numCols)
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
      val elms = new Array[Int](0)
      val gout = Matrix[Int](elms, coll._numRows, coll._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(coll._numRows*coll._numCols)
    *///override def getGPUKernelId// = List(DeliteCuda.VectPlusInt_S, DeliteCuda.AsyncLaunch1D)
	  //*///override def getGPUKernelId// = List(DeliteCuda.VectPlusInt_S, DeliteCuda.AsyncRBM_1I1S)
  }

  protected[optiml] case class OPGPU_sumCol(m: IntMatrix) extends DeliteOP_SingleTask[Vector[Int]](m) {
    def task = {
		  // assume the array is initialized to all 0
		  val width = m.numCols
	 	  val height = m.numRows
      val out = new IntVectorImpl(true, m.numCols)
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
      val elms = new Array[Int](0)
      val gout = Vector[Int](elms, true, m._numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(m._numCols)
    *///override def getGPUKernelId// = List(DeliteCuda.sumColsInt, DeliteCuda.AsyncLaunch1D)
	  //*///override def getGPUKernelId// = List(DeliteCuda.sumColsInt, DeliteCuda.AysncRBM_1I2D)
	}

  protected[optiml] case class OPGPU_recip(m: IntMatrix) extends DeliteOP_SingleTask[Matrix[Int]](m) {
		def task = {
      val out = new IntMatrixImpl(m._numRows, m._numCols)
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
      val elms = new Array[Int](0)
      val gout = Matrix[Int](elms, m.numRows, m.numCols)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(m._numRows*m._numCols)
    *///override def getGPUKernelId// = List(DeliteCuda.VectRecipInt, DeliteCuda.AsyncLaunch1D)
	  //*///override def getGPUKernelId// = List(DeliteCuda.VectRecipInt, DeliteCuda.AsyncRBM_1I)
  }

  protected[optiml] case class OPGPU_diag_single(w: Int, vals: IntVector) extends DeliteOP_SingleTask[Matrix[Int]](vals) {
    def task = {
      if (vals._length != w) throw new Exception("diag: dimensions don't agree")
      val out = new IntMatrixImpl(w,w)
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
      val elms = new Array[Int](0)
      val gout = Matrix[Int](elms, w, w)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(w, w)
	  *///override def getGPUKernelId// = List(DeliteCuda.matDiagInt, DeliteCuda.AsyncLaunch2D)
  }

  /////////////////////////////////////////////////////////////////////////////////////
  ///////////////////         END OF GPU OPS        ///////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////


  /////////////////////
  //  RBM functions

  protected[optiml] case class OP_sumCol(m: IntMatrix) extends DeliteOP_SingleTask[Vector[Int]](m) {
    def task = {
		  // assume the array is initialized to all 0
		  val width = m.numCols
	 	  val height = m.numRows
      val out = new IntVectorImpl(true, m.numCols)
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

	protected[optiml] case class OP_repmat(m: IntMatrix, iRep: Int, jRep: Int) extends DeliteOP_SingleTask[Matrix[Int]](m) {
		def task = {
      val out = new IntMatrixImpl(iRep*m._numRows, jRep*m._numCols)
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

	protected[optiml] case class OP_recip(m: IntMatrix) extends DeliteOP_SingleTask[Matrix[Int]](m) {
		def task = {
      val out = new IntMatrixImpl(m._numRows, m._numCols)
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
 
  protected[delite] case class OP_sigmoid(val collA: IntMatrix)
    extends DeliteOP_SingleTask[Matrix[Int]](collA) {

    def task = {
      val out = new IntMatrixImpl(collA._numRows, collA._numCols)
      var i = 0
      var j = 0
      while (i < out._numRows){
        while (j < out._numCols){
          out(i,j) = (1.0/(1.0+math.exp(-collA(i,j)))).asInstanceOf[Int]
          j += 1
        }
        j = 0
        i += 1
      }
      out
    }
  }

}


trait IntMatrix extends Matrix[Int] {
  import IntMatrix._

  protected[optiml] var _data: Array[Int]

  override def apply(i: Int, j: Int) : Int
  override def lifted_apply(i: Int, j: Int) : Int  = {
    run(OP_apply(this,i,j))
  }

  override def update(row: Int, col: Int, x: Int)
  override def lifted_update(row: Int, col:Int, x: Int) : Unit = {
    run(OP_update(this,row,col,x))
  }

  override def dc_update(i: Int, x: Int)
  override def dc_apply(i: Int) : Int    

  //////////
  // arith

  // gda, nb, linreg ops
  override def +(m: Matrix[Int])(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Matrix[Int] = {
    if(Config.executorType == "gpu")
      run(OPGPU_plus_single(this,m.asInstanceOf[IntMatrix]))
    else
      //run(OP_plus_single(this,m.asInstanceOf[IntMatrix]))
    run(OP_+(this,m.asInstanceOf[IntMatrix],run(OP_alloc[Int](this)).asInstanceOf[IntMatrix]))
  }

  // for performance testing only
  def plusEqualsZipWith(m: Matrix[Int])(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Unit = {
    run(OP_+=(this,m.asInstanceOf[IntMatrix]))
  }

  def plusEqualsSingle(m: Matrix[Int])(implicit ops: ArithOps[Int], c: ClassManifest[Int]): Unit = {
    //if(Config.executorType == "gpu")
    //  run(OPGPU_plusEquals_single(this,m.asInstanceOf[IntMatrix]))
    //else
      run(OP_plusEquals_single(this,m.asInstanceOf[IntMatrix]))
  }

  override def +=(m: Matrix[Int])(implicit ops: ArithOps[Int], c: ClassManifest[Int]): Unit = {
    //run(OP_plusEquals_single(this,m.asInstanceOf[IntMatrix]))
    run(OP_+=(this,m.asInstanceOf[IntMatrix]))
  }

  override def *(m: Matrix[Int])(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Matrix[Int] = {
    if(Config.executorType == "gpu")
      run(OPGPU_mult_single(this, m.asInstanceOf[IntMatrix]))
    else{
      if (Config.useNativeLibs){
        run(OP_native_*(this, m.asInstanceOf[IntMatrix]))
      }
      else{
        run(OP_*(this, m.asInstanceOf[IntMatrix]))
      }
    }
  }

  override def *(v: Vector[Int])(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Vector[Int] = {
    if(Config.executorType == "gpu")
      run[Vector[Int]](OPGPU_vmult_single(this, v.asInstanceOf[IntVector]))
    else{
      if (Config.useNativeLibs){
        run(OP_native_vmult_single(this, v.asInstanceOf[IntVector]))
      }
      else{
        //run(OP_vmult_single(this, v.asInstanceOf[IntVector]))
        run(OP_vmult(this, v.asInstanceOf[IntVector]))
      }
    }
  }

  override def *(d: Int)(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Matrix[Int] = {
    //run(OP_smult_single(this, d))
    if(Config.executorType == "gpu")
      run(OPGPU_smult_single(this, d))
    else
      run(OP_mapInt(this, run(OP_alloc[Int](this)).asInstanceOf[IntMatrix], e => e*d))
  }

  override def *=(d: Int)(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Matrix[Int] = {
    //run(OP_smult_single_mutable(this, d))
    run(OP_mapInt(this, this, e => e*d))    
  }

  override def dot(v: Vector[Int])(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Matrix[Int] = {
    if(Config.executorType == "gpu")
      run(OPGPU_vdot_single(this,v.asInstanceOf[IntVector]))
    else
      run(OP_vdot_single(this,v.asInstanceOf[IntVector]))
  }

  override def trans(implicit c: ClassManifest[Int]) : Matrix[Int] = {
    if(Config.executorType == "gpu")
      run(OPGPU_trans(this))
    else
      run(OP_trans(this))
  }

  // others
  
  override def +(d: Int)(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Matrix[Int] = {
    if(Config.executorType == "gpu")
      run(OPGPU_IntPlus(this, d))
    else
      run(OP_mapInt(this, run(OP_alloc[Int](this)).asInstanceOf[IntMatrix], e => e+d))
  }

  override def -(m: Matrix[Int])(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Matrix[Int] = {
    if(Config.executorType == "gpu")
      run(OPGPU_-(this, m.asInstanceOf[IntMatrix]));
    else{
      //run(OP_minus_single(this,m.asInstanceOf[IntMatrix]))
      run(OP_-(this, m.asInstanceOf[IntMatrix], run(OP_alloc[Int](this)).asInstanceOf[IntMatrix]));
    }
  }

  override def -(d: Int)(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Matrix[Int] = {
    run(OP_mapInt(this, run(OP_alloc[Int](this)).asInstanceOf[IntMatrix], e => e-d))
  }

  override def /(m: Matrix[Int])(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Matrix[Int] = {
    run(OP_/(this, m.asInstanceOf[IntMatrix], run(OP_alloc[Int](this)).asInstanceOf[IntMatrix]))
  }
  override def /(d: Int)(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Matrix[Int] = {
      run(OP_mapInt(this, run(OP_alloc[Int](this)).asInstanceOf[IntMatrix], e => e/d))
  }

  override def dot(m: Matrix[Int])(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Matrix[Int] = {
    if(Config.executorType == "gpu")
      run(OPGPU_mdot(this, m.asInstanceOf[IntMatrix]))
    else
      run(OP_mdot(this, m.asInstanceOf[IntMatrix], run(OP_alloc[Int](this)).asInstanceOf[IntMatrix]))
  }

  override def unary_-(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Matrix[Int] = {
    run(OP_unary_-(this, run(OP_alloc[Int](this)).asInstanceOf[IntMatrix]))
  }

  override def abs(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Matrix[Int] = {
    run(OP_abs(this, run(OP_alloc[Int](this)).asInstanceOf[IntMatrix]))
  }

  override def exp(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Matrix[Int] = {
    if(Config.executorType == "gpu")
      run(OPGPU_exp(this))
    else {
		
      		run(OP_exp(this, run(OP_alloc[Int](this)).asInstanceOf[IntMatrix]))
	}
  }

  override def >(m: Matrix[Int])(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Matrix[Int] = {
    if(Config.executorType == "gpu")
      run(OPGPU_>(this, m.asInstanceOf[IntMatrix]))
    else
      run(OP_>(this, m.asInstanceOf[IntMatrix], run(OP_alloc[Int](this)).asInstanceOf[IntMatrix]))
  }

  override def >(d: Int)(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Matrix[Int] = {
    run(OP_mapInt(this, run(OP_alloc[Int](this)).asInstanceOf[IntMatrix], e => if (e > d) 1 else 0))
  }

  override def <(m: Matrix[Int])(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Matrix[Int] = {
    run(OP_<(this, m.asInstanceOf[IntMatrix], run(OP_alloc[Int](this)).asInstanceOf[IntMatrix]))
  }
  override def <(d: Int)(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Matrix[Int] = {
    run(OP_mapInt(this, run(OP_alloc[Int](this)).asInstanceOf[IntMatrix], e => if (e < d) 1 else 0))
  }

  override def sum[B <: DeliteDSLType](implicit ops: ArithOps[Int], conv: Int => B, pfact: DeliteProxyFactory[B], c: ClassManifest[Int]): B = {
    run(OP_sum[B](this))
  }

  override def min[B <: DeliteDSLType](implicit cmp: Int => Ordered[Int], conv: Int => B, pfact: DeliteProxyFactory[B]) : B = {
    run(OP_min[B](this))
  }

  override def max[B <: DeliteDSLType](implicit cmp: Int => Ordered[Int], conv: Int => B, pfact: DeliteProxyFactory[B]) : B = {
    run(OP_max[B](this))
  }

  ///////////////////
  // bulk operations

  override def map[B](f: Int => B)(implicit pFact: Matrix.ProxyFactory[B], c: ClassManifest[B]) : Matrix[B] =  {
    run[Matrix[B]](OP_map(this, run(OP_alloc[B](this)), f))
  }

  ////////////////////
  // RBM oeprations
  
  // RBM : SUM operation on Matrix that adds up each column (returns a row vector)
  override def sumCol: Vector[Int] = {
    if(Config.executorType == "gpu")
      run(OPGPU_sumCol(this))
    else
      run(OP_sumCol(this))
  }

  // RBM : operation that replicates the given matrix to generate a larger matrix
  override def repmat(i: Int, j: Int): Matrix[Int] = {
    run(OP_repmat(this, i, j))
  }

  // RBM : reciprocal of each elemnts (1/elms)
  // This will be replaced with map operation
  override def reciprocal: Matrix[Int] = {
    if(Config.executorType == "gpu")
      run(OPGPU_recip(this))
    else
      run(OP_recip(this))
  }

  override def sigmoid: Matrix[Int] = {
	
    	run(OP_mapInt(this, run(OP_alloc[Int](this)).asInstanceOf[IntMatrix], e => (1.0/(1.0+math.exp(-e))).asInstanceOf[Int]))
    //run(OP_sigmoid(this))
  }
  
}
