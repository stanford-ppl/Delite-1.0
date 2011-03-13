/* Specialized operations for LongVectors.
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  May 5, 2010
 * modified: May 5, 2010
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
package ppl.delite.dsl.optiml.specialized

import ppl.delite.dsl.optiml.{ArithOps, Vector, Matrix}
import ppl.delite.core.appinclude._
import ppl.delite.core.{Config, DeliteDSLType, DeliteProxyFactory, DeliteUnit}
import ppl.delite.cuda._
import ppl.delite.dsl.primitive.{DeliteInt, DeliteLong}
import ppl.delite.core.ops.{DeliteOP_ZipWith2, DeliteOP_Reduce, DeliteOP_Map, DeliteOP_SingleTask}
import ppl.delite.core.ops.specialized._
import ppl.delite.cnative._

object LongVector {

  protected[optiml] case class OP_alloc[B](orig: LongVector)(implicit c: ClassManifest[B]) extends DeliteOP_SingleTask[Vector[B]](orig) {
    def task = {
      Vector[B](orig.is_row, orig.length)
    }
  }
  
  protected[optiml] case class OP_apply(val collA: LongVector, n: Int)
    extends DeliteOP_SingleTask[DeliteLong](collA){

    def task = {
      collA(n)
    }
  }

  protected[optiml] case class OP_update(val collA: LongVector, index: Int, x: Long)
    extends DeliteOP_SingleTask[DeliteUnit](collA){

    def task = {
      collA(index) = x
    }
  }

  protected[optiml] case class OP_trans(val collA: LongVector)
    extends DeliteOP_SingleTask[Vector[Long]](collA) {

    def task = {
      val out = new LongVectorImpl(!collA._is_row, collA._length)
      var i = 0
      while (i < out._length){
        out(i) = collA(i)
        i += 1
      }
      out
    }
  }

  /**
   * Single task versions of arithmetic ops
   */

  protected[optiml] case class OP_plus_single(val collA: LongVector, val collB: LongVector)
    extends DeliteOP_SingleTask[Vector[Long]](collA, collB) {

    def task = {
      val out = new LongVectorImpl(collA._is_row, collA._length)
      var i = 0
      while (i < out._length){
        out(i) = collA(i)+collB(i)
        i += 1
      }
      out
    } 
  }

  protected[optiml] case class OP_outer_single(val collA: LongVector, val collB: LongVector)
    extends DeliteOP_SingleTask[Matrix[Long]](collA,collB) {

    def task = {
      if (collA._length != collB._length || collA._is_row == collB._is_row || collA._is_row) throw new IndexOutOfBoundsException()
      val out = new LongMatrixImpl(collA._length, collA._length)
      var i = 0
      var j = 0
      while( i < collA._length ){
        while (j < collB._length) {
          out(i,j) = collA(i)*collB(j)
          j += 1
        }
        j = 0
        i += 1
      }
      //Matrix.fromMap[Long,Long](v1, (elem => v2*elem))
      out
    }
  }

  protected[optiml] case class OP_minus_single(val collA: LongVector, val collB: LongVector)
    extends DeliteOP_SingleTask[Vector[Long]](collA, collB) {

    def task = {
      val out = new LongVectorImpl(collA._is_row, collA._length)
      var i = 0
      while (i < out._length){
        out(i) = collA(i)-collB(i)
        i += 1
      }
      out
    }
  }

  protected[optiml] case class OP_dot_single[B <: DeliteDSLType](collA: LongVector, collB: LongVector, conv: Long => B)
    extends DeliteOP_SingleTask[B](collA,collB) {

    def task = {
      collA.chkLength(collA,collB)
      var acc : Long = 0
      var i = 0
      while (i != collA._length) {
        acc += collA(i) * collB(i)
        i += 1
      }
      conv(acc)
    }
  }

  /**
   * arithmetic ops
   */

  protected[optiml] case class OP_+(val collA: LongVector, val collB: LongVector, val out: LongVector)
    extends DeliteOP_ZipWith2Spec[Long,Vector] {
    override val associative = true

    def func = (a,b) => a+b

  }

  protected[optiml] case class OP_-(val collA: LongVector, val collB: LongVector, val out: LongVector)
    extends DeliteOP_ZipWith2Spec[Long,Vector] {

    def func = (a,b) => a-b
  }

  protected[optiml] case class OP_/(val collA: LongVector, val collB: LongVector, val out: LongVector)
    extends DeliteOP_ZipWith2Spec[Long,Vector] {

    def func = (a,b) => a/b
  }

  protected[optiml] case class OP_mtimes(v: LongVector, m: LongMatrix)
    extends DeliteOP_SingleTask[Vector[Long]](v,m) {

    def task = {
      v.chkVecMatAgree(v, m)
      val v_trans = v.trans
      m.trans.mapRowsToVec(a_row => a_row._dot(v_trans))
    }
  }

  protected[optiml] case class OP_*(val collA: LongVector, val collB: LongVector, val out: LongVector)
    extends DeliteOP_ZipWith2Spec[Long,Vector] {

    def func = (a,b) => a*b
  }

  protected[optiml] case class OP_native_*(val v1: LongVector, val v2: LongVector)
    extends DeliteOP_SingleTask[Vector[Long]](v1, v2) {
		
      def task = {
        val out = new LongVectorImpl(v1._is_row, v1._length)
        throw new UnsupportedOperationException
//DeliteNative.vectMultLong(v1._data, v2._data, out._data, v1._length)
        out
      }
  }

  protected[optiml] case class OP_outer(v1: LongVector, v2: LongVector)
    extends DeliteOP_SingleTask[Matrix[Long]](v1,v2) {

    def task = {
      if (v1.length != v2.length || v1.is_row == v2.is_row || v1.is_row) v1.vecDimError()
      Matrix.fromMap[Long,Long](v1, (a_row => v2*a_row))
    }
  }

  protected[optiml] case class OP_dot[B <: DeliteDSLType](v1: LongVector, v2: LongVector)
    (implicit conv: Long => B) extends DeliteOP_SingleTask[B](v1, v2) {

    def task = {
      v1.chkLength(v1, v2)
      var acc : Long = v1(0)*v2(0)
      for (i <- 1 until v1.length){
        acc = acc + (v1(i)*v2(i))
      }
      conv(acc)
    }
  }


  protected[optiml] case class OP_sum[B <: DeliteDSLType](val coll: LongVector)
    (implicit conv: Long => B) extends DeliteOP_ReduceSpec[Long,B] {

    def func = (a,b) => a+b
  }

  protected[optiml] case class OP_abs(val coll: LongVector, val out: LongVector)
    extends DeliteOP_MapSpec[Long,Vector]{

    def func = e => longArithOps.abs(e)
  }

  protected[optiml] case class OP_exp(val coll: LongVector, val out: LongVector)
    extends DeliteOP_Map[Long,Long,Vector]{

    def func = e => longArithOps.exp(e)
  }


  /////////////////
  // ordering ops

  protected[optiml] case class OP_min[B <: DeliteDSLType](val coll: LongVector)(implicit conv: Long => B)
    extends DeliteOP_ReduceSpec[Long,B] {

    def func = (a,b) => if (a < b) a else b
  }

  protected[optiml] case class OP_minIndex(v: LongVector)
    extends DeliteOP_SingleTask[DeliteInt](v) {

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

  protected[optiml] case class OP_max[B <: DeliteDSLType](val coll: LongVector)(implicit conv: Long => B)
    extends DeliteOP_ReduceSpec[Long,B] {

    def func = (a,b) => if (a > b) a else b
  }

  /////////////
  // bulk ops

  protected[optiml] case class OP_reduce[B <: DeliteDSLType](val coll: LongVector, val func: (Long,Long) => Long)
    (implicit conv: Long => B) extends DeliteOP_ReduceSpec[Long,B]

  protected[optiml] case class OP_map[B : ClassManifest](val coll: LongVector, val out: Vector[B], val func: Long => B)
    extends DeliteOP_Map[Long,B,Vector]

  protected[optiml] case class OP_mapLong(val coll: LongVector, val out: LongVector, val func: Long => Long)
    extends DeliteOP_MapSpec[Long,Vector]


  /////////////////////////////////////////////////////////////////////////////////////
  ///////////////////            GPU OPS        ///////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////
  
  protected[optiml] case class OPGPU_trans(val collA: LongVector)
    extends DeliteOP_SingleTask[Vector[Long]](collA) {

    def task = {
      val out = new LongVectorImpl(!collA._is_row, collA._length)
      var i = 0
      while (i < out._length){
        out(i) = collA(i)
        i += 1
      }
      out
    }

	  /*override def getGPUInputs = List(collA)
    override def getGPUConsts = List(collA._length)
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Vector[Long](elms, !collA._is_row, collA._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._length)
    *///override def getGPUKernelId = List(DeliteCuda.DVectMoveLong, DeliteCuda.AsyncLaunch1D)
    //*///override def getGPUKernelId = {List(DeliteCuda.DVectMoveLong, DeliteCuda.Async1I1D)}
  }

  protected[optiml] case class OPGPU_plus_single(val collA: LongVector, val collB: LongVector)
    extends DeliteOP_SingleTask[Vector[Long]](collA, collB) {

    def task = {
      val out = new LongVectorImpl(collA._is_row, collA._length)
      var i = 0
      while (i < out._length){
        out(i) = collA(i)+collB(i)
        i += 1
      }
      out
    }

    /*override def getGPUInputs = List(collA, collB)
    override def getGPUConsts = List(collA._length)
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Vector[Long](elms, collA._is_row, collA._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._length)
	  *///override def getGPUKernelId = List(DeliteCuda.DVectPlusLong, DeliteCuda.AsyncLaunch1D)
    //*///override def getGPUKernelId = {List(DeliteCuda.DVectPlusLong, DeliteCuda.Async2I1D)}
  }

  protected[optiml] case class OPGPU_outer_single(val collA: LongVector, val collB: LongVector)
    extends DeliteOP_SingleTask[Matrix[Long]](collA,collB) {

    def task = {
      if (collA._length != collB._length || collA._is_row == collB._is_row || collA._is_row) throw new IndexOutOfBoundsException()
      val out = new LongMatrixImpl(collA._length, collA._length)
      var i = 0
      var j = 0
      while( i < collA._length ){
        while (j < collB._length) {
          out(i,j) = collA(i)*collB(j)
          j += 1
        }
        j = 0
        i += 1
      }
      out
    }

    /*override def getGPUInputs = List(collA, collB)
    override def getGPUConsts = List(collA._length)
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Matrix[Long](elms, collA._length, collA._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._length, collA._length/16, 16, 1)
	  *///override def getGPUKernelId = List(DeliteCuda.DVectOuterLong, DeliteCuda.AsyncLaunch2D)
    //*///override def getGPUKernelId = {List(DeliteCuda.DVectOuterLong, DeliteCuda.Async2I2D)}
  }

  protected[optiml] case class OPGPU_minus_single(val collA: LongVector, val collB: LongVector)
    extends DeliteOP_SingleTask[Vector[Long]](collA, collB) {

    def task = {
      val out = new LongVectorImpl(collA._is_row, collA._length)
      var i = 0
      while (i < out._length){
        out(i) = collA(i)-collB(i)
        i += 1
      }
      out
    }

    /*override def getGPUInputs = List(collA, collB)
    override def getGPUConsts = List(collA._length)
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Vector[Long](elms, collA._is_row, collA._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._length)
    *///override def getGPUKernelId = {List(DeliteCuda.DVectMinusLong, DeliteCuda.AsyncLaunch1D)}
	  //*///override def getGPUKernelId = {List(DeliteCuda.DVectMinusLong, DeliteCuda.Async2I1D)}
  }

  protected[optiml] case class OPGPU_sum[B <: DeliteDSLType](val coll: LongVector)
    (implicit conv: Long => B) extends DeliteOP_ReduceSpec[Long,B] {

    def func = (a,b) => a+b
    /*
    /*override def getGPUInputs = List(coll)
    override def getGPUConsts = List(coll.size)
    override def getGPUOutput = {
      //val elms = new Array[Long](0)
      //val gout = Vector[Long](elms, true, 1)
      val gout = DeliteLong(0.0)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
	  *///override def getGPUKernelId = List(DeliteCuda.sumLong, DeliteCuda.Async1I1D)
	  */
  }

  protected[optiml] case class OPGPU_*(val collA: LongVector, val collB: LongVector)
    extends DeliteOP_SingleTask[Vector[Long]](collA, collB) {

    def task = {
      val out = new LongVectorImpl(collA._is_row, collB._length)
      var i = 0
      while (i < out._length){
        out(i) = collA(i) * collB(i)
        i += 1
      }
      out
    }

    /*override def getGPUInputs = List(collA, collB)
    override def getGPUConsts = List(collA._length)
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Vector[Long](elms, collA._is_row, collA._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._length)
    *///override def getGPUKernelId = List(DeliteCuda.DVectMultLong, DeliteCuda.AsyncLaunch1D)
	  //*///override def getGPUKernelId = {List(DeliteCuda.DVectMultLong, DeliteCuda.Async2I1D)}
  }


  protected[optiml] case class OPGPU_LongMult(val coll: LongVector, val x: Long)
    extends DeliteOP_SingleTask[Vector[Long]](coll) {

    def task = {
      val out = new LongVectorImpl(coll._is_row, coll._length)
      var i = 0
      while (i < out._length){
        out(i) = coll(i) * x
        i += 1
      }
      out
    }

    /*override def getGPUInputs = List(coll)
    override def getGPUConsts = List(x, coll._length)
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Vector[Long](elms, coll._is_row, coll._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(coll._length)
    *///override def getGPUKernelId = List(DeliteCuda.VectMultLong_S, DeliteCuda.AsyncLaunch1D)
	  //*///override def getGPUKernelId = List(DeliteCuda.VectMultLong_S, DeliteCuda.AsyncRBM_1I1S)
  }
  
  protected[optiml] case class OPGPU_LongPlus(val coll: LongVector, val x: Long)
    extends DeliteOP_SingleTask[Vector[Long]](coll) {

    def task = {
      val out = new LongVectorImpl(coll._is_row, coll._length)
      var i = 0
      while (i < out._length){
        out(i) = coll(i) + x
        i += 1
      }
      out
    }

    /*override def getGPUInputs = List(coll)
    override def getGPUConsts = List(x, coll._length)
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Vector[Long](elms, coll.is_row, coll.length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(coll._length)
    *///override def getGPUKernelId = List(DeliteCuda.VectPlusLong_S, DeliteCuda.AsyncLaunch1D)
	  //*///override def getGPUKernelId = List(DeliteCuda.VectPlusLong_S, DeliteCuda.AsyncRBM_1I1S)
  }

  protected[optiml] case class OPGPU_LongDiv(val coll: LongVector, val x: Long)
    extends DeliteOP_SingleTask[Vector[Long]](coll) {

    def task = {
      val out = new LongVectorImpl(coll._is_row, coll._length)
      var i = 0
      while (i < out._length){
        out(i) = coll(i) / x
        i += 1
      }
      out
    }

    /*override def getGPUInputs = List(coll)
    override def getGPUConsts = List(x, coll._length)
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Vector[Long](elms, coll._is_row, coll._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(coll._length)
    *///override def getGPUKernelId = List(DeliteCuda.VectDivLong_S, DeliteCuda.AsyncLaunch1D)
	 // *///override def getGPUKernelId = List(DeliteCuda.VectDivLong_S, DeliteCuda.AsyncRBM_1I1S)
  }

  protected[optiml] case class OPGPU_exp(val coll: LongVector)
    extends DeliteOP_SingleTask[Vector[Long]](coll) {

      def task = {
        val out = new LongVectorImpl(coll._is_row, coll._length)
        var i = 0
        while (i < out._length){
          out(i) = longArithOps.exp(coll(i))
          i += 1
        }
        out
      }

      /*override def getGPUInputs = List(coll)
      override def getGPUConsts = List(coll._length)
      override def getGPUOutput = {
        val elms = new Array[Long](0)
        val gout = Vector[Long](elms, coll._is_row, coll._length)
        gout.cvalue = gout.asInstanceOf[gout.DSLType]
        gout.isComputed = true
        gout
      }
      override def getGPUKernelDims = List(coll._length)
      *///override def getGPUKernelId = List(DeliteCuda.VectExpLong, DeliteCuda.AsyncLaunch1D)
      //*///override def getGPUKernelId = List(DeliteCuda.VectExpLong, DeliteCuda.AsyncRBM_1I)
  }

  case class OPGPU_REPMAT(v: LongVector, iRep: Int, jRep: Int) extends DeliteOP_SingleTask[Matrix[Long]](v) {
    def task = {
	  	val length = v.length
		  var i = 0
      var j = 0
		  var ii = 0
		  var jj = 0

		  if(v.is_row) { // when input is a row vector
        val out = new LongMatrixImpl(iRep, jRep*v.length)
			  ii = 0
        while (ii < iRep) {
          jj = 0
          while (jj < jRep) {
            j = 0
            while (j < length) {
              out(ii,jj*length+j) = v(j)
              j += 1
            }
            jj += 1
          }
          ii += 1
        }
        out
      }
      else {  // when input is a column vector
        val out = new LongMatrixImpl(iRep*v.length, jRep)
        ii = 0
        while (ii < iRep) {
          jj = 0
          while (jj < jRep) {
            i = 0
            while (i < length) {
              out(ii*length+i, jj) = v(i)
              i += 1
            }
            jj += 1
          }
          ii += 1
        }
        out
      }
	  }

    /*override def getGPUInputs = List(v)
    override def getGPUConsts = List(v._length, iRep, jRep)
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Matrix[Long](elms, iRep, v._length*jRep)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(v._length*jRep, iRep, 512, 1)
	  *///override def getGPUKernelId = List(DeliteCuda.VectRepLong, DeliteCuda.AsyncLaunch2D)
    //*///override def getGPUKernelId = List(DeliteCuda.VectRepLong, DeliteCuda.AsyncRBM_Repmat)
  }

  /////////////////////////////////////////////////////////////////////////////////////
  ///////////////////         END OF GPU OPS        ///////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////

  
  /////////////////////
  //  RBM functions

  // TODO: If (iRep==1 for row Vector) or (j==1 for col Vector), does the result have to be a Vector instead of a Matrix?
	/* REPMAT1D : Replication of the input vector(jRep times) to generate larger matrix */
	case class OP_REPMAT(v: LongVector, iRep: Int, jRep: Int) extends DeliteOP_SingleTask[Matrix[Long]](v) {
    def task = {
	  	val length = v.length
		  var i = 0
      var j = 0
		  var ii = 0
		  var jj = 0
      
		  if(v.is_row) { // when input is a row vector
        val out = new LongMatrixImpl(iRep, jRep*v.length)
			  ii = 0
        while (ii < iRep) {
          jj = 0
          while (jj < jRep) {
            j = 0
            while (j < length) {
              out(ii,jj*length+j) = v(j)
              j += 1
            }
            jj += 1
          }
          ii += 1
        }
        out
      }
      else {  // when input is a column vector
        val out = new LongMatrixImpl(iRep*v.length, jRep)
        ii = 0
        while (ii < iRep) {
          jj = 0
          while (jj < jRep) {
            i = 0
            while (i < length) {
              out(ii*length+i, jj) = v(i)
              i += 1
            }
            jj += 1
          }
          ii += 1
        }
        out
      }
	  }   
  }

  /*
  protected[delite] case class OP_mapLR[B : ClassManifest](val v: LongVector, val func: Long => B, val x_cur:Long, val tau:Int)
    extends DeliteOP_SingleTask[Vector[B]](v) {

    def task = {
      val length = v.length
      val out = Vector[B](v.is_row, v.length)
      var i = 0
      while (i < length){
        out(i) = func(v(i))
        i += 1
      }
      out
    }

    /*override def getGPUInputs = {List(v)}
    override def getGPUOutput = {
      val elms = new Array[Long](0)
      val gout = Vector[Long](elms, v.is_row, v.length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
	  *///override def getGPUKernelId = {List(DeliteCuda.MAP_LR, DeliteCuda.AsyncMAPLR)}
  }
  */

  protected[optiml] case class OP_sort(val coll: LongVector)
    extends DeliteOP_SingleTask[Vector[Long]](coll) {

    def task = {
      // in scala.util.Sorting.quickSort, coll._data.length is used
      if(coll._length != coll._data.length){
        val d = new Array[Long](coll._length)
        Array.copy(coll._data, 0, d, 0, coll._length)
        coll._data = d
      }
      scala.util.Sorting.quickSort(coll._data)
      coll
    }

  }

}


trait LongVector extends Vector[Long] {
  import LongVector._

  protected[optiml] var _data: Array[Long]

  def apply(n: Int) : Long
  override def lifted_apply(n: Int) : Long  = {
    run(OP_apply(this,n))
  }


  def update(index: Int, x: Long) : Unit
  override def lifted_update(index: Int, x: Long) : Unit = {
    run(OP_update(this,index,x))
  }

  override def dc_update(i: Int, x: Long)
  override def dc_apply(i: Int) : Long 

  //////////
  // arith

  // gda, kmeans, nb ops
  override def trans(implicit c: ClassManifest[Long]) : Vector[Long] = {
    if(Config.executorType == "gpu")
      run(OPGPU_trans(this))
    else
      run(OP_trans(this))
  }

  override def +(v: Vector[Long])(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Vector[Long] = {
    //run(OP_+(this, v.asInstanceOf[LongVector], run(OP_alloc[Long](this)).asInstanceOf[LongVector]))
    if(Config.executorType == "gpu")
      run(OPGPU_plus_single(this,v.asInstanceOf[LongVector]))
    else
      run(OP_+(this, v.asInstanceOf[LongVector], run(OP_alloc[Long](this)).asInstanceOf[LongVector]))
      //run(OP_plus_single(this,v.asInstanceOf[LongVector]))
  }

  override def outer(v: Vector[Long])(implicit ops: ArithOps[Long], pFact: Matrix.ProxyFactory[Long], c: ClassManifest[Long]) : Matrix[Long] = {
    if(Config.executorType == "gpu")
      run(OPGPU_outer_single(this,v.asInstanceOf[LongVector]))
    else
      run(OP_outer_single(this,v.asInstanceOf[LongVector]))
  }

  override def -(v: Vector[Long])(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Vector[Long] = {
    //run(OP_-(this, v.asInstanceOf[LongVector], run(OP_alloc[Long](this)).asInstanceOf[LongVector]))
    if(Config.executorType == "gpu")
      run(OPGPU_minus_single(this,v.asInstanceOf[LongVector]))
    else
     run(OP_-(this, v.asInstanceOf[LongVector], run(OP_alloc[Long](this)).asInstanceOf[LongVector]))
     //run(OP_minus_single(this,v.asInstanceOf[LongVector]))
  }

  override def dot[B <: DeliteDSLType](v: Vector[Long])(implicit ops: ArithOps[Long], conv: Long => B, pfact: DeliteProxyFactory[B]) : B = {
    run(OP_dot_single(this, v.asInstanceOf[LongVector], conv))(pfact)
  }

  // others
  override def +(x: Long)(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Vector[Long] = {
    if(Config.executorType == "gpu")
      run(OPGPU_LongPlus(this, x))
    else
      run(OP_mapLong(this, run(OP_alloc[Long](this)).asInstanceOf[LongVector], ele => ele+x))
  }

  override def -(x: Long)(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Vector[Long] = {
    run(OP_mapLong(this, run(OP_alloc[Long](this)).asInstanceOf[LongVector], ele => ele-x))
  }

  override def /(v: Vector[Long])(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Vector[Long] = {
    run(OP_/(this, v.asInstanceOf[LongVector], run(OP_alloc[Long](this)).asInstanceOf[LongVector]))
  }
  override def /(x: Long)(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Vector[Long] = {
    if(Config.executorType == "gpu")
      run(OPGPU_LongDiv(this, x))
    else
      run(OP_mapLong(this, run(OP_alloc[Long](this)).asInstanceOf[LongVector], ele => ele/x))
  }

  override def *(m: Matrix[Long])(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Vector[Long] = {
    run(OP_mtimes(this,m.asInstanceOf[LongMatrix]))
  }

  override def *(v: Vector[Long])(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Vector[Long] = {
    if(Config.executorType == "gpu")
      run(OPGPU_*(this, v.asInstanceOf[LongVector]))
    else {
      //if (Config.useNativeLibs) 
      //  run(OP_native_*(this, v.asInstanceOf[LongVector]))
      //else 
        run(OP_*(this, v.asInstanceOf[LongVector], run(OP_alloc[Long](this)).asInstanceOf[LongVector]))
    }
  }

  override def *(x: Long)(implicit ops: ArithOps[Long], c: ClassManifest[Long]) : Vector[Long] = {
    if(Config.executorType == "gpu")
      run(OPGPU_LongMult(this, x))
    else
      run(OP_mapLong(this, run(OP_alloc[Long](this)).asInstanceOf[LongVector], ele => ele*x))
  }

  override def sum[B <: DeliteDSLType](implicit pfact: DeliteProxyFactory[B], ops: ArithOps[Long], conv: Long => B): B = {
    if(Config.executorType == "gpu")
      run(OPGPU_sum(this)(conv))
    else
      run(OP_sum(this)(conv))
  }

  override def abs(implicit ops: ArithOps[Long], pfact: DeliteProxyFactory[Vector[Long]], c: ClassManifest[Long]) : Vector[Long] = {
    run(OP_abs(this, run(OP_alloc[Long](this))(pfact).asInstanceOf[LongVector]))(pfact)
  }

  override def exp(implicit ops: ArithOps[Long], pfact: DeliteProxyFactory[Vector[Long]], c: ClassManifest[Long]) : Vector[Long] = {
    if(Config.executorType == "gpu")
      run(OPGPU_exp(this))(pfact)
    else
      run(OP_exp(this, run(OP_alloc[Long](this))(pfact).asInstanceOf[LongVector]))(pfact)
  }

  
  /////////////////////////////
  // stuff requiring ordering

  override def min[B <: DeliteDSLType](implicit cmp: Long => Ordered[Long], conv: Long => B, pfact: DeliteProxyFactory[B]): B = {
    run(OP_min(this))
  }

  override def minIndex(implicit cmp: Long => Ordered[Long]): DeliteInt = {
    run(OP_minIndex(this))
  }

  override def max[B <: DeliteDSLType](implicit cmp: Long => Ordered[Long], conv: Long => B, pfact: DeliteProxyFactory[B]): B = {
    run(OP_max(this))
  }

  //////////////////
  // bulk operations

  override def map[B](f: Long => B)(implicit pFact: DeliteProxyFactory[Vector[B]], c: ClassManifest[B]) : Vector[B] = {
    run[Vector[B]](OP_map[B](this, run(OP_alloc[B](this)), f))
  }

  override def reduce[B <: DeliteDSLType](func: (Long,Long) => Long)(implicit conv: Long => B, pfact: DeliteProxyFactory[B]): B = {
    run(OP_reduce(this, func)(conv))
  }


  ///////////////////////////
  // GPU Specific Operations

  // Map operation for Linear Regression
  /*
  override def mapLR[B](f: Long => B)(x_cur:Long, tau:Int)(implicit pFact: DeliteProxyFactory[Vector[B]], c: ClassManifest[B]) : Vector[B] = {
    if(Config.executorType == "gpu")
      run[Vector[B]](OP_mapLR[B](this, f, x_cur, tau))
    else
      run[Vector[B]](OP_map[B](this, run(OP_alloc[B](this)), f))
  }
  */
  
  ////////////////////
  // RBM oeprations

  // RBM : operation that replicates the given matrix to generate a larger matrix
  override def repmat(i: Int, j: Int): Matrix[Long] = {
    if(Config.executorType == "gpu")
      run(OPGPU_REPMAT(this, i, j))
    else
      run(OP_REPMAT(this, i, j))
  }

  override def sort(implicit cmp: Long => Ordered[Long], pfact: DeliteProxyFactory[Vector[Long]]) : Vector[Long] = {
    run(OP_sort(this))(pfact)
  }

}
