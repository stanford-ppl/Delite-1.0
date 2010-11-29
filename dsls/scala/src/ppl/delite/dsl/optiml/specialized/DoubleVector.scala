/* Specialized operations for DoubleVectors.
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
import ppl.delite.dsl.primitive.{DeliteInt, DeliteDouble}
import ppl.delite.core.ops.specialized._
import ppl.delite.cnative._
import ppl.delite.core.ops._

object DoubleVector {

  protected[optiml] case class OP_alloc[B](orig: DoubleVector)(implicit c: ClassManifest[B]) extends DeliteOP_SingleTask[Vector[B]](orig) {
    def task = {
      Vector[B](orig.is_row, orig.length)
    }
  }
  
  protected[optiml] case class OP_apply(val collA: DoubleVector, n: Int)
    extends DeliteOP_SingleTask[DeliteDouble](collA){

    def task = {
      collA(n)
    }
  }

  protected[optiml] case class OP_update(val collA: DoubleVector, index: Int, x: Double)
    extends DeliteOP_SingleTask[DeliteUnit](collA){

    def task = {
      collA(index) = x
    }
  }

  protected[optiml] case class OP_trans(val collA: DoubleVector)
    extends DeliteOP_SingleTask[Vector[Double]](collA) {

    def task = {
      val out = new DoubleVectorImpl(!collA._is_row, collA._length)
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

  protected[optiml] case class OP_plus_single(val collA: DoubleVector, val collB: DoubleVector)
    extends DeliteOP_SingleTask[Vector[Double]](collA, collB) {

    def task = {
      val out = new DoubleVectorImpl(collA._is_row, collA._length)
      var i = 0
      while (i < out._length){
        out(i) = collA(i)+collB(i)
        i += 1
      }
      out
    } 
  }

  protected[optiml] case class OP_outer_single(val collA: DoubleVector, val collB: DoubleVector)
    extends DeliteOP_SingleTask[Matrix[Double]](collA,collB) {

    def task = {
      if (collA._length != collB._length || collA._is_row == collB._is_row || collA._is_row) throw new IndexOutOfBoundsException()
      val out = new DoubleMatrixImpl(collA._length, collA._length)
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
      //Matrix.fromMap[Double,Double](v1, (elem => v2*elem))
      out
    }
  }

  protected[optiml] case class OP_minus_single(val collA: DoubleVector, val collB: DoubleVector)
    extends DeliteOP_SingleTask[Vector[Double]](collA, collB) {

    def task = {
      val out = new DoubleVectorImpl(collA._is_row, collA._length)
      var i = 0
      while (i < out._length){
        out(i) = collA(i)-collB(i)
        i += 1
      }
      out
    }
  }

  protected[optiml] case class OP_dot_single[B <: DeliteDSLType](collA: DoubleVector, collB: DoubleVector, conv: Double => B)
    extends DeliteOP_SingleTask[B](collA,collB) {

    def task = {
      collA.chkLength(collA,collB)
      var acc : Double = 0
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

  protected[optiml] case class OP_+(val collA: DoubleVector, val collB: DoubleVector, val out: DoubleVector)
    extends DeliteOP_ZipWith2Spec[Double,Vector] {
    override val associative = true

    def func = (a,b) => a+b

  }

  protected[optiml] case class OP_-(val collA: DoubleVector, val collB: DoubleVector, val out: DoubleVector)
    extends DeliteOP_ZipWith2Spec[Double,Vector] {

    def func = (a,b) => a-b
  }

  protected[optiml] case class OP_/(val collA: DoubleVector, val collB: DoubleVector, val out: DoubleVector)
    extends DeliteOP_ZipWith2Spec[Double,Vector] {

    def func = (a,b) => a/b
  }

  protected[optiml] case class OP_mtimes(v: DoubleVector, m: DoubleMatrix)
    extends DeliteOP_SingleTask[Vector[Double]](v,m) {

    def task = {
      v.chkVecMatAgree(v, m)
      val v_trans = v.trans
      m.trans.mapRowsToVec(a_row => a_row._dot(v_trans))
    }
  }

  protected[optiml] case class OP_*(val collA: DoubleVector, val collB: DoubleVector, val out: DoubleVector)
    extends DeliteOP_ZipWith2Spec[Double,Vector] {

    def func = (a,b) => a*b
  }

  protected[optiml] case class OP_native_*(val v1: DoubleVector, val v2: DoubleVector)
    extends DeliteOP_SingleTask[Vector[Double]](v1, v2) {
		
      def task = {
        val out = new DoubleVectorImpl(v1._is_row, v1._length)
        DeliteNative.vectMultDouble(v1._data, v2._data, out._data, v1._length)
        out
      }
  }

  protected[optiml] case class OP_outer(v1: DoubleVector, v2: DoubleVector)
    extends DeliteOP_SingleTask[Matrix[Double]](v1,v2) {

    def task = {
      if (v1.length != v2.length || v1.is_row == v2.is_row || v1.is_row) v1.vecDimError()
      Matrix.fromMap[Double,Double](v1, (a_row => v2*a_row))
    }
  }

  protected[optiml] case class OP_dot[B <: DeliteDSLType](v1: DoubleVector, v2: DoubleVector)
    (implicit conv: Double => B) extends DeliteOP_SingleTask[B](v1, v2) {

    def task = {
      v1.chkLength(v1, v2)
      var acc : Double = v1(0)*v2(0)
      for (i <- 1 until v1.length){
        acc = acc + (v1(i)*v2(i))
      }
      conv(acc)
    }
  }


  protected[optiml] case class OP_sum[B <: DeliteDSLType](val coll: DoubleVector)
    (implicit conv: Double => B) extends DeliteOP_ReduceSpec[Double,B] {

    def func = (a,b) => a+b
  }

  protected[optiml] case class OP_abs(val coll: DoubleVector, val out: DoubleVector)
    extends DeliteOP_MapSpec[Double,Vector]{

    def func = e => doubleArithOps.abs(e)
  }

  protected[optiml] case class OP_exp(val coll: DoubleVector, val out: DoubleVector)
    extends DeliteOP_Map[Double,Double,Vector]{

    def func = e => doubleArithOps.exp(e)
  }


  /////////////////
  // ordering ops

  protected[optiml] case class OP_min[B <: DeliteDSLType](val coll: DoubleVector)(implicit conv: Double => B)
    extends DeliteOP_ReduceSpec[Double,B] {

    def func = (a,b) => if (a < b) a else b
  }

  protected[optiml] case class OP_minIndex(v: DoubleVector)
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

  protected[optiml] case class OP_max[B <: DeliteDSLType](val coll: DoubleVector)(implicit conv: Double => B)
    extends DeliteOP_ReduceSpec[Double,B] {

    def func = (a,b) => if (a > b) a else b
  }

  protected[optiml] case class OP_sort(val coll: DoubleVector)
    extends DeliteOP_SingleTask[Vector[Double]](coll) {

    def task = {
      scala.util.Sorting.quickSort(coll._data)
      coll
    }

  }

  /////////////
  // bulk ops

  protected[optiml] case class OP_reduce[B <: DeliteDSLType](val coll: DoubleVector, val func: (Double,Double) => Double)
    (implicit conv: Double => B) extends DeliteOP_ReduceSpec[Double,B]

  protected[optiml] case class OP_map[B : ClassManifest](val coll: DoubleVector, val out: Vector[B], val func: Double => B)
    extends DeliteOP_Map[Double,B,Vector]

  protected[optiml] case class OP_mapDouble(val coll: DoubleVector, val out: DoubleVector, val func: Double => Double)
    extends DeliteOP_MapSpec[Double,Vector]


  /////////////////////////////////////////////////////////////////////////////////////
  ///////////////////            GPU OPS        ///////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////
  
  protected[optiml] case class OPGPU_trans(val collA: DoubleVector)
    extends DeliteOP_SingleTask[Vector[Double]](collA) {

    def task = {
      val out = new DoubleVectorImpl(!collA._is_row, collA._length)
      var i = 0
      while (i < out._length){
        out(i) = collA(i)
        i += 1
      }
      out
    }

	  override def getGPUInputs = List(collA)
    override def getGPUConsts = List(collA._length)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Vector[Double](elms, !collA._is_row, collA._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._length)
    override def getGPUKernelId = List(DeliteCuda.DVectMoveDouble, DeliteCuda.AsyncLaunch1D)
    //override def getGPUKernelId = {List(DeliteCuda.DVectMoveDouble, DeliteCuda.Async1I1D)}
  }

  protected[optiml] case class OPGPU_plus_single(val collA: DoubleVector, val collB: DoubleVector)
    extends DeliteOP_SingleTask[Vector[Double]](collA, collB) {

    def task = {
      val out = new DoubleVectorImpl(collA._is_row, collA._length)
      var i = 0
      while (i < out._length){
        out(i) = collA(i)+collB(i)
        i += 1
      }
      out
    }

    override def getGPUInputs = List(collA, collB)
    override def getGPUConsts = List(collA._length)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Vector[Double](elms, collA._is_row, collA._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._length)
	  override def getGPUKernelId = List(DeliteCuda.DVectPlusDouble, DeliteCuda.AsyncLaunch1D)
    //override def getGPUKernelId = {List(DeliteCuda.DVectPlusDouble, DeliteCuda.Async2I1D)}
  }

  protected[optiml] case class OPGPU_outer_single(val collA: DoubleVector, val collB: DoubleVector)
    extends DeliteOP_SingleTask[Matrix[Double]](collA,collB) {

    def task = {
      if (collA._length != collB._length || collA._is_row == collB._is_row || collA._is_row) throw new IndexOutOfBoundsException()
      val out = new DoubleMatrixImpl(collA._length, collA._length)
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

    override def getGPUInputs = List(collA, collB)
    override def getGPUConsts = List(collA._length)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Matrix[Double](elms, collA._length, collA._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._length, collA._length/16, 16, 1)
	  override def getGPUKernelId = List(DeliteCuda.DVectOuterDouble, DeliteCuda.AsyncLaunch2D)
    //override def getGPUKernelId = {List(DeliteCuda.DVectOuterDouble, DeliteCuda.Async2I2D)}
  }

  protected[optiml] case class OPGPU_minus_single(val collA: DoubleVector, val collB: DoubleVector)
    extends DeliteOP_SingleTask[Vector[Double]](collA, collB) {

    def task = {
      val out = new DoubleVectorImpl(collA._is_row, collA._length)
      var i = 0
      while (i < out._length){
        out(i) = collA(i)-collB(i)
        i += 1
      }
      out
    }

    override def getGPUInputs = List(collA, collB)
    override def getGPUConsts = List(collA._length)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Vector[Double](elms, collA._is_row, collA._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._length)
    override def getGPUKernelId = {List(DeliteCuda.DVectMinusDouble, DeliteCuda.AsyncLaunch1D)}
	  //override def getGPUKernelId = {List(DeliteCuda.DVectMinusDouble, DeliteCuda.Async2I1D)}
  }

  protected[optiml] case class OPGPU_sum[B <: DeliteDSLType](val coll: DoubleVector)
    (implicit conv: Double => B) extends DeliteOP_ReduceSpec[Double,B] {

    def func = (a,b) => a+b
    /*
    override def getGPUInputs = List(coll)
    override def getGPUConsts = List(coll.size)
    override def getGPUOutput = {
      //val elms = new Array[Double](0)
      //val gout = Vector[Double](elms, true, 1)
      val gout = DeliteDouble(0.0)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
	  override def getGPUKernelId = List(DeliteCuda.sumDouble, DeliteCuda.Async1I1D)
	  */
  }

  protected[optiml] case class OPGPU_*(val collA: DoubleVector, val collB: DoubleVector)
    extends DeliteOP_SingleTask[Vector[Double]](collA, collB) {

    def task = {
      val out = new DoubleVectorImpl(collA._is_row, collB._length)
      var i = 0
      while (i < out._length){
        out(i) = collA(i) * collB(i)
        i += 1
      }
      out
    }

    override def getGPUInputs = List(collA, collB)
    override def getGPUConsts = List(collA._length)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Vector[Double](elms, collA._is_row, collA._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._length)
    override def getGPUKernelId = List(DeliteCuda.DVectMultDouble, DeliteCuda.AsyncLaunch1D)
	  //override def getGPUKernelId = {List(DeliteCuda.DVectMultDouble, DeliteCuda.Async2I1D)}
  }


  protected[optiml] case class OPGPU_DoubleMult(val coll: DoubleVector, val x: Double)
    extends DeliteOP_SingleTask[Vector[Double]](coll) {

    def task = {
      val out = new DoubleVectorImpl(coll._is_row, coll._length)
      var i = 0
      while (i < out._length){
        out(i) = coll(i) * x
        i += 1
      }
      out
    }

    override def getGPUInputs = List(coll)
    override def getGPUConsts = List(x, coll._length)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Vector[Double](elms, coll._is_row, coll._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(coll._length)
    override def getGPUKernelId = List(DeliteCuda.VectMultDouble_S, DeliteCuda.AsyncLaunch1D)
	  //override def getGPUKernelId = List(DeliteCuda.VectMultDouble_S, DeliteCuda.AsyncRBM_1I1S)
  }
  
  protected[optiml] case class OPGPU_DoublePlus(val coll: DoubleVector, val x: Double)
    extends DeliteOP_SingleTask[Vector[Double]](coll) {

    def task = {
      val out = new DoubleVectorImpl(coll._is_row, coll._length)
      var i = 0
      while (i < out._length){
        out(i) = coll(i) + x
        i += 1
      }
      out
    }

    override def getGPUInputs = List(coll)
    override def getGPUConsts = List(x, coll._length)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Vector[Double](elms, coll.is_row, coll.length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(coll._length)
    override def getGPUKernelId = List(DeliteCuda.VectPlusDouble_S, DeliteCuda.AsyncLaunch1D)
	  //override def getGPUKernelId = List(DeliteCuda.VectPlusDouble_S, DeliteCuda.AsyncRBM_1I1S)
  }

  protected[optiml] case class OPGPU_DoubleDiv(val coll: DoubleVector, val x: Double)
    extends DeliteOP_SingleTask[Vector[Double]](coll) {

    def task = {
      val out = new DoubleVectorImpl(coll._is_row, coll._length)
      var i = 0
      while (i < out._length){
        out(i) = coll(i) / x
        i += 1
      }
      out
    }

    override def getGPUInputs = List(coll)
    override def getGPUConsts = List(x, coll._length)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Vector[Double](elms, coll._is_row, coll._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(coll._length)
    override def getGPUKernelId = List(DeliteCuda.VectDivDouble_S, DeliteCuda.AsyncLaunch1D)
	 // override def getGPUKernelId = List(DeliteCuda.VectDivDouble_S, DeliteCuda.AsyncRBM_1I1S)
  }

  protected[optiml] case class OPGPU_exp(val coll: DoubleVector)
    extends DeliteOP_SingleTask[Vector[Double]](coll) {

      def task = {
        val out = new DoubleVectorImpl(coll._is_row, coll._length)
        var i = 0
        while (i < out._length){
          out(i) = doubleArithOps.exp(coll(i))
          i += 1
        }
        out
      }

      override def getGPUInputs = List(coll)
      override def getGPUConsts = List(coll._length)
      override def getGPUOutput = {
        val elms = new Array[Double](0)
        val gout = Vector[Double](elms, coll._is_row, coll._length)
        gout.cvalue = gout.asInstanceOf[gout.DSLType]
        gout.isComputed = true
        gout
      }
      override def getGPUKernelDims = List(coll._length)
      override def getGPUKernelId = List(DeliteCuda.VectExpDouble, DeliteCuda.AsyncLaunch1D)
      //override def getGPUKernelId = List(DeliteCuda.VectExpDouble, DeliteCuda.AsyncRBM_1I)
  }

  case class OPGPU_REPMAT(v: DoubleVector, iRep: Int, jRep: Int) extends DeliteOP_SingleTask[Matrix[Double]](v) {
    def task = {
	  	val length = v.length
		  var i = 0
      var j = 0
		  var ii = 0
		  var jj = 0

		  if(v.is_row) { // when input is a row vector
        val out = new DoubleMatrixImpl(iRep, jRep*v.length)
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
        val out = new DoubleMatrixImpl(iRep*v.length, jRep)
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

    override def getGPUInputs = List(v)
    override def getGPUConsts = List(v._length, iRep, jRep)
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Matrix[Double](elms, iRep, v._length*jRep)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(v._length*jRep, iRep, 512, 1)
	  override def getGPUKernelId = List(DeliteCuda.VectRepDouble, DeliteCuda.AsyncLaunch2D)
    //override def getGPUKernelId = List(DeliteCuda.VectRepDouble, DeliteCuda.AsyncRBM_Repmat)
  }

  /////////////////////////////////////////////////////////////////////////////////////
  ///////////////////         END OF GPU OPS        ///////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////

  
  /////////////////////
  //  RBM functions

  // TODO: If (iRep==1 for row Vector) or (j==1 for col Vector), does the result have to be a Vector instead of a Matrix?
	/* REPMAT1D : Replication of the input vector(jRep times) to generate larger matrix */
	case class OP_REPMAT(v: DoubleVector, iRep: Int, jRep: Int) extends DeliteOP_SingleTask[Matrix[Double]](v) {
    def task = {
	  	val length = v.length
		  var i = 0
      var j = 0
		  var ii = 0
		  var jj = 0
      
		  if(v.is_row) { // when input is a row vector
        val out = new DoubleMatrixImpl(iRep, jRep*v.length)
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
        val out = new DoubleMatrixImpl(iRep*v.length, jRep)
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

  case class OP_repmat_rowV(v: DoubleVector, iRep: Int, jRep: Int) extends DeliteOP_ForEach[Int, Matrix[Double]](v)(){
    val out = new DoubleMatrixImpl(iRep, jRep*v.length)
    val coll = Vector.range(0,jRep*v.length)

    def func = col => {
      val colToRep = col % v.length
      var rI = 0
      while(rI < iRep){
        out(rI, col) = v(colToRep)
        rI += 1
      }
    }
  }

  case class OP_repmat_colV(v: DoubleVector, iRep: Int, jRep: Int) extends DeliteOP_ForEach[Int, Matrix[Double]](v)(){
    val out = new DoubleMatrixImpl(iRep*v.length, jRep)
    val coll = Vector.range(0,iRep*v.length)

    def func = row => {
      val rowToRep = row % v.length
      var cI = 0
      while(cI < jRep){
        out(row, cI) = v(rowToRep)
        cI += 1
      }
    }
  }

  /*
  protected[delite] case class OP_mapLR[B : ClassManifest](val v: DoubleVector, val func: Double => B, val x_cur:Double, val tau:Int)
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

    override def getGPUInputs = {List(v)}
    override def getGPUOutput = {
      val elms = new Array[Double](0)
      val gout = Vector[Double](elms, v.is_row, v.length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
	  override def getGPUKernelId = {List(DeliteCuda.MAP_LR, DeliteCuda.AsyncMAPLR)}
  }
  */

  

}


trait DoubleVector extends Vector[Double] {
  import DoubleVector._

  protected[optiml] var _data: Array[Double]

  def apply(n: Int) : Double
  override def lifted_apply(n: Int) : Double  = {
    run(OP_apply(this,n))
  }


  def update(index: Int, x: Double) : Unit
  override def lifted_update(index: Int, x: Double) : Unit = {
    run(OP_update(this,index,x))
  }

  override def dc_update(i: Int, x: Double)
  override def dc_apply(i: Int) : Double 

  //////////
  // arith

  // gda, kmeans, nb ops
  override def trans(implicit c: ClassManifest[Double]) : Vector[Double] = {
    if(Config.executorType == "gpu")
      run(OPGPU_trans(this))
    else
      run(OP_trans(this))
  }

  override def +(v: Vector[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Vector[Double] = {
    //run(OP_+(this, v.asInstanceOf[DoubleVector], run(OP_alloc[Double](this)).asInstanceOf[DoubleVector]))
    if(Config.executorType == "gpu")
      run(OPGPU_plus_single(this,v.asInstanceOf[DoubleVector]))
    else
      run(OP_+(this, v.asInstanceOf[DoubleVector], run(OP_alloc[Double](this)).asInstanceOf[DoubleVector]))
      //run(OP_plus_single(this,v.asInstanceOf[DoubleVector]))
  }

  override def outer(v: Vector[Double])(implicit ops: ArithOps[Double], pFact: Matrix.ProxyFactory[Double], c: ClassManifest[Double]) : Matrix[Double] = {
    if(Config.executorType == "gpu")
      run(OPGPU_outer_single(this,v.asInstanceOf[DoubleVector]))
    else
      run(OP_outer_single(this,v.asInstanceOf[DoubleVector]))
  }

  override def -(v: Vector[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Vector[Double] = {
    //run(OP_-(this, v.asInstanceOf[DoubleVector], run(OP_alloc[Double](this)).asInstanceOf[DoubleVector]))
    if(Config.executorType == "gpu")
      run(OPGPU_minus_single(this,v.asInstanceOf[DoubleVector]))
    else
     run(OP_-(this, v.asInstanceOf[DoubleVector], run(OP_alloc[Double](this)).asInstanceOf[DoubleVector]))
     //run(OP_minus_single(this,v.asInstanceOf[DoubleVector]))
  }

  override def dot[B <: DeliteDSLType](v: Vector[Double])(implicit ops: ArithOps[Double], conv: Double => B, pfact: DeliteProxyFactory[B]) : B = {
    run(OP_dot_single(this, v.asInstanceOf[DoubleVector], conv))(pfact)
  }

  // others
  override def +(x: Double)(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Vector[Double] = {
    if(Config.executorType == "gpu")
      run(OPGPU_DoublePlus(this, x))
    else
      run(OP_mapDouble(this, run(OP_alloc[Double](this)).asInstanceOf[DoubleVector], ele => ele+x))
  }

  override def -(x: Double)(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Vector[Double] = {
    run(OP_mapDouble(this, run(OP_alloc[Double](this)).asInstanceOf[DoubleVector], ele => ele-x))
  }

  override def /(v: Vector[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Vector[Double] = {
    run(OP_/(this, v.asInstanceOf[DoubleVector], run(OP_alloc[Double](this)).asInstanceOf[DoubleVector]))
  }
  override def /(x: Double)(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Vector[Double] = {
    if(Config.executorType == "gpu")
      run(OPGPU_DoubleDiv(this, x))
    else
      run(OP_mapDouble(this, run(OP_alloc[Double](this)).asInstanceOf[DoubleVector], ele => ele/x))
  }

  override def *(m: Matrix[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Vector[Double] = {
    run(OP_mtimes(this,m.asInstanceOf[DoubleMatrix]))
  }

  override def *(v: Vector[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Vector[Double] = {
    if(Config.executorType == "gpu")
      run(OPGPU_*(this, v.asInstanceOf[DoubleVector]))
    else {
      //if (Config.useNativeLibs) 
      //  run(OP_native_*(this, v.asInstanceOf[DoubleVector]))
      //else 
        run(OP_*(this, v.asInstanceOf[DoubleVector], run(OP_alloc[Double](this)).asInstanceOf[DoubleVector]))
    }
  }

  override def *(x: Double)(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Vector[Double] = {
    if(Config.executorType == "gpu")
      run(OPGPU_DoubleMult(this, x))
    else
      run(OP_mapDouble(this, run(OP_alloc[Double](this)).asInstanceOf[DoubleVector], ele => ele*x))
  }

  override def sum[B <: DeliteDSLType](implicit pfact: DeliteProxyFactory[B], ops: ArithOps[Double], conv: Double => B): B = {
    if(Config.executorType == "gpu")
      run(OPGPU_sum(this)(conv))
    else
      run(OP_sum(this)(conv))
  }

  override def abs(implicit ops: ArithOps[Double], pfact: DeliteProxyFactory[Vector[Double]], c: ClassManifest[Double]) : Vector[Double] = {
    run(OP_abs(this, run(OP_alloc[Double](this))(pfact).asInstanceOf[DoubleVector]))(pfact)
  }

  override def exp(implicit ops: ArithOps[Double], pfact: DeliteProxyFactory[Vector[Double]], c: ClassManifest[Double]) : Vector[Double] = {
    if(Config.executorType == "gpu")
      run(OPGPU_exp(this))(pfact)
    else
      run(OP_exp(this, run(OP_alloc[Double](this))(pfact).asInstanceOf[DoubleVector]))(pfact)
  }

  
  /////////////////////////////
  // stuff requiring ordering

  override def min[B <: DeliteDSLType](implicit cmp: Double => Ordered[Double], conv: Double => B, pfact: DeliteProxyFactory[B]): B = {
    run(OP_min(this))
  }

  override def minIndex(implicit cmp: Double => Ordered[Double]): DeliteInt = {
    run(OP_minIndex(this))
  }

  override def max[B <: DeliteDSLType](implicit cmp: Double => Ordered[Double], conv: Double => B, pfact: DeliteProxyFactory[B]): B = {
    run(OP_max(this))
  }


  override def sort(implicit cmp: Double => Ordered[Double], pfact: DeliteProxyFactory[Vector[Double]]) : Vector[Double] = {
    run(OP_sort(this))(pfact)
  }

  //////////////////
  // bulk operations

  override def map[B](f: Double => B)(implicit pFact: DeliteProxyFactory[Vector[B]], c: ClassManifest[B]) : Vector[B] = {
    run[Vector[B]](OP_map[B](this, run(OP_alloc[B](this)), f))
  }

  override def reduce[B <: DeliteDSLType](func: (Double,Double) => Double)(implicit conv: Double => B, pfact: DeliteProxyFactory[B]): B = {
    run(OP_reduce(this, func)(conv))
  }


  ///////////////////////////
  // GPU Specific Operations

  // Map operation for Linear Regression
  /*
  override def mapLR[B](f: Double => B)(x_cur:Double, tau:Int)(implicit pFact: DeliteProxyFactory[Vector[B]], c: ClassManifest[B]) : Vector[B] = {
    if(Config.executorType == "gpu")
      run[Vector[B]](OP_mapLR[B](this, f, x_cur, tau))
    else
      run[Vector[B]](OP_map[B](this, run(OP_alloc[B](this)), f))
  }
  */
  
  ////////////////////
  // RBM oeprations

  // RBM : operation that replicates the given matrix to generate a larger matrix
  override def repmat(i: Int, j: Int): Matrix[Double] = {
    if(Config.executorType == "gpu")
      run(OPGPU_REPMAT(this, i, j))
    else{
      if(this.is_row)
        run(OP_repmat_rowV(this, i, j))
      else
        run(OP_repmat_colV(this, i, j))

//      run(OP_REPMAT(this, i, j))
    }
  }
  
}
