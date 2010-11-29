/* Specialized operations for FloatVectors.
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
import ppl.delite.dsl.primitive.{DeliteInt, DeliteFloat}
import ppl.delite.core.ops.{DeliteOP_ZipWith2, DeliteOP_Reduce, DeliteOP_Map, DeliteOP_SingleTask}
import ppl.delite.core.ops.specialized._
import ppl.delite.cnative._

object FloatVector {

  protected[optiml] case class OP_alloc[B](orig: FloatVector)(implicit c: ClassManifest[B]) extends DeliteOP_SingleTask[Vector[B]](orig) {
    def task = {
      Vector[B](orig.is_row, orig.length)
    }
  }
  
  protected[optiml] case class OP_apply(val collA: FloatVector, n: Int)
    extends DeliteOP_SingleTask[DeliteFloat](collA){

    def task = {
      collA(n)
    }
  }

  protected[optiml] case class OP_update(val collA: FloatVector, index: Int, x: Float)
    extends DeliteOP_SingleTask[DeliteUnit](collA){

    def task = {
      collA(index) = x
    }
  }

  protected[optiml] case class OP_trans(val collA: FloatVector)
    extends DeliteOP_SingleTask[Vector[Float]](collA) {

    def task = {
      val out = new FloatVectorImpl(!collA._is_row, collA._length)
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

  protected[optiml] case class OP_plus_single(val collA: FloatVector, val collB: FloatVector)
    extends DeliteOP_SingleTask[Vector[Float]](collA, collB) {

    def task = {
      val out = new FloatVectorImpl(collA._is_row, collA._length)
      var i = 0
      while (i < out._length){
        out(i) = collA(i)+collB(i)
        i += 1
      }
      out
    } 
  }

  protected[optiml] case class OP_outer_single(val collA: FloatVector, val collB: FloatVector)
    extends DeliteOP_SingleTask[Matrix[Float]](collA,collB) {

    def task = {
      if (collA._length != collB._length || collA._is_row == collB._is_row || collA._is_row) throw new IndexOutOfBoundsException()
      val out = new FloatMatrixImpl(collA._length, collA._length)
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
      //Matrix.fromMap[Float,Float](v1, (elem => v2*elem))
      out
    }
  }

  protected[optiml] case class OP_minus_single(val collA: FloatVector, val collB: FloatVector)
    extends DeliteOP_SingleTask[Vector[Float]](collA, collB) {

    def task = {
      val out = new FloatVectorImpl(collA._is_row, collA._length)
      var i = 0
      while (i < out._length){
        out(i) = collA(i)-collB(i)
        i += 1
      }
      out
    }
  }

  protected[optiml] case class OP_dot_single[B <: DeliteDSLType](collA: FloatVector, collB: FloatVector, conv: Float => B)
    extends DeliteOP_SingleTask[B](collA,collB) {

    def task = {
      collA.chkLength(collA,collB)
      var acc : Float = 0
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

  protected[optiml] case class OP_+(val collA: FloatVector, val collB: FloatVector, val out: FloatVector)
    extends DeliteOP_ZipWith2Spec[Float,Vector] {
    override val associative = true

    def func = (a,b) => a+b

  }

  protected[optiml] case class OP_-(val collA: FloatVector, val collB: FloatVector, val out: FloatVector)
    extends DeliteOP_ZipWith2Spec[Float,Vector] {

    def func = (a,b) => a-b
  }

  protected[optiml] case class OP_/(val collA: FloatVector, val collB: FloatVector, val out: FloatVector)
    extends DeliteOP_ZipWith2Spec[Float,Vector] {

    def func = (a,b) => a/b
  }

  protected[optiml] case class OP_mtimes(v: FloatVector, m: FloatMatrix)
    extends DeliteOP_SingleTask[Vector[Float]](v,m) {

    def task = {
      v.chkVecMatAgree(v, m)
      val v_trans = v.trans
      m.trans.mapRowsToVec(a_row => a_row._dot(v_trans))
    }
  }

  protected[optiml] case class OP_*(val collA: FloatVector, val collB: FloatVector, val out: FloatVector)
    extends DeliteOP_ZipWith2Spec[Float,Vector] {

    def func = (a,b) => a*b
  }

  protected[optiml] case class OP_native_*(val v1: FloatVector, val v2: FloatVector)
    extends DeliteOP_SingleTask[Vector[Float]](v1, v2) {
		
      def task = {
        val out = new FloatVectorImpl(v1._is_row, v1._length)
        DeliteNative.vectMultFloat(v1._data, v2._data, out._data, v1._length)
        out
      }
  }

  protected[optiml] case class OP_outer(v1: FloatVector, v2: FloatVector)
    extends DeliteOP_SingleTask[Matrix[Float]](v1,v2) {

    def task = {
      if (v1.length != v2.length || v1.is_row == v2.is_row || v1.is_row) v1.vecDimError()
      Matrix.fromMap[Float,Float](v1, (a_row => v2*a_row))
    }
  }

  protected[optiml] case class OP_dot[B <: DeliteDSLType](v1: FloatVector, v2: FloatVector)
    (implicit conv: Float => B) extends DeliteOP_SingleTask[B](v1, v2) {

    def task = {
      v1.chkLength(v1, v2)
      var acc : Float = v1(0)*v2(0)
      for (i <- 1 until v1.length){
        acc = acc + (v1(i)*v2(i))
      }
      conv(acc)
    }
  }


  protected[optiml] case class OP_sum[B <: DeliteDSLType](val coll: FloatVector)
    (implicit conv: Float => B) extends DeliteOP_ReduceSpec[Float,B] {

    def func = (a,b) => a+b
  }

  protected[optiml] case class OP_abs(val coll: FloatVector, val out: FloatVector)
    extends DeliteOP_MapSpec[Float,Vector]{

    def func = e => floatArithOps.abs(e)
  }

  protected[optiml] case class OP_exp(val coll: FloatVector, val out: FloatVector)
    extends DeliteOP_Map[Float,Float,Vector]{

    def func = e => floatArithOps.exp(e)
  }


  /////////////////
  // ordering ops

  protected[optiml] case class OP_min[B <: DeliteDSLType](val coll: FloatVector)(implicit conv: Float => B)
    extends DeliteOP_ReduceSpec[Float,B] {

    def func = (a,b) => if (a < b) a else b
  }

  protected[optiml] case class OP_minIndex(v: FloatVector)
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

  protected[optiml] case class OP_max[B <: DeliteDSLType](val coll: FloatVector)(implicit conv: Float => B)
    extends DeliteOP_ReduceSpec[Float,B] {

    def func = (a,b) => if (a > b) a else b
  }

  /////////////
  // bulk ops

  protected[optiml] case class OP_reduce[B <: DeliteDSLType](val coll: FloatVector, val func: (Float,Float) => Float)
    (implicit conv: Float => B) extends DeliteOP_ReduceSpec[Float,B]

  protected[optiml] case class OP_map[B : ClassManifest](val coll: FloatVector, val out: Vector[B], val func: Float => B)
    extends DeliteOP_Map[Float,B,Vector]

  protected[optiml] case class OP_mapFloat(val coll: FloatVector, val out: FloatVector, val func: Float => Float)
    extends DeliteOP_MapSpec[Float,Vector]


  /////////////////////////////////////////////////////////////////////////////////////
  ///////////////////            GPU OPS        ///////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////
  
  protected[optiml] case class OPGPU_trans(val collA: FloatVector)
    extends DeliteOP_SingleTask[Vector[Float]](collA) {

    def task = {
      val out = new FloatVectorImpl(!collA._is_row, collA._length)
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
      val elms = new Array[Float](0)
      val gout = Vector[Float](elms, !collA._is_row, collA._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._length)
    override def getGPUKernelId = List(DeliteCuda.DVectMoveFloat, DeliteCuda.AsyncLaunch1D)
    //override def getGPUKernelId = {List(DeliteCuda.DVectMoveFloat, DeliteCuda.Async1I1D)}
  }

  protected[optiml] case class OPGPU_plus_single(val collA: FloatVector, val collB: FloatVector)
    extends DeliteOP_SingleTask[Vector[Float]](collA, collB) {

    def task = {
      val out = new FloatVectorImpl(collA._is_row, collA._length)
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
      val elms = new Array[Float](0)
      val gout = Vector[Float](elms, collA._is_row, collA._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._length)
	  override def getGPUKernelId = List(DeliteCuda.DVectPlusFloat, DeliteCuda.AsyncLaunch1D)
    //override def getGPUKernelId = {List(DeliteCuda.DVectPlusFloat, DeliteCuda.Async2I1D)}
  }

  protected[optiml] case class OPGPU_outer_single(val collA: FloatVector, val collB: FloatVector)
    extends DeliteOP_SingleTask[Matrix[Float]](collA,collB) {

    def task = {
      if (collA._length != collB._length || collA._is_row == collB._is_row || collA._is_row) throw new IndexOutOfBoundsException()
      val out = new FloatMatrixImpl(collA._length, collA._length)
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
      val elms = new Array[Float](0)
      val gout = Matrix[Float](elms, collA._length, collA._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._length, collA._length/16, 16, 1)
	  override def getGPUKernelId = List(DeliteCuda.DVectOuterFloat, DeliteCuda.AsyncLaunch2D)
    //override def getGPUKernelId = {List(DeliteCuda.DVectOuterFloat, DeliteCuda.Async2I2D)}
  }

  protected[optiml] case class OPGPU_minus_single(val collA: FloatVector, val collB: FloatVector)
    extends DeliteOP_SingleTask[Vector[Float]](collA, collB) {

    def task = {
      val out = new FloatVectorImpl(collA._is_row, collA._length)
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
      val elms = new Array[Float](0)
      val gout = Vector[Float](elms, collA._is_row, collA._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._length)
    override def getGPUKernelId = {List(DeliteCuda.DVectMinusFloat, DeliteCuda.AsyncLaunch1D)}
	  //override def getGPUKernelId = {List(DeliteCuda.DVectMinusFloat, DeliteCuda.Async2I1D)}
  }

  protected[optiml] case class OPGPU_sum[B <: DeliteDSLType](val coll: FloatVector)
    (implicit conv: Float => B) extends DeliteOP_ReduceSpec[Float,B] {

    def func = (a,b) => a+b
    /*
    override def getGPUInputs = List(coll)
    override def getGPUConsts = List(coll.size)
    override def getGPUOutput = {
      //val elms = new Array[Float](0)
      //val gout = Vector[Float](elms, true, 1)
      val gout = DeliteFloat(0.0)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
	  override def getGPUKernelId = List(DeliteCuda.sumFloat, DeliteCuda.Async1I1D)
	  */
  }

  protected[optiml] case class OPGPU_*(val collA: FloatVector, val collB: FloatVector)
    extends DeliteOP_SingleTask[Vector[Float]](collA, collB) {

    def task = {
      val out = new FloatVectorImpl(collA._is_row, collB._length)
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
      val elms = new Array[Float](0)
      val gout = Vector[Float](elms, collA._is_row, collA._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._length)
    override def getGPUKernelId = List(DeliteCuda.DVectMultFloat, DeliteCuda.AsyncLaunch1D)
	  //override def getGPUKernelId = {List(DeliteCuda.DVectMultFloat, DeliteCuda.Async2I1D)}
  }


  protected[optiml] case class OPGPU_FloatMult(val coll: FloatVector, val x: Float)
    extends DeliteOP_SingleTask[Vector[Float]](coll) {

    def task = {
      val out = new FloatVectorImpl(coll._is_row, coll._length)
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
      val elms = new Array[Float](0)
      val gout = Vector[Float](elms, coll._is_row, coll._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(coll._length)
    override def getGPUKernelId = List(DeliteCuda.VectMultFloat_S, DeliteCuda.AsyncLaunch1D)
	  //override def getGPUKernelId = List(DeliteCuda.VectMultFloat_S, DeliteCuda.AsyncRBM_1I1S)
  }
  
  protected[optiml] case class OPGPU_FloatPlus(val coll: FloatVector, val x: Float)
    extends DeliteOP_SingleTask[Vector[Float]](coll) {

    def task = {
      val out = new FloatVectorImpl(coll._is_row, coll._length)
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
      val elms = new Array[Float](0)
      val gout = Vector[Float](elms, coll.is_row, coll.length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(coll._length)
    override def getGPUKernelId = List(DeliteCuda.VectPlusFloat_S, DeliteCuda.AsyncLaunch1D)
	  //override def getGPUKernelId = List(DeliteCuda.VectPlusFloat_S, DeliteCuda.AsyncRBM_1I1S)
  }

  protected[optiml] case class OPGPU_FloatDiv(val coll: FloatVector, val x: Float)
    extends DeliteOP_SingleTask[Vector[Float]](coll) {

    def task = {
      val out = new FloatVectorImpl(coll._is_row, coll._length)
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
      val elms = new Array[Float](0)
      val gout = Vector[Float](elms, coll._is_row, coll._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(coll._length)
    override def getGPUKernelId = List(DeliteCuda.VectDivFloat_S, DeliteCuda.AsyncLaunch1D)
	 // override def getGPUKernelId = List(DeliteCuda.VectDivFloat_S, DeliteCuda.AsyncRBM_1I1S)
  }

  protected[optiml] case class OPGPU_exp(val coll: FloatVector)
    extends DeliteOP_SingleTask[Vector[Float]](coll) {

      def task = {
        val out = new FloatVectorImpl(coll._is_row, coll._length)
        var i = 0
        while (i < out._length){
          out(i) = floatArithOps.exp(coll(i))
          i += 1
        }
        out
      }

      override def getGPUInputs = List(coll)
      override def getGPUConsts = List(coll._length)
      override def getGPUOutput = {
        val elms = new Array[Float](0)
        val gout = Vector[Float](elms, coll._is_row, coll._length)
        gout.cvalue = gout.asInstanceOf[gout.DSLType]
        gout.isComputed = true
        gout
      }
      override def getGPUKernelDims = List(coll._length)
      override def getGPUKernelId = List(DeliteCuda.VectExpFloat, DeliteCuda.AsyncLaunch1D)
      //override def getGPUKernelId = List(DeliteCuda.VectExpFloat, DeliteCuda.AsyncRBM_1I)
  }

  case class OPGPU_REPMAT(v: FloatVector, iRep: Int, jRep: Int) extends DeliteOP_SingleTask[Matrix[Float]](v) {
    def task = {
	  	val length = v.length
		  var i = 0
      var j = 0
		  var ii = 0
		  var jj = 0

		  if(v.is_row) { // when input is a row vector
        val out = new FloatMatrixImpl(iRep, jRep*v.length)
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
        val out = new FloatMatrixImpl(iRep*v.length, jRep)
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
      val elms = new Array[Float](0)
      val gout = Matrix[Float](elms, iRep, v._length*jRep)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(v._length*jRep, iRep, 512, 1)
	  override def getGPUKernelId = List(DeliteCuda.VectRepFloat, DeliteCuda.AsyncLaunch2D)
    //override def getGPUKernelId = List(DeliteCuda.VectRepFloat, DeliteCuda.AsyncRBM_Repmat)
  }

  /////////////////////////////////////////////////////////////////////////////////////
  ///////////////////         END OF GPU OPS        ///////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////

  
  /////////////////////
  //  RBM functions

  // TODO: If (iRep==1 for row Vector) or (j==1 for col Vector), does the result have to be a Vector instead of a Matrix?
	/* REPMAT1D : Replication of the input vector(jRep times) to generate larger matrix */
	case class OP_REPMAT(v: FloatVector, iRep: Int, jRep: Int) extends DeliteOP_SingleTask[Matrix[Float]](v) {
    def task = {
	  	val length = v.length
		  var i = 0
      var j = 0
		  var ii = 0
		  var jj = 0
      
		  if(v.is_row) { // when input is a row vector
        val out = new FloatMatrixImpl(iRep, jRep*v.length)
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
        val out = new FloatMatrixImpl(iRep*v.length, jRep)
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
  protected[delite] case class OP_mapLR[B : ClassManifest](val v: FloatVector, val func: Float => B, val x_cur:Float, val tau:Int)
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
      val elms = new Array[Float](0)
      val gout = Vector[Float](elms, v.is_row, v.length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
	  override def getGPUKernelId = {List(DeliteCuda.MAP_LR, DeliteCuda.AsyncMAPLR)}
  }
  */

  

}


trait FloatVector extends Vector[Float] {
  import FloatVector._

  protected[optiml] var _data: Array[Float]

  def apply(n: Int) : Float
  override def lifted_apply(n: Int) : Float  = {
    run(OP_apply(this,n))
  }


  def update(index: Int, x: Float) : Unit
  override def lifted_update(index: Int, x: Float) : Unit = {
    run(OP_update(this,index,x))
  }

  override def dc_update(i: Int, x: Float)
  override def dc_apply(i: Int) : Float 

  //////////
  // arith

  // gda, kmeans, nb ops
  override def trans(implicit c: ClassManifest[Float]) : Vector[Float] = {
    if(Config.executorType == "gpu")
      run(OPGPU_trans(this))
    else
      run(OP_trans(this))
  }

  override def +(v: Vector[Float])(implicit ops: ArithOps[Float], c: ClassManifest[Float]) : Vector[Float] = {
    //run(OP_+(this, v.asInstanceOf[FloatVector], run(OP_alloc[Float](this)).asInstanceOf[FloatVector]))
    if(Config.executorType == "gpu")
      run(OPGPU_plus_single(this,v.asInstanceOf[FloatVector]))
    else
      run(OP_+(this, v.asInstanceOf[FloatVector], run(OP_alloc[Float](this)).asInstanceOf[FloatVector]))
      //run(OP_plus_single(this,v.asInstanceOf[FloatVector]))
  }

  override def outer(v: Vector[Float])(implicit ops: ArithOps[Float], pFact: Matrix.ProxyFactory[Float], c: ClassManifest[Float]) : Matrix[Float] = {
    if(Config.executorType == "gpu")
      run(OPGPU_outer_single(this,v.asInstanceOf[FloatVector]))
    else
      run(OP_outer_single(this,v.asInstanceOf[FloatVector]))
  }

  override def -(v: Vector[Float])(implicit ops: ArithOps[Float], c: ClassManifest[Float]) : Vector[Float] = {
    //run(OP_-(this, v.asInstanceOf[FloatVector], run(OP_alloc[Float](this)).asInstanceOf[FloatVector]))
    if(Config.executorType == "gpu")
      run(OPGPU_minus_single(this,v.asInstanceOf[FloatVector]))
    else
     run(OP_-(this, v.asInstanceOf[FloatVector], run(OP_alloc[Float](this)).asInstanceOf[FloatVector]))
     //run(OP_minus_single(this,v.asInstanceOf[FloatVector]))
  }

  override def dot[B <: DeliteDSLType](v: Vector[Float])(implicit ops: ArithOps[Float], conv: Float => B, pfact: DeliteProxyFactory[B]) : B = {
    run(OP_dot_single(this, v.asInstanceOf[FloatVector], conv))(pfact)
  }

  // others
  override def +(x: Float)(implicit ops: ArithOps[Float], c: ClassManifest[Float]) : Vector[Float] = {
    if(Config.executorType == "gpu")
      run(OPGPU_FloatPlus(this, x))
    else
      run(OP_mapFloat(this, run(OP_alloc[Float](this)).asInstanceOf[FloatVector], ele => ele+x))
  }

  override def -(x: Float)(implicit ops: ArithOps[Float], c: ClassManifest[Float]) : Vector[Float] = {
    run(OP_mapFloat(this, run(OP_alloc[Float](this)).asInstanceOf[FloatVector], ele => ele-x))
  }

  override def /(v: Vector[Float])(implicit ops: ArithOps[Float], c: ClassManifest[Float]) : Vector[Float] = {
    run(OP_/(this, v.asInstanceOf[FloatVector], run(OP_alloc[Float](this)).asInstanceOf[FloatVector]))
  }
  override def /(x: Float)(implicit ops: ArithOps[Float], c: ClassManifest[Float]) : Vector[Float] = {
    if(Config.executorType == "gpu")
      run(OPGPU_FloatDiv(this, x))
    else
      run(OP_mapFloat(this, run(OP_alloc[Float](this)).asInstanceOf[FloatVector], ele => ele/x))
  }

  override def *(m: Matrix[Float])(implicit ops: ArithOps[Float], c: ClassManifest[Float]) : Vector[Float] = {
    run(OP_mtimes(this,m.asInstanceOf[FloatMatrix]))
  }

  override def *(v: Vector[Float])(implicit ops: ArithOps[Float], c: ClassManifest[Float]) : Vector[Float] = {
    if(Config.executorType == "gpu")
      run(OPGPU_*(this, v.asInstanceOf[FloatVector]))
    else {
      //if (Config.useNativeLibs) 
      //  run(OP_native_*(this, v.asInstanceOf[FloatVector]))
      //else 
        run(OP_*(this, v.asInstanceOf[FloatVector], run(OP_alloc[Float](this)).asInstanceOf[FloatVector]))
    }
  }

  override def *(x: Float)(implicit ops: ArithOps[Float], c: ClassManifest[Float]) : Vector[Float] = {
    if(Config.executorType == "gpu")
      run(OPGPU_FloatMult(this, x))
    else
      run(OP_mapFloat(this, run(OP_alloc[Float](this)).asInstanceOf[FloatVector], ele => ele*x))
  }

  override def sum[B <: DeliteDSLType](implicit pfact: DeliteProxyFactory[B], ops: ArithOps[Float], conv: Float => B): B = {
    if(Config.executorType == "gpu")
      run(OPGPU_sum(this)(conv))
    else
      run(OP_sum(this)(conv))
  }

  override def abs(implicit ops: ArithOps[Float], pfact: DeliteProxyFactory[Vector[Float]], c: ClassManifest[Float]) : Vector[Float] = {
    run(OP_abs(this, run(OP_alloc[Float](this))(pfact).asInstanceOf[FloatVector]))(pfact)
  }

  override def exp(implicit ops: ArithOps[Float], pfact: DeliteProxyFactory[Vector[Float]], c: ClassManifest[Float]) : Vector[Float] = {
    if(Config.executorType == "gpu")
      run(OPGPU_exp(this))(pfact)
    else
      run(OP_exp(this, run(OP_alloc[Float](this))(pfact).asInstanceOf[FloatVector]))(pfact)
  }

  
  /////////////////////////////
  // stuff requiring ordering

  override def min[B <: DeliteDSLType](implicit cmp: Float => Ordered[Float], conv: Float => B, pfact: DeliteProxyFactory[B]): B = {
    run(OP_min(this))
  }

  override def minIndex(implicit cmp: Float => Ordered[Float]): DeliteInt = {
    run(OP_minIndex(this))
  }

  override def max[B <: DeliteDSLType](implicit cmp: Float => Ordered[Float], conv: Float => B, pfact: DeliteProxyFactory[B]): B = {
    run(OP_max(this))
  }

  //////////////////
  // bulk operations

  override def map[B](f: Float => B)(implicit pFact: DeliteProxyFactory[Vector[B]], c: ClassManifest[B]) : Vector[B] = {
    run[Vector[B]](OP_map[B](this, run(OP_alloc[B](this)), f))
  }

  override def reduce[B <: DeliteDSLType](func: (Float,Float) => Float)(implicit conv: Float => B, pfact: DeliteProxyFactory[B]): B = {
    run(OP_reduce(this, func)(conv))
  }


  ///////////////////////////
  // GPU Specific Operations

  // Map operation for Linear Regression
  /*
  override def mapLR[B](f: Float => B)(x_cur:Float, tau:Int)(implicit pFact: DeliteProxyFactory[Vector[B]], c: ClassManifest[B]) : Vector[B] = {
    if(Config.executorType == "gpu")
      run[Vector[B]](OP_mapLR[B](this, f, x_cur, tau))
    else
      run[Vector[B]](OP_map[B](this, run(OP_alloc[B](this)), f))
  }
  */
  
  ////////////////////
  // RBM oeprations

  // RBM : operation that replicates the given matrix to generate a larger matrix
  override def repmat(i: Int, j: Int): Matrix[Float] = {
    if(Config.executorType == "gpu")
      run(OPGPU_REPMAT(this, i, j))
    else
      run(OP_REPMAT(this, i, j))
  }
  
}
