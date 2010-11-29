/* Specialized operations for IntVectors.
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
import ppl.delite.dsl.primitive.DeliteInt
import ppl.delite.core.ops.{DeliteOP_ZipWith2, DeliteOP_Reduce, DeliteOP_Map, DeliteOP_SingleTask}
import ppl.delite.core.ops.specialized._
import ppl.delite.cnative._

object IntVector {

  protected[optiml] case class OP_alloc[B](orig: IntVector)(implicit c: ClassManifest[B]) extends DeliteOP_SingleTask[Vector[B]](orig) {
    def task = {
      Vector[B](orig.is_row, orig.length)
    }
  }
  
  protected[optiml] case class OP_apply(val collA: IntVector, n: Int)
    extends DeliteOP_SingleTask[DeliteInt](collA){

    def task = {
      collA(n)
    }
  }

  protected[optiml] case class OP_update(val collA: IntVector, index: Int, x: Int)
    extends DeliteOP_SingleTask[DeliteUnit](collA){

    def task = {
      collA(index) = x
    }
  }

  protected[optiml] case class OP_trans(val collA: IntVector)
    extends DeliteOP_SingleTask[Vector[Int]](collA) {

    def task = {
      val out = new IntVectorImpl(!collA._is_row, collA._length)
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

  protected[optiml] case class OP_plus_single(val collA: IntVector, val collB: IntVector)
    extends DeliteOP_SingleTask[Vector[Int]](collA, collB) {

    def task = {
      val out = new IntVectorImpl(collA._is_row, collA._length)
      var i = 0
      while (i < out._length){
        out(i) = collA(i)+collB(i)
        i += 1
      }
      out
    } 
  }

  protected[optiml] case class OP_outer_single(val collA: IntVector, val collB: IntVector)
    extends DeliteOP_SingleTask[Matrix[Int]](collA,collB) {

    def task = {
      if (collA._length != collB._length || collA._is_row == collB._is_row || collA._is_row) throw new IndexOutOfBoundsException()
      val out = new IntMatrixImpl(collA._length, collA._length)
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
      //Matrix.fromMap[Int,Int](v1, (elem => v2*elem))
      out
    }
  }

  protected[optiml] case class OP_minus_single(val collA: IntVector, val collB: IntVector)
    extends DeliteOP_SingleTask[Vector[Int]](collA, collB) {

    def task = {
      val out = new IntVectorImpl(collA._is_row, collA._length)
      var i = 0
      while (i < out._length){
        out(i) = collA(i)-collB(i)
        i += 1
      }
      out
    }
  }

  protected[optiml] case class OP_dot_single[B <: DeliteDSLType](collA: IntVector, collB: IntVector, conv: Int => B)
    extends DeliteOP_SingleTask[B](collA,collB) {

    def task = {
      collA.chkLength(collA,collB)
      var acc : Int = 0
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

  protected[optiml] case class OP_+(val collA: IntVector, val collB: IntVector, val out: IntVector)
    extends DeliteOP_ZipWith2Spec[Int,Vector] {
    override val associative = true

    def func = (a,b) => a+b

  }

  protected[optiml] case class OP_-(val collA: IntVector, val collB: IntVector, val out: IntVector)
    extends DeliteOP_ZipWith2Spec[Int,Vector] {

    def func = (a,b) => a-b
  }

  protected[optiml] case class OP_/(val collA: IntVector, val collB: IntVector, val out: IntVector)
    extends DeliteOP_ZipWith2Spec[Int,Vector] {

    def func = (a,b) => a/b
  }

  protected[optiml] case class OP_mtimes(v: IntVector, m: IntMatrix)
    extends DeliteOP_SingleTask[Vector[Int]](v,m) {

    def task = {
      v.chkVecMatAgree(v, m)
      val v_trans = v.trans
      m.trans.mapRowsToVec(a_row => a_row._dot(v_trans))
    }
  }

  protected[optiml] case class OP_*(val collA: IntVector, val collB: IntVector, val out: IntVector)
    extends DeliteOP_ZipWith2Spec[Int,Vector] {

    def func = (a,b) => a*b
  }

  protected[optiml] case class OP_native_*(val v1: IntVector, val v2: IntVector)
    extends DeliteOP_SingleTask[Vector[Int]](v1, v2) {
		
      def task = {
        val out = new IntVectorImpl(v1._is_row, v1._length)
        throw new UnsupportedOperationException
//DeliteNative.vectMultInt(v1._data, v2._data, out._data, v1._length)
        out
      }
  }

  protected[optiml] case class OP_outer(v1: IntVector, v2: IntVector)
    extends DeliteOP_SingleTask[Matrix[Int]](v1,v2) {

    def task = {
      if (v1.length != v2.length || v1.is_row == v2.is_row || v1.is_row) v1.vecDimError()
      Matrix.fromMap[Int,Int](v1, (a_row => v2*a_row))
    }
  }

  protected[optiml] case class OP_dot[B <: DeliteDSLType](v1: IntVector, v2: IntVector)
    (implicit conv: Int => B) extends DeliteOP_SingleTask[B](v1, v2) {

    def task = {
      v1.chkLength(v1, v2)
      var acc : Int = v1(0)*v2(0)
      for (i <- 1 until v1.length){
        acc = acc + (v1(i)*v2(i))
      }
      conv(acc)
    }
  }


  protected[optiml] case class OP_sum[B <: DeliteDSLType](val coll: IntVector)
    (implicit conv: Int => B) extends DeliteOP_ReduceSpec[Int,B] {

    def func = (a,b) => a+b
  }

  protected[optiml] case class OP_abs(val coll: IntVector, val out: IntVector)
    extends DeliteOP_MapSpec[Int,Vector]{

    def func = e => intArithOps.abs(e)
  }

  protected[optiml] case class OP_exp(val coll: IntVector, val out: IntVector)
    extends DeliteOP_Map[Int,Int,Vector]{

    def func = e => intArithOps.exp(e)
  }


  /////////////////
  // ordering ops

  protected[optiml] case class OP_min[B <: DeliteDSLType](val coll: IntVector)(implicit conv: Int => B)
    extends DeliteOP_ReduceSpec[Int,B] {

    def func = (a,b) => if (a < b) a else b
  }

  protected[optiml] case class OP_minIndex(v: IntVector)
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

  protected[optiml] case class OP_max[B <: DeliteDSLType](val coll: IntVector)(implicit conv: Int => B)
    extends DeliteOP_ReduceSpec[Int,B] {

    def func = (a,b) => if (a > b) a else b
  }

  /////////////
  // bulk ops

  protected[optiml] case class OP_reduce[B <: DeliteDSLType](val coll: IntVector, val func: (Int,Int) => Int)
    (implicit conv: Int => B) extends DeliteOP_ReduceSpec[Int,B]

  protected[optiml] case class OP_map[B : ClassManifest](val coll: IntVector, val out: Vector[B], val func: Int => B)
    extends DeliteOP_Map[Int,B,Vector]

  protected[optiml] case class OP_mapInt(val coll: IntVector, val out: IntVector, val func: Int => Int)
    extends DeliteOP_MapSpec[Int,Vector]


  /////////////////////////////////////////////////////////////////////////////////////
  ///////////////////            GPU OPS        ///////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////
  
  protected[optiml] case class OPGPU_trans(val collA: IntVector)
    extends DeliteOP_SingleTask[Vector[Int]](collA) {

    def task = {
      val out = new IntVectorImpl(!collA._is_row, collA._length)
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
      val elms = new Array[Int](0)
      val gout = Vector[Int](elms, !collA._is_row, collA._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._length)
    *///override def getGPUKernelId = List(DeliteCuda.DVectMoveInt, DeliteCuda.AsyncLaunch1D)
    //*///override def getGPUKernelId = {List(DeliteCuda.DVectMoveInt, DeliteCuda.Async1I1D)}
  }

  protected[optiml] case class OPGPU_plus_single(val collA: IntVector, val collB: IntVector)
    extends DeliteOP_SingleTask[Vector[Int]](collA, collB) {

    def task = {
      val out = new IntVectorImpl(collA._is_row, collA._length)
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
      val elms = new Array[Int](0)
      val gout = Vector[Int](elms, collA._is_row, collA._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._length)
	  *///override def getGPUKernelId = List(DeliteCuda.DVectPlusInt, DeliteCuda.AsyncLaunch1D)
    //*///override def getGPUKernelId = {List(DeliteCuda.DVectPlusInt, DeliteCuda.Async2I1D)}
  }

  protected[optiml] case class OPGPU_outer_single(val collA: IntVector, val collB: IntVector)
    extends DeliteOP_SingleTask[Matrix[Int]](collA,collB) {

    def task = {
      if (collA._length != collB._length || collA._is_row == collB._is_row || collA._is_row) throw new IndexOutOfBoundsException()
      val out = new IntMatrixImpl(collA._length, collA._length)
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
      val elms = new Array[Int](0)
      val gout = Matrix[Int](elms, collA._length, collA._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._length, collA._length/16, 16, 1)
	  *///override def getGPUKernelId = List(DeliteCuda.DVectOuterInt, DeliteCuda.AsyncLaunch2D)
    //*///override def getGPUKernelId = {List(DeliteCuda.DVectOuterInt, DeliteCuda.Async2I2D)}
  }

  protected[optiml] case class OPGPU_minus_single(val collA: IntVector, val collB: IntVector)
    extends DeliteOP_SingleTask[Vector[Int]](collA, collB) {

    def task = {
      val out = new IntVectorImpl(collA._is_row, collA._length)
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
      val elms = new Array[Int](0)
      val gout = Vector[Int](elms, collA._is_row, collA._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._length)
    *///override def getGPUKernelId = {List(DeliteCuda.DVectMinusInt, DeliteCuda.AsyncLaunch1D)}
	  //*///override def getGPUKernelId = {List(DeliteCuda.DVectMinusInt, DeliteCuda.Async2I1D)}
  }

  protected[optiml] case class OPGPU_sum[B <: DeliteDSLType](val coll: IntVector)
    (implicit conv: Int => B) extends DeliteOP_ReduceSpec[Int,B] {

    def func = (a,b) => a+b
    /*
    /*override def getGPUInputs = List(coll)
    override def getGPUConsts = List(coll.size)
    override def getGPUOutput = {
      //val elms = new Array[Int](0)
      //val gout = Vector[Int](elms, true, 1)
      val gout = DeliteInt(0.0)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
	  *///override def getGPUKernelId = List(DeliteCuda.sumInt, DeliteCuda.Async1I1D)
	  */
  }

  protected[optiml] case class OPGPU_*(val collA: IntVector, val collB: IntVector)
    extends DeliteOP_SingleTask[Vector[Int]](collA, collB) {

    def task = {
      val out = new IntVectorImpl(collA._is_row, collB._length)
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
      val elms = new Array[Int](0)
      val gout = Vector[Int](elms, collA._is_row, collA._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(collA._length)
    *///override def getGPUKernelId = List(DeliteCuda.DVectMultInt, DeliteCuda.AsyncLaunch1D)
	  //*///override def getGPUKernelId = {List(DeliteCuda.DVectMultInt, DeliteCuda.Async2I1D)}
  }


  protected[optiml] case class OPGPU_IntMult(val coll: IntVector, val x: Int)
    extends DeliteOP_SingleTask[Vector[Int]](coll) {

    def task = {
      val out = new IntVectorImpl(coll._is_row, coll._length)
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
      val elms = new Array[Int](0)
      val gout = Vector[Int](elms, coll._is_row, coll._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(coll._length)
    *///override def getGPUKernelId = List(DeliteCuda.VectMultInt_S, DeliteCuda.AsyncLaunch1D)
	  //*///override def getGPUKernelId = List(DeliteCuda.VectMultInt_S, DeliteCuda.AsyncRBM_1I1S)
  }
  
  protected[optiml] case class OPGPU_IntPlus(val coll: IntVector, val x: Int)
    extends DeliteOP_SingleTask[Vector[Int]](coll) {

    def task = {
      val out = new IntVectorImpl(coll._is_row, coll._length)
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
      val elms = new Array[Int](0)
      val gout = Vector[Int](elms, coll.is_row, coll.length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(coll._length)
    *///override def getGPUKernelId = List(DeliteCuda.VectPlusInt_S, DeliteCuda.AsyncLaunch1D)
	  //*///override def getGPUKernelId = List(DeliteCuda.VectPlusInt_S, DeliteCuda.AsyncRBM_1I1S)
  }

  protected[optiml] case class OPGPU_IntDiv(val coll: IntVector, val x: Int)
    extends DeliteOP_SingleTask[Vector[Int]](coll) {

    def task = {
      val out = new IntVectorImpl(coll._is_row, coll._length)
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
      val elms = new Array[Int](0)
      val gout = Vector[Int](elms, coll._is_row, coll._length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(coll._length)
    *///override def getGPUKernelId = List(DeliteCuda.VectDivInt_S, DeliteCuda.AsyncLaunch1D)
	 // *///override def getGPUKernelId = List(DeliteCuda.VectDivInt_S, DeliteCuda.AsyncRBM_1I1S)
  }

  protected[optiml] case class OPGPU_exp(val coll: IntVector)
    extends DeliteOP_SingleTask[Vector[Int]](coll) {

      def task = {
        val out = new IntVectorImpl(coll._is_row, coll._length)
        var i = 0
        while (i < out._length){
          out(i) = intArithOps.exp(coll(i))
          i += 1
        }
        out
      }

      /*override def getGPUInputs = List(coll)
      override def getGPUConsts = List(coll._length)
      override def getGPUOutput = {
        val elms = new Array[Int](0)
        val gout = Vector[Int](elms, coll._is_row, coll._length)
        gout.cvalue = gout.asInstanceOf[gout.DSLType]
        gout.isComputed = true
        gout
      }
      override def getGPUKernelDims = List(coll._length)
      *///override def getGPUKernelId = List(DeliteCuda.VectExpInt, DeliteCuda.AsyncLaunch1D)
      //*///override def getGPUKernelId = List(DeliteCuda.VectExpInt, DeliteCuda.AsyncRBM_1I)
  }

  case class OPGPU_REPMAT(v: IntVector, iRep: Int, jRep: Int) extends DeliteOP_SingleTask[Matrix[Int]](v) {
    def task = {
	  	val length = v.length
		  var i = 0
      var j = 0
		  var ii = 0
		  var jj = 0

		  if(v.is_row) { // when input is a row vector
        val out = new IntMatrixImpl(iRep, jRep*v.length)
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
        val out = new IntMatrixImpl(iRep*v.length, jRep)
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
      val elms = new Array[Int](0)
      val gout = Matrix[Int](elms, iRep, v._length*jRep)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
    override def getGPUKernelDims = List(v._length*jRep, iRep, 512, 1)
	  *///override def getGPUKernelId = List(DeliteCuda.VectRepInt, DeliteCuda.AsyncLaunch2D)
    //*///override def getGPUKernelId = List(DeliteCuda.VectRepInt, DeliteCuda.AsyncRBM_Repmat)
  }

  /////////////////////////////////////////////////////////////////////////////////////
  ///////////////////         END OF GPU OPS        ///////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////

  
  /////////////////////
  //  RBM functions

  // TODO: If (iRep==1 for row Vector) or (j==1 for col Vector), does the result have to be a Vector instead of a Matrix?
	/* REPMAT1D : Replication of the input vector(jRep times) to generate larger matrix */
	case class OP_REPMAT(v: IntVector, iRep: Int, jRep: Int) extends DeliteOP_SingleTask[Matrix[Int]](v) {
    def task = {
	  	val length = v.length
		  var i = 0
      var j = 0
		  var ii = 0
		  var jj = 0
      
		  if(v.is_row) { // when input is a row vector
        val out = new IntMatrixImpl(iRep, jRep*v.length)
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
        val out = new IntMatrixImpl(iRep*v.length, jRep)
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
  protected[delite] case class OP_mapLR[B : ClassManifest](val v: IntVector, val func: Int => B, val x_cur:Int, val tau:Int)
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
      val elms = new Array[Int](0)
      val gout = Vector[Int](elms, v.is_row, v.length)
      gout.cvalue = gout.asInstanceOf[gout.DSLType]
      gout.isComputed = true
      gout
    }
	  *///override def getGPUKernelId = {List(DeliteCuda.MAP_LR, DeliteCuda.AsyncMAPLR)}
  }
  */

  

}


trait IntVector extends Vector[Int] {
  import IntVector._

  protected[optiml] var _data: Array[Int]

  def apply(n: Int) : Int
  override def lifted_apply(n: Int) : Int  = {
    run(OP_apply(this,n))
  }


  def update(index: Int, x: Int) : Unit
  override def lifted_update(index: Int, x: Int) : Unit = {
    run(OP_update(this,index,x))
  }

  override def dc_update(i: Int, x: Int)
  override def dc_apply(i: Int) : Int 

  //////////
  // arith

  // gda, kmeans, nb ops
  override def trans(implicit c: ClassManifest[Int]) : Vector[Int] = {
    if(Config.executorType == "gpu")
      run(OPGPU_trans(this))
    else
      run(OP_trans(this))
  }

  override def +(v: Vector[Int])(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Vector[Int] = {
    //run(OP_+(this, v.asInstanceOf[IntVector], run(OP_alloc[Int](this)).asInstanceOf[IntVector]))
    if(Config.executorType == "gpu")
      run(OPGPU_plus_single(this,v.asInstanceOf[IntVector]))
    else
      run(OP_+(this, v.asInstanceOf[IntVector], run(OP_alloc[Int](this)).asInstanceOf[IntVector]))
      //run(OP_plus_single(this,v.asInstanceOf[IntVector]))
  }

  override def outer(v: Vector[Int])(implicit ops: ArithOps[Int], pFact: Matrix.ProxyFactory[Int], c: ClassManifest[Int]) : Matrix[Int] = {
    if(Config.executorType == "gpu")
      run(OPGPU_outer_single(this,v.asInstanceOf[IntVector]))
    else
      run(OP_outer_single(this,v.asInstanceOf[IntVector]))
  }

  override def -(v: Vector[Int])(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Vector[Int] = {
    //run(OP_-(this, v.asInstanceOf[IntVector], run(OP_alloc[Int](this)).asInstanceOf[IntVector]))
    if(Config.executorType == "gpu")
      run(OPGPU_minus_single(this,v.asInstanceOf[IntVector]))
    else
     run(OP_-(this, v.asInstanceOf[IntVector], run(OP_alloc[Int](this)).asInstanceOf[IntVector]))
     //run(OP_minus_single(this,v.asInstanceOf[IntVector]))
  }

  override def dot[B <: DeliteDSLType](v: Vector[Int])(implicit ops: ArithOps[Int], conv: Int => B, pfact: DeliteProxyFactory[B]) : B = {
    run(OP_dot_single(this, v.asInstanceOf[IntVector], conv))(pfact)
  }

  // others
  override def +(x: Int)(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Vector[Int] = {
    if(Config.executorType == "gpu")
      run(OPGPU_IntPlus(this, x))
    else
      run(OP_mapInt(this, run(OP_alloc[Int](this)).asInstanceOf[IntVector], ele => ele+x))
  }

  override def -(x: Int)(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Vector[Int] = {
    run(OP_mapInt(this, run(OP_alloc[Int](this)).asInstanceOf[IntVector], ele => ele-x))
  }

  override def /(v: Vector[Int])(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Vector[Int] = {
    run(OP_/(this, v.asInstanceOf[IntVector], run(OP_alloc[Int](this)).asInstanceOf[IntVector]))
  }
  override def /(x: Int)(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Vector[Int] = {
    if(Config.executorType == "gpu")
      run(OPGPU_IntDiv(this, x))
    else
      run(OP_mapInt(this, run(OP_alloc[Int](this)).asInstanceOf[IntVector], ele => ele/x))
  }

  override def *(m: Matrix[Int])(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Vector[Int] = {
    run(OP_mtimes(this,m.asInstanceOf[IntMatrix]))
  }

  override def *(v: Vector[Int])(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Vector[Int] = {
    if(Config.executorType == "gpu")
      run(OPGPU_*(this, v.asInstanceOf[IntVector]))
    else {
      //if (Config.useNativeLibs) 
      //  run(OP_native_*(this, v.asInstanceOf[IntVector]))
      //else 
        run(OP_*(this, v.asInstanceOf[IntVector], run(OP_alloc[Int](this)).asInstanceOf[IntVector]))
    }
  }

  override def *(x: Int)(implicit ops: ArithOps[Int], c: ClassManifest[Int]) : Vector[Int] = {
    if(Config.executorType == "gpu")
      run(OPGPU_IntMult(this, x))
    else
      run(OP_mapInt(this, run(OP_alloc[Int](this)).asInstanceOf[IntVector], ele => ele*x))
  }

  override def sum[B <: DeliteDSLType](implicit pfact: DeliteProxyFactory[B], ops: ArithOps[Int], conv: Int => B): B = {
    if(Config.executorType == "gpu")
      run(OPGPU_sum(this)(conv))
    else
      run(OP_sum(this)(conv))
  }

  override def abs(implicit ops: ArithOps[Int], pfact: DeliteProxyFactory[Vector[Int]], c: ClassManifest[Int]) : Vector[Int] = {
    run(OP_abs(this, run(OP_alloc[Int](this))(pfact).asInstanceOf[IntVector]))(pfact)
  }

  override def exp(implicit ops: ArithOps[Int], pfact: DeliteProxyFactory[Vector[Int]], c: ClassManifest[Int]) : Vector[Int] = {
    if(Config.executorType == "gpu")
      run(OPGPU_exp(this))(pfact)
    else
      run(OP_exp(this, run(OP_alloc[Int](this))(pfact).asInstanceOf[IntVector]))(pfact)
  }

  
  /////////////////////////////
  // stuff requiring ordering

  override def min[B <: DeliteDSLType](implicit cmp: Int => Ordered[Int], conv: Int => B, pfact: DeliteProxyFactory[B]): B = {
    run(OP_min(this))
  }

  override def minIndex(implicit cmp: Int => Ordered[Int]): DeliteInt = {
    run(OP_minIndex(this))
  }

  override def max[B <: DeliteDSLType](implicit cmp: Int => Ordered[Int], conv: Int => B, pfact: DeliteProxyFactory[B]): B = {
    run(OP_max(this))
  }

  //////////////////
  // bulk operations

  override def map[B](f: Int => B)(implicit pFact: DeliteProxyFactory[Vector[B]], c: ClassManifest[B]) : Vector[B] = {
    run[Vector[B]](OP_map[B](this, run(OP_alloc[B](this)), f))
  }

  override def reduce[B <: DeliteDSLType](func: (Int,Int) => Int)(implicit conv: Int => B, pfact: DeliteProxyFactory[B]): B = {
    run(OP_reduce(this, func)(conv))
  }


  ///////////////////////////
  // GPU Specific Operations

  // Map operation for Linear Regression
  /*
  override def mapLR[B](f: Int => B)(x_cur:Int, tau:Int)(implicit pFact: DeliteProxyFactory[Vector[B]], c: ClassManifest[B]) : Vector[B] = {
    if(Config.executorType == "gpu")
      run[Vector[B]](OP_mapLR[B](this, f, x_cur, tau))
    else
      run[Vector[B]](OP_map[B](this, run(OP_alloc[B](this)), f))
  }
  */
  
  ////////////////////
  // RBM oeprations

  // RBM : operation that replicates the given matrix to generate a larger matrix
  override def repmat(i: Int, j: Int): Matrix[Int] = {
    if(Config.executorType == "gpu")
      run(OPGPU_REPMAT(this, i, j))
    else
      run(OP_REPMAT(this, i, j))
  }
  
}
