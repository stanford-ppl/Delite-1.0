/* array-backed, Double precision floating point Vector
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * modified: 5/11/10
 * credits:  Nathan Bronson (from PBuffer.scala, HeapDoublePBuffer.scala)
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

package ppl.delite.dsl.optiml.specialized

import ppl.delite.core.include._
import ppl.delite.core.appinclude._
import ppl.delite.nativeGPU.GPUable
import ppl.delite.dsl.optiml.{Matrix, ArithOps, Vector}
import ppl.delite.core.ops.{DeliteOP_Map, DeliteOP_MutableSingleTask, DeliteOP_SingleTask}
import ppl.delite.core.{DeliteProxyFactory, DeliteCore}

object DoubleVectorImpl {
  protected[optiml] case class OP_plusEquals[A <: Double](v: DoubleVectorImpl, x: A)
    extends DeliteOP_MutableSingleTask[Vector[Double]]()(v){

    def task = {
      v.ensureExtra(1)
      v._data(v._length) = x
      v._length += 1
      v
    }
  }

  protected[optiml] case class OP_copyFrom[A <: Double](v: DoubleVectorImpl, pos: Int, xs: Vector[A])
    extends DeliteOP_MutableSingleTask[Vector[Double]](xs)(v) {

    def task = {
      v.chkUpdate
      v.chkRange(pos, pos + xs._length)
      xs match {
        case _ => {
          var i = 0
          while (i < xs._length) {
            v._data(pos + i) = xs(i)
            i += 1
          }
        }
      }
      v
    }
  }

  protected[optiml] case class OP_insert[A <: Double](v: DoubleVectorImpl, pos: Int, x: A)
    extends DeliteOP_MutableSingleTask[Vector[Double]]()(v) {

    def task = {
      v.insertSpace(pos, 1)
      v._data(pos) = x
      v
    }
  }

  protected[optiml] case class OP_insertAll[A <: Double](v: DoubleVectorImpl, pos: Int, xs: Vector[A])
    extends DeliteOP_MutableSingleTask[Vector[Double]](xs)(v) {
    
    def task = {
      v.insertSpace(pos, xs._length)
      v.copyFrom(pos, xs)
    }
  }

  protected[optiml] case class OP_removeAll(v: DoubleVectorImpl, pos: Int, len: Int)
    extends DeliteOP_MutableSingleTask[Vector[Double]]()(v) {

    def task = {
      v.chkUpdate
      v.chkRange(pos, pos + len)
      v._length -= len
      val out_data = new Array[Double](v._length)
      Array.copy(v._data, 0, out_data, 0, pos)
      Array.copy(v._data, pos+len, out_data, pos, v._length-pos)
      v._data = out_data
      v
    }
  }

  protected[optiml] case class OP_trim(v: DoubleVectorImpl) extends DeliteOP_MutableSingleTask[Vector[Double]]()(v) {

    def task = {
      v.chkUpdate
      if (v._length < v._data.length) {
        val d = new Array[Double](v._length)
        Array.copy(v._data, 0, d, 0, v._length)
        v._data = d
      }
      v
    }

  }

  protected[optiml] case class OP_map[B : ClassManifest](val coll: Vector[Double], val out: Vector[B], val func: Double => B)
    extends DeliteOP_Map[Double,B,Vector]

  protected[optiml] case class OP_alloc[B: ClassManifest](orig: Vector[Double]) extends DeliteOP_SingleTask[Vector[B]](orig) {
    def task = {
      Vector[B](orig.is_row, orig.length)
    }
  }

}

private[optiml] class DoubleVectorImpl extends DoubleVector with GPUable[Double] {
  import DoubleVectorImpl._

  type DSLType = DoubleVectorImpl

  def this(row_vec: Boolean, len: Int) = {
    this()
    init(row_vec, len)
    isComputed = true
  }

  /* Generate a DoubleVectorImpl with given array and length (not necessarily consistent) */
  def this(data: Array[Double], row_vec: Boolean, len: Int) = {
	  this()
	  _data = data
	  _length = len
	  _is_row = row_vec
    cvalue = this
    isComputed = true
  }

  def init(row_vec: Boolean, len: Int) = {
    _data = new Array[Double](len)
    _length = len
    _is_row = row_vec
    cvalue = this
  }

  override def concretize {
    _data = cvalue._data
    _length = cvalue._length
    _is_row = cvalue._is_row
    cvalue = this    
  }

  def length = force._length
  def is_row = force._is_row
  def frozen = force._frozen

  protected[optiml] var _data: Array[Double] = null
  override var _length = -1
  override var _is_row = false
  override protected var _frozen = false

  // for streaming use
  var _matrix: Matrix[Double] = null
  var _index: Int = -1
  var _is_streaming = false
  var _is_initialized = false

  override def matrix = _matrix
  override def index = _index
  
  //////////////
  // GPUable
  protected[delite] def gpu_data = _data
  protected[delite] def gpu_datasize = _length
  protected[delite] def gpu_setdata(elms: Array[Double]) = { _data = elms }
  protected[delite] def gpu_apply(n: Int) = apply(n)
  protected[delite] def gpu_update(n: Int, x: Double) = update(n,x)

  ////////////////////
  // DeliteCollection

  override def dc_apply(i: Int) = apply(i)
  override def dc_update(i: Int, x: Double) = update(i,x)

  //////////////
  // life cycle

  def mutableClone = new DoubleVectorImpl(is_row, 0) ++= this

  def view(start: Int, stride: Int, length: Int, is_row: Boolean) = new DoubleVectorViewImpl(_data, start, stride, length, is_row)

  ////////////
  // data ops

  def apply(n: Int) : Double = {
    //_data(chkIndex(n))
    _data(n)
  }
  
  def indices : Vector[Int] = {
    Vector.range(0, length, is_row = _is_row)
  }

  def update(index: Int, x: Double) {
    //run(OP_update(this,index,x))
    //chkUpdate
    //_data(chkIndex(index)) = x
    _data(index) = x
  }

  def +=[A <: Double](x: A): Vector[Double] = {
    run(OP_plusEquals(this,x))(doubleVecProxyFactory)
  }

  def copyFrom[A <: Double](pos: Int, xs: Vector[A]): Vector[Double] = {
    run(OP_copyFrom(this,pos,xs))(doubleVecProxyFactory)
  }

  def insert[A <: Double](pos: Int, x: A): Vector[Double] = {
    run(OP_insert(this,pos,x))(doubleVecProxyFactory)
  }

  def insertAll[A <: Double](pos: Int, xs: Vector[A]): Vector[Double] = {
    run(OP_insertAll(this,pos,xs))(doubleVecProxyFactory)    
  }

  def removeAll(pos: Int, len: Int): Vector[Double] = {
    run(OP_removeAll(this,pos,len))(doubleVecProxyFactory)
  }

  def trim: Vector[Double] = {
    run(OP_trim(this))(doubleVecProxyFactory)
  }

  protected def ensureExtra(extra: Int) {
    chkUpdate
    if (_data.length - _length < extra) {
      realloc(_length + extra)
    }
  }

  protected def realloc(minLen: Int) {
    var n = 4 max (_data.length * 2)
    while (n < minLen) n *= 2
    val d = new Array[Double](n)
    Array.copy(_data, 0, d, 0, _length)
    _data = d
  }

  protected def insertSpace(pos: Int, len: Int) {
    chkPos(pos)
    ensureExtra(len)
    Array.copy(_data, pos, _data, pos + len, _length - pos)
    _length += len
  }

  protected def initialize() {
    _matrix.initRow(_index)
    _is_initialized = true
    // println("initialize row "+_index)
  }
  
  override def map[B](f: Double => B)(implicit pFact: DeliteProxyFactory[Vector[B]], c: ClassManifest[B]): Vector[B] = {
    if(_is_streaming && !_is_initialized)  initialize
    run(OP_map[B](this, run(OP_alloc[B](this)), f))
  }

  override def find(pred:Double => Boolean):Vector[Int] = {
    if(_is_streaming && !_is_initialized)  initialize
    val len = length
    var out = Vector[Int](0)
    var i = 0
    while(i < len){
      if(pred(this.apply(i))) out += i
      i += 1
    }
    out
  }

  override def index_= (i: Int){
    _index = i
  }

  override def data = this._data

}
