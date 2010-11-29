/* array-backed, Long precision floating point Vector
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * modified: 5/11/10
 * credits:  Nathan Bronson (from PBuffer.scala, HeapLongPBuffer.scala)
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

package ppl.delite.dsl.optiml.specialized

import ppl.delite.core.include._
import ppl.delite.core.appinclude._
import ppl.delite.nativeGPU.GPUable
import ppl.delite.dsl.optiml.{Matrix, ArithOps, Vector}
import ppl.delite.core.ops.{DeliteOP_MutableSingleTask, DeliteOP_SingleTask}
import ppl.delite.core.DeliteCore

object LongVectorImpl {
  protected[optiml] case class OP_plusEquals[A <: Long](v: LongVectorImpl, x: A)
    extends DeliteOP_MutableSingleTask[Vector[Long]]()(v){

    def task = {
      v.ensureExtra(1)
      v._data(v._length) = x
      v._length += 1
      v
    }
  }

  protected[optiml] case class OP_copyFrom[A <: Long](v: LongVectorImpl, pos: Int, xs: Vector[A])
    extends DeliteOP_MutableSingleTask[Vector[Long]](xs)(v) {

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

  protected[optiml] case class OP_insert[A <: Long](v: LongVectorImpl, pos: Int, x: A)
    extends DeliteOP_MutableSingleTask[Vector[Long]]()(v) {

    def task = {
      v.insertSpace(pos, 1)
      v._data(pos) = x
      v
    }
  }

  protected[optiml] case class OP_insertAll[A <: Long](v: LongVectorImpl, pos: Int, xs: Vector[A])
    extends DeliteOP_MutableSingleTask[Vector[Long]](xs)(v) {
    
    def task = {
      v.insertSpace(pos, xs._length)
      v.copyFrom(pos, xs)
    }
  }

  protected[optiml] case class OP_removeAll(v: LongVectorImpl, pos: Int, len: Int)
    extends DeliteOP_MutableSingleTask[Vector[Long]]()(v) {

    def task = {
      v.chkUpdate
      v.chkRange(pos, pos + len)
      v._length -= len
      val out_data = new Array[Long](v._length)
      Array.copy(v._data, 0, out_data, 0, pos)
      Array.copy(v._data, pos+len, out_data, pos, v._length-pos)
      v._data = out_data
      v
    }
  }

  protected[optiml] case class OP_trim(v: LongVectorImpl) extends DeliteOP_MutableSingleTask[Vector[Long]]()(v) {

    def task = {
      v.chkUpdate
      if (v._length < v._data.length) {
        val d = new Array[Long](v._length)
        Array.copy(v._data, 0, d, 0, v._length)
        v._data = d
      }
      v
    }

  }
}

private[optiml] class LongVectorImpl extends LongVector with GPUable[Long] {
  import LongVectorImpl._

  type DSLType = LongVectorImpl

  def this(row_vec: Boolean, len: Int) = {
    this()
    init(row_vec, len)
    isComputed = true
  }

  /* Generate a LongVectorImpl with given array and length (not necessarily consistent) */
  def this(data: Array[Long], row_vec: Boolean, len: Int) = {
	  this()
	  _data = data
	  _length = len
	  _is_row = row_vec
    isComputed = true
  }

  def init(row_vec: Boolean, len: Int) = {
    _data = new Array[Long](len)
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

  protected[optiml] var _data: Array[Long] = null
  override var _length = -1
  override var _is_row = false
  override protected var _frozen = false

  //////////////
  // GPUable
  protected[delite] def gpu_data = _data
  protected[delite] def gpu_datasize = _length
  protected[delite] def gpu_setdata(elms: Array[Long]) = { _data = elms }
  protected[delite] def gpu_apply(n: Int) = apply(n)
  protected[delite] def gpu_update(n: Int, x: Long) = update(n,x)

  ////////////////////
  // DeliteCollection

  override def dc_apply(i: Int) = apply(i)
  override def dc_update(i: Int, x: Long) = update(i,x)

  //////////////
  // life cycle

  def mutableClone = new LongVectorImpl(is_row, 0) ++= this

  def view(start: Int, stride: Int, length: Int, is_row: Boolean) = new LongVectorViewImpl(_data, start, stride, length, is_row)

  ////////////
  // data ops

  def apply(n: Int) : Long = {
    //_data(chkIndex(n))
    _data(n)
  }
  
  def indices : Vector[Int] = {
    Vector.range(0, length, is_row = _is_row)
  }

  def update(index: Int, x: Long) {
    //run(OP_update(this,index,x))
    //chkUpdate
    //_data(chkIndex(index)) = x
    _data(index) = x
  }

  def +=[A <: Long](x: A): Vector[Long] = {
    run(OP_plusEquals(this,x))(longVecProxyFactory)
  }

  def copyFrom[A <: Long](pos: Int, xs: Vector[A]): Vector[Long] = {
    run(OP_copyFrom(this,pos,xs))(longVecProxyFactory)
  }

  def insert[A <: Long](pos: Int, x: A): Vector[Long] = {
    run(OP_insert(this,pos,x))(longVecProxyFactory)
  }

  def insertAll[A <: Long](pos: Int, xs: Vector[A]): Vector[Long] = {
    run(OP_insertAll(this,pos,xs))(longVecProxyFactory)    
  }

  def removeAll(pos: Int, len: Int): Vector[Long] = {
    run(OP_removeAll(this,pos,len))(longVecProxyFactory)
  }

  def trim: Vector[Long] = {
    run(OP_trim(this))(longVecProxyFactory)
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
    val d = new Array[Long](n)
    Array.copy(_data, 0, d, 0, _length)
    _data = d
  }

  protected def insertSpace(pos: Int, len: Int) {
    chkPos(pos)
    ensureExtra(len)
    Array.copy(_data, pos, _data, pos + len, _length - pos)
    _length += len
  }

}
