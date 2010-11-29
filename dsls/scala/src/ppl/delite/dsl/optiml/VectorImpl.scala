/* array-backed, double precision floating point Vector
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * modified: 4/14/09
 * credits:  Nathan Bronson (from PBuffer.scala, HeapDoublePBuffer.scala)
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

package ppl.delite.dsl.optiml

import ppl.delite.core.include._
import ppl.delite.core.appinclude._
import ppl.delite.core.DeliteCore
import ppl.delite.core.ops.DeliteOP_MutableSingleTask

object VectorImpl {
  protected[optiml] case class OP_plusEquals[T, A <: T](v: VectorImpl[T], x: A)
    extends DeliteOP_MutableSingleTask[Vector[T]]()(v){
    
    def task = {
      v.ensureExtra(1)
      v._data(v._length) = x
      v._length += 1
      v
    }
  }

  protected[optiml] case class OP_copyFrom[T, A <: T](v: VectorImpl[T], pos: Int, xs: Vector[A])
    extends DeliteOP_MutableSingleTask[Vector[T]](xs)(v) {

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

  protected[optiml] case class OP_insert[T, A <: T](v: VectorImpl[T], pos: Int, x: A)
    extends DeliteOP_MutableSingleTask[Vector[T]]()(v) {

    def task = {
      v.insertSpace(pos, 1)
      v._data(pos) = x
      v
    }
  }

  protected[optiml] case class OP_insertAll[T, A <: T](v: VectorImpl[T], pos: Int, xs: Vector[A])
    extends DeliteOP_MutableSingleTask[Vector[T]](xs)(v) {
    
    def task = {
      v.insertSpace(pos, xs._length)
      v.copyFrom(pos, xs)
    }
  }

  protected[optiml] case class OP_removeAll[T : ClassManifest](v: VectorImpl[T], pos: Int, len: Int)
    extends DeliteOP_MutableSingleTask[Vector[T]]()(v) {

    def task = {
      v.chkUpdate
      v.chkRange(pos, pos + len)
      v._length -= len
      val out_data = new Array[T](v._length)
      Array.copy(v._data, 0, out_data, 0, pos)
      Array.copy(v._data, pos+len, out_data, pos, v._length-pos)
      v._data = out_data
      v
    }
  }

  protected[optiml] case class OP_trim[T : ClassManifest](v: VectorImpl[T])
    extends DeliteOP_MutableSingleTask[Vector[T]]()(v) {

    def task = {
      v.chkUpdate
      if (v._length < v._data.length) {
        val d = new Array[T](v._length)
        Array.copy(v._data, 0, d, 0, v._length)
        v._data = d
      }
      v
    }

  }  
}

private class VectorImpl[T: ClassManifest] extends Vector[T] {
  import VectorImpl._

  type DSLType = VectorImpl[T]

  def this(row_vec: Boolean, len: Int) = {
    this()
    init(row_vec, len)
    isComputed = true
  }
  
  def init(row_vec: Boolean, len: Int) {
    _data = new Array[T](len)
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

  protected var _data: Array[T] = null
  override var _length = -1
  override var _is_row = false
  override protected var _frozen = false

  def length = force._length
  def is_row = force._is_row
  def frozen = force._frozen
  
  override def dc_apply(i: Int) = apply(i)
  override def dc_update(i: Int, x: T) = update(i,x)

  //////////////
  // life cycle

  def mutableClone = new VectorImpl[T](is_row, 0) ++= this

  def view(start: Int, stride: Int, length: Int, is_row: Boolean) = new VectorViewImpl[T](_data, start, stride, length, is_row)

  ////////////
  // data ops

  // TODO: how do we lift apply/update for generic collections?
  def lifted_apply(n: Int) = apply(n)
  def apply(n: Int) : T = {
    //_data(chkIndex(n))
    _data(n)
  }

  def indices : Vector[Int] = {
    return Vector.range(0, length, is_row = _is_row )
  }

  def lifted_update(index: Int, x: T) = update(index, x)
  def update(index: Int, x: T) {
    //run(OP_update(this,index,x))
    //chkUpdate
    //_data(chkIndex(index)) = x
    _data(index) = x
  }

  /*
  def +=[A <: T](x: A): Vector[T] = {
    ensureExtra(1)
    _data(_length) = x
    _length += 1
    this
  }

  def copyFrom[A <: T](pos: Int, xs: Vector[A]): Vector[T] = {
    chkUpdate
    chkRange(pos, pos + xs.length)
    xs match {      
      case _ => {
        var i = 0
        while (i < xs.length) {
          _data(pos + i) = xs(i)
          i += 1
        }
      }
    }
    this
  }

  def insert[A <: T](pos: Int, x: A): Vector[T] = {
    insertSpace(pos, 1)
    _data(pos) = x
    this
  }

  def insertAll[A <: T](pos: Int, xs: Vector[A]): Vector[T] = {
    insertSpace(pos, xs.length)
    copyFrom(pos, xs)
  }

  def removeAll(pos: Int, len: Int): Vector[T] = {
    chkUpdate
    chkRange(pos, pos + len)
    Array.copy(_data, pos + len, _data, pos, _length - (pos + len))
    _length -= len
    this
  }

  def trim: Vector[T] = {
    chkUpdate
    if (_length < _data.length) {
      val d = new Array[T](_length)
      Array.copy(_data, 0, d, 0, _length)
      _data = d
    }
    this
  }
  */

  def +=[A <: T](x: A): Vector[T] = {
    run(OP_plusEquals(this,x))
  }

  def copyFrom[A <: T](pos: Int, xs: Vector[A]): Vector[T] = {
    run(OP_copyFrom(this,pos,xs))
  }

  def insert[A <: T](pos: Int, x: A): Vector[T] = {
    run(OP_insert(this,pos,x))
  }

  def insertAll[A <: T](pos: Int, xs: Vector[A]): Vector[T] = {
    run(OP_insertAll(this,pos,xs))
  }

  def removeAll(pos: Int, len: Int): Vector[T] = {
    run(OP_removeAll(this,pos,len))
  }

  def trim: Vector[T] = {
    run(OP_trim(this))
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
    val d = new Array[T](n)
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
