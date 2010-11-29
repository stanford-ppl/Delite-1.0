package ppl.delite.dsl.optiml

import ppl.delite.dsl.optiml.specialized.IntVectorImpl

/* A read-only RangeVector that does not allocate any data.
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: Jul 12, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

class RangeVector extends IntVectorImpl {
  protected var _start = 0
  protected var _stride = 1

  _frozen = true
  
  def this(start: Int, end: Int, stride: Int, row_vec: Boolean){
    this()
    _start = start
    _stride = stride
    _length = (end-start + stride - 1) / stride // Math.ceil(end-start / stride)
    _is_row = row_vec
    isComputed = true
    cvalue = this
  }

  def this(row_vec: Boolean, len: Int){
    this()
    _length = len
    _is_row = row_vec
    isComputed = true
    cvalue = this
  }

  override def apply(n: Int) : Int = {
    //_start + chkIndex(n)*_stride
    _start + n*_stride
  }


  // we only need to override these because we aren't currently doing the check for frozen for efficiency reasons

  // TODO: could make this a lazy initialization and allow updates,
  //       but update would be slow due to the check
  override def update(index: Int, x: Int) {
    throw new IllegalArgumentException("RangeVector cannot be updated")
  }

  override def +=[A <: Int](x: A): Vector[Int] = {
    throw new IllegalArgumentException("RangeVector cannot be updated")
  }

  override def copyFrom[A <: Int](pos: Int, xs: Vector[A]): Vector[Int] = {
    throw new IllegalArgumentException("RangeVector cannot be updated")
  }

  override def insert[A <: Int](pos: Int, x: A): Vector[Int] = {
    throw new IllegalArgumentException("RangeVector cannot be updated")
  }

  override def insertAll[A <: Int](pos: Int, xs: Vector[A]): Vector[Int] = {
    throw new IllegalArgumentException("RangeVector cannot be updated")
  }

  override def removeAll(pos: Int, len: Int): Vector[Int] = {
    throw new IllegalArgumentException("RangeVector cannot be updated")
  }

  override def trim: Vector[Int] = {
    throw new IllegalArgumentException("RangeVector cannot be updated")
  }

}