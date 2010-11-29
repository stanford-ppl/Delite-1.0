/* T matrix datatype
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: 5/8/09
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

package ppl.delite.dsl.optiml

import ppl.delite.core.appinclude._
import ppl.delite.core.include._
import ppl.delite.core.ops.{DeliteOP_MutableSingleTask, DeliteOP_SingleTask}

object MatrixImpl {
  protected[optiml] case class OP_clone[T](m: MatrixImpl[T]) extends DeliteOP_SingleTask[Matrix[T]](m) {
    def task = {
      val ret = m.mutableClone
      if (m.frozen) ret.freeze()
      ret
    }
  }

  protected[optiml] case class OP_mutableClone[T : ClassManifest](m: MatrixImpl[T]) extends DeliteOP_SingleTask[Matrix[T]](m) {
    def task = {
      val res = new MatrixImpl[T](m._numRows, m._numCols)
      for (i <- 0 until m.size){
        res.dc_update(i, m._data(i))
      }
      res
    }
  }

  protected[optiml] case class OP_vview[T : ClassManifest](m: MatrixImpl[T], start: Int, stride: Int, length: Int, is_row: Boolean)
    extends DeliteOP_SingleTask[Vector[T]] {

    def task = {
      val vv = new VectorViewImpl[T](m._data, start, stride, length, is_row)
      if (m.frozen) vv.freeze()
      vv
    }
  }

  protected[optiml] case class OP_insertRow[T, A <: T](m: MatrixImpl[T], pos: Int, x: Vector[A])
    extends DeliteOP_MutableSingleTask[Matrix[T]](x)(m) {

    def task = {
      m.chkUpdate
      m.chkEquals(x.length, m._numCols)
      val idx = pos*m._numCols
      m.insertSpace(idx, m._numCols)
      for (i <- idx until idx+m._numCols){
        m._data(i) = x(i-idx)
      }
      m._numRows += 1
      m
    }
  }

  protected[optiml] case class OP_insertAllRows[T, A <: T](m: MatrixImpl[T], pos: Int, xs: Matrix[A])
    extends DeliteOP_MutableSingleTask[Matrix[T]](xs)(m) {

    def task = {
      m.chkUpdate
      m.chkEquals(xs._numCols, m._numCols)
      val idx = pos*m._numCols
      val sz = m._numCols*xs._numRows
      m.insertSpace(idx, sz)
      for (i <- idx until idx+sz){
        m._data(i) = xs.flattened(i-idx)
      }
      m._numRows += xs._numRows
      m
    }
  }

  protected[optiml] case class OP_insertCol[T, A <: T](m: MatrixImpl[T], pos: Int, x: Vector[A])(implicit c: ClassManifest[T])
    extends DeliteOP_MutableSingleTask[Matrix[T]](x)(m) {

    def task = {
      m.chkUpdate
      m.chkEquals(x._length, m._numRows)

      val newCols = m._numCols+1
      val out_data = new Array[T](m._numRows*newCols)
      for (i <- 0 until m._numRows){
        var col = 0
        for (j <- 0 until newCols) {
          if (j == pos){
            out_data(i*newCols+j) = x(i)
          }
          else{
            out_data(i*newCols+j) = m(i,col)
            col += 1
          }
        }
      }
      m._data = out_data
      m._numCols += 1
      m
    }
  }

  protected[optiml] case class OP_insertAllCols[T, A <: T](m: MatrixImpl[T], pos: Int, xs: Matrix[A])(implicit c: ClassManifest[T])
    extends DeliteOP_MutableSingleTask[Matrix[T]](xs)(m) {

    def task = {
      m.chkUpdate
      m.chkEquals(xs._numRows, m._numRows)

      val newCols = m._numCols+xs._numCols
      val out_data = new Array[T](m._numRows*newCols)
      for (i <- 0 until m._numRows){
        var col = 0
        for (j <- 0 until newCols){
          if (j < pos || j >= pos+xs._numCols){
            out_data(i*newCols+j) = m(i,col)
            col += 1
          }
          else{
            out_data(i*newCols+j) = xs(i,j-pos)
          }
        }
      }
      m._data = out_data

      m._numCols += xs._numCols
      m
    }
  }

  protected[optiml] case class OP_removeRows[T](m: MatrixImpl[T], pos: Int, num: Int)
    extends DeliteOP_MutableSingleTask[Matrix[T]]()(m) {

    def task = {
      m.chkUpdate
      val idx = pos*m._numCols
      val len = num*m._numCols
      Array.copy(m._data, idx + len, m._data, idx, m.size - (idx + len))
      m._numRows -= num
      m
    }
  }

  protected[optiml] case class OP_removeCols[T : ClassManifest](m: MatrixImpl[T], pos:Int, num: Int)
    extends DeliteOP_MutableSingleTask[Matrix[T]]()(m) {

    def task = {
      m.chkUpdate

      val newCols = m._numCols-num
      val out_data = new Array[T](m._numRows*newCols)
      for (i <- 0 until m._numRows){
        var col = 0
        for (j <- 0 until m._numCols){
          if (j < pos || j >= pos+num){
            out_data(i*newCols+col) = m(i,j)
            col += 1
          }
        }
      }
      m._data = out_data
      m._numCols -= num
      m
    }
  }
}

private class MatrixImpl[T: ClassManifest] extends Matrix[T] {
  import MatrixImpl._
  
  type DSLType = MatrixImpl[T]

  def this(nRows: Int, nCols: Int) = {
    this()
    init(nRows, nCols)
    isComputed = true
  }

  def init(nRows: Int, nCols: Int) {
    _data = new Array[T](nRows*nCols)
    _numRows = nRows
    _numCols = nCols
    cvalue = this
  }

  override def concretize {
    _data = cvalue._data
    _numRows = cvalue._numRows
    _numCols = cvalue._numCols
    cvalue = this
  }

  def numRows = force._numRows
  def numCols = force._numCols
  def frozen = force._frozen

  protected var _data: Array[T] = null
  override var _numRows = -1
  override var _numCols = 1
  override protected var _frozen = false

  //////////////////////
  // delite collection

  def dc_update(i: Int, x: T) = {
    _data(i) = x
  }
  def dc_apply(i: Int) : T = {
    _data(i)
  }


  //////////////////////
  // public

  // TODO: how do we lift apply/update for generic collections?
  def lifted_apply(i: Int, j: Int) = apply(i,j)
  def apply(i: Int, j: Int) : T = {
    _data(chkPos(i*numCols+j))
  }

  def lifted_update(row: Int, col: Int, x: T) = update(row, col, x)
  def update(row: Int, col: Int, x: T) = {
    chkUpdate
    _data(chkPos(row*numCols+col)) = x
  }
  
  override def clone: Matrix[T] = {
    run(OP_clone(this))
  }

  def mutableClone: Matrix[T] = {
    run(OP_mutableClone(this))
  }

  def vview(start: Int, stride: Int, length: Int, is_row: Boolean) : Vector[T] = {
    run(OP_vview(this,start,stride,length,is_row))(new Vector.ViewProxyFactory)
  }

  def insertRow[A <: T](pos: Int, x: Vector[A]): Matrix[T] = {
    run(OP_insertRow(this,pos,x))
  }

  def insertAllRows[A <: T](pos: Int, xs: Matrix[A]): Matrix[T] = {
    run(OP_insertAllRows(this,pos,xs))
  }

  def insertCol[A <: T](pos: Int, x: Vector[A]): Matrix[T] = {
    run(OP_insertCol(this,pos,x))
  }

  def insertAllCols[A <: T](pos: Int, xs: Matrix[A]): Matrix[T] = {
    run(OP_insertAllCols(this,pos,xs))
  }

  def removeRows(pos: Int, num: Int): Matrix[T] = {
    run(OP_removeRows(this,pos,num))
  }

  def removeCols(pos:Int, num: Int): Matrix[T] = {
    run(OP_removeCols(this,pos,num))
  }

  protected def ensureExtra(extra: Int) {
    chkUpdate
    if (_data.length - size < extra) {
      realloc(size + extra)
    }
  }

  protected def realloc(minLen: Int) {
    var n = 4 max (_data.length * 2)
    while (n < minLen) n *= 2
    val d = new Array[T](n)
    Array.copy(_data, 0, d, 0, size)
    _data = d
  }

  protected def insertSpace(pos: Int, len: Int) {
    chkPos(pos)
    ensureExtra(len)
    Array.copy(_data, pos, _data, pos + len, size - pos)
  }

  protected def chkPos(index: Int) = {
    if (index < 0 || index >= size) throw new IndexOutOfBoundsException
    index
  }
}

