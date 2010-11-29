/* Int matrix datatype
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: 5/8/09
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

package ppl.delite.dsl.optiml.specialized

import ppl.delite.core.appinclude._
import ppl.delite.core.include._
import ppl.delite.nativeGPU.GPUable
import ppl.delite.dsl.optiml.{ArithOps, Vector, VectorView, Matrix}
import ppl.delite.core.ops.{DeliteOP_MutableSingleTask, DeliteOP_SingleTask}

object IntMatrixImpl {
  protected[optiml] case class OP_clone(m: IntMatrixImpl) extends DeliteOP_SingleTask[Matrix[Int]](m) {
    def task = {
      val ret = m.mutableClone
      if (m.frozen) ret.freeze()
      ret
    }
  }

  protected[optiml] case class OP_mutableClone(m: IntMatrixImpl) extends DeliteOP_SingleTask[Matrix[Int]](m) {
    def task = {
      val res = new IntMatrixImpl(m._numRows, m._numCols)
      for (i <- 0 until m.size){
        res.dc_update(i, m._data(i))
      }
      res
    }
  }

  protected[optiml] case class OP_vview(m: IntMatrixImpl, start: Int, stride: Int, length: Int, is_row: Boolean)
    extends DeliteOP_SingleTask[Vector[Int]] {

    def task = {
      val vv = new IntVectorViewImpl(m._data, start, stride, length, is_row)
      if (m.frozen) vv.freeze()
      vv
    }
  }

  protected[optiml] case class OP_insertRow[A <: Int](m: IntMatrixImpl, pos: Int, x: Vector[A])
    extends DeliteOP_MutableSingleTask[Matrix[Int]](x)(m) {

    def task = {
      m.chkUpdate
      m.chkEquals(x._length, m._numCols)
      val idx = pos*m._numCols
      m.insertSpace(idx, m._numCols)
      for (i <- idx until idx+m._numCols){
        m._data(i) = x(i-idx)
      }
      m._numRows += 1
      m
    }
  }

  protected[optiml] case class OP_insertAllRows[A <: Int](m: IntMatrixImpl, pos: Int, xs: Matrix[A])
    extends DeliteOP_MutableSingleTask[Matrix[Int]](xs)(m) {

    def task = {
      m.chkUpdate
      m.chkEquals(xs._numCols, m._numCols)
      val idx = pos*m._numCols
      val sz = m.numCols*xs._numRows
      m.insertSpace(idx, sz)
      for (i <- idx until idx+sz){
        m._data(i) = xs.flattened(i-idx)
      }
      m._numRows += xs._numRows
      m
    }
  }

  protected[optiml] case class OP_insertCol[A <: Int](m: IntMatrixImpl, pos: Int, x: Vector[A])
    extends DeliteOP_MutableSingleTask[Matrix[Int]](x)(m) {

    def task = {
      m.chkUpdate
      m.chkEquals(x._length, m._numRows)

      val newCols = m.numCols+1
      val out_data = new Array[Int](m._numRows*newCols)
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

  protected[optiml] case class OP_insertAllCols[A <: Int](m: IntMatrixImpl, pos: Int, xs: Matrix[A])
    extends DeliteOP_MutableSingleTask[Matrix[Int]](xs)(m) {

    def task = {
      m.chkUpdate
      m.chkEquals(xs._numRows, m._numRows)

      val newCols = m._numCols+xs._numCols
      val out_data = new Array[Int](m._numRows*newCols)
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

  protected[optiml] case class OP_removeRows(m: IntMatrixImpl, pos: Int, num: Int)
    extends DeliteOP_MutableSingleTask[Matrix[Int]]()(m) {

    def task = {
      m.chkUpdate
      val idx = pos*m._numCols
      val len = num*m._numCols
      Array.copy(m._data, idx + len, m._data, idx, m.size - (idx + len))
      m._numRows -= num
      m
    }
  }

  protected[optiml] case class OP_removeCols(m: IntMatrixImpl, pos:Int, num: Int)
    extends DeliteOP_MutableSingleTask[Matrix[Int]]()(m) {

    def task = {
      m.chkUpdate

      val newCols = m._numCols-num
      val out_data = new Array[Int](m._numRows*newCols)
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

private[optiml] class IntMatrixImpl extends IntMatrix with GPUable[Int] {
  import IntMatrixImpl._
  
  type DSLType = IntMatrixImpl

  def this(nRows: Int, nCols: Int) = {
    this()
    _data = new Array[Int](nRows*nCols)
    init(nRows, nCols)
  }

  /* Generate a IntMatrixImpl with given array and row/col numbers (not necessarily consistent) */
  def this(data: Array[Int], nRows: Int, nCols: Int) = {
	  this()
    assert(data != null)
    _data = data
    init(nRows, nCols)
  }

  def init(nRows: Int, nCols: Int) {
    _numRows = nRows
    _numCols = nCols
    cvalue = this
    isComputed = true
  }

  override def concretize = {
    _data = cvalue._data
    _numRows = cvalue._numRows
    _numCols = cvalue._numCols
    cvalue = this
  }

  def numRows = force._numRows
  def numCols = force._numCols
  def frozen = force._frozen

  protected[optiml] var _data: Array[Int] = null
  override var _numRows = -1
  override var _numCols = 1
  override protected var _frozen = false

  //////////////
  // GPUable
  protected[delite] def gpu_data = _data
  protected[delite] def gpu_datasize = _numRows*_numCols
  protected[delite] def gpu_setdata(elms: Array[Int]) = { _data = elms }
  protected[delite] def gpu_apply(n: Int) = dc_apply(n)
  protected[delite] def gpu_update(n: Int, x: Int) = dc_update(n,x)

  //////////////////////
  // delite collection

  def dc_update(i: Int, x: Int) = {
    _data(i) = x
  }
  def dc_apply(i: Int) : Int = {
    _data(i)
  }


  //////////////////////
  // public

  def apply(i: Int, j: Int) : Int = {
    //_data(chkPos(i*numCols+j))
    _data(i*_numCols+j)
  }

  def update(row: Int, col: Int, x: Int) = {
    //chkUpdate
    //_data(chkPos(row*numCols+col)) = x
    _data(row*_numCols+col) = x
  }

  override def clone: Matrix[Int] = {
    run(OP_clone(this))(intMatFactory)
  }

  def mutableClone: Matrix[Int] = {
    run(OP_mutableClone(this))(intMatFactory)
  }

  def vview(start: Int, stride: Int, length: Int, is_row: Boolean) : Vector[Int] = {
    //run(OP_vview(this,start,stride,length,is_row))(intVecViewProxyFactory)
    new IntVectorViewImpl(_data, start, stride, length, is_row)    
  }

  def insertRow[A <: Int](pos: Int, x: Vector[A]): Matrix[Int] = {
    run(OP_insertRow(this,pos,x))(intMatFactory)
  }

  def insertAllRows[A <: Int](pos: Int, xs: Matrix[A]): Matrix[Int] = {
    run(OP_insertAllRows(this,pos,xs))(intMatFactory)
  }

  def insertCol[A <: Int](pos: Int, x: Vector[A]): Matrix[Int] = {
    run(OP_insertCol(this,pos,x))(intMatFactory)
  }

  def insertAllCols[A <: Int](pos: Int, xs: Matrix[A]): Matrix[Int] = {
    run(OP_insertAllCols(this,pos,xs))(intMatFactory)
  }

  def removeRows(pos: Int, num: Int): Matrix[Int] = {
    run(OP_removeRows(this,pos,num))(intMatFactory)
  }

  def removeCols(pos:Int, num: Int): Matrix[Int] = {
    run(OP_removeCols(this,pos,num))(intMatFactory)
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
    val d = new Array[Int](n)
    Array.copy(_data, 0, d, 0, size)
    _data = d
  }

  protected def insertSpace(pos: Int, len: Int) {
    if (pos < 0 || pos > size) throw new IndexOutOfBoundsException
    ensureExtra(len)
    Array.copy(_data, pos, _data, pos + len, size - pos)
  }

  protected def chkPos(index: Int) = {
    if (index < 0 || index >= size) throw new IndexOutOfBoundsException
    index
  }
}

