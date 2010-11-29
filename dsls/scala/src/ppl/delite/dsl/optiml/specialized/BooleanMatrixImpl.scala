/* Boolean matrix datatype
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

object BooleanMatrixImpl {
  protected[optiml] case class OP_clone(m: BooleanMatrixImpl) extends DeliteOP_SingleTask[Matrix[Boolean]](m) {
    def task = {
      val ret = m.mutableClone
      if (m.frozen) ret.freeze()
      ret
    }
  }

  protected[optiml] case class OP_mutableClone(m: BooleanMatrixImpl) extends DeliteOP_SingleTask[Matrix[Boolean]](m) {
    def task = {
      val res = new BooleanMatrixImpl(m._numRows, m._numCols)
      for (i <- 0 until m.size){
        res.dc_update(i, m._data(i))
      }
      res
    }
  }

  protected[optiml] case class OP_vview(m: BooleanMatrixImpl, start: Int, stride: Int, length: Int, is_row: Boolean)
    extends DeliteOP_SingleTask[Vector[Boolean]] {

    def task = {
      val vv = new BooleanVectorViewImpl(m._data, start, stride, length, is_row)
      if (m.frozen) vv.freeze()
      vv
    }
  }

  protected[optiml] case class OP_insertRow[A <: Boolean](m: BooleanMatrixImpl, pos: Int, x: Vector[A])
    extends DeliteOP_MutableSingleTask[Matrix[Boolean]](x)(m) {

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

  protected[optiml] case class OP_insertAllRows[A <: Boolean](m: BooleanMatrixImpl, pos: Int, xs: Matrix[A])
    extends DeliteOP_MutableSingleTask[Matrix[Boolean]](xs)(m) {

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

  protected[optiml] case class OP_insertCol[A <: Boolean](m: BooleanMatrixImpl, pos: Int, x: Vector[A])
    extends DeliteOP_MutableSingleTask[Matrix[Boolean]](x)(m) {

    def task = {
      m.chkUpdate
      m.chkEquals(x._length, m._numRows)

      val newCols = m.numCols+1
      val out_data = new Array[Boolean](m._numRows*newCols)
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

  protected[optiml] case class OP_insertAllCols[A <: Boolean](m: BooleanMatrixImpl, pos: Int, xs: Matrix[A])
    extends DeliteOP_MutableSingleTask[Matrix[Boolean]](xs)(m) {

    def task = {
      m.chkUpdate
      m.chkEquals(xs._numRows, m._numRows)

      val newCols = m._numCols+xs._numCols
      val out_data = new Array[Boolean](m._numRows*newCols)
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

  protected[optiml] case class OP_removeRows(m: BooleanMatrixImpl, pos: Int, num: Int)
    extends DeliteOP_MutableSingleTask[Matrix[Boolean]]()(m) {

    def task = {
      m.chkUpdate
      val idx = pos*m._numCols
      val len = num*m._numCols
      Array.copy(m._data, idx + len, m._data, idx, m.size - (idx + len))
      m._numRows -= num
      m
    }
  }

  protected[optiml] case class OP_removeCols(m: BooleanMatrixImpl, pos:Int, num: Int)
    extends DeliteOP_MutableSingleTask[Matrix[Boolean]]()(m) {

    def task = {
      m.chkUpdate

      val newCols = m._numCols-num
      val out_data = new Array[Boolean](m._numRows*newCols)
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

private[optiml] class BooleanMatrixImpl extends BooleanMatrix with GPUable[Boolean] {
  import BooleanMatrixImpl._
  
  type DSLType = BooleanMatrixImpl

  def this(nRows: Int, nCols: Int) = {
    this()
    _data = new Array[Boolean](nRows*nCols)
    init(nRows, nCols)
  }

  /* Generate a BooleanMatrixImpl with given array and row/col numbers (not necessarily consistent) */
  def this(data: Array[Boolean], nRows: Int, nCols: Int) = {
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

  protected[optiml] var _data: Array[Boolean] = null
  override var _numRows = -1
  override var _numCols = 1
  override protected var _frozen = false

  //////////////
  // GPUable
  protected[delite] def gpu_data = _data
  protected[delite] def gpu_datasize = _numRows*_numCols
  protected[delite] def gpu_setdata(elms: Array[Boolean]) = { _data = elms }
  protected[delite] def gpu_apply(n: Int) = dc_apply(n)
  protected[delite] def gpu_update(n: Int, x: Boolean) = dc_update(n,x)

  //////////////////////
  // delite collection

  def dc_update(i: Int, x: Boolean) = {
    _data(i) = x
  }
  def dc_apply(i: Int) : Boolean = {
    _data(i)
  }


  //////////////////////
  // public

  def apply(i: Int, j: Int) : Boolean = {
    //_data(chkPos(i*numCols+j))
    _data(i*_numCols+j)
  }

  def update(row: Int, col: Int, x: Boolean) = {
    //chkUpdate
    //_data(chkPos(row*numCols+col)) = x
    _data(row*_numCols+col) = x
  }

  override def clone: Matrix[Boolean] = {
    run(OP_clone(this))(booleanMatFactory)
  }

  def mutableClone: Matrix[Boolean] = {
    run(OP_mutableClone(this))(booleanMatFactory)
  }

  def vview(start: Int, stride: Int, length: Int, is_row: Boolean) : Vector[Boolean] = {
    //run(OP_vview(this,start,stride,length,is_row))(booleanVecViewProxyFactory)
    new BooleanVectorViewImpl(_data, start, stride, length, is_row)    
  }

  def insertRow[A <: Boolean](pos: Int, x: Vector[A]): Matrix[Boolean] = {
    run(OP_insertRow(this,pos,x))(booleanMatFactory)
  }

  def insertAllRows[A <: Boolean](pos: Int, xs: Matrix[A]): Matrix[Boolean] = {
    run(OP_insertAllRows(this,pos,xs))(booleanMatFactory)
  }

  def insertCol[A <: Boolean](pos: Int, x: Vector[A]): Matrix[Boolean] = {
    run(OP_insertCol(this,pos,x))(booleanMatFactory)
  }

  def insertAllCols[A <: Boolean](pos: Int, xs: Matrix[A]): Matrix[Boolean] = {
    run(OP_insertAllCols(this,pos,xs))(booleanMatFactory)
  }

  def removeRows(pos: Int, num: Int): Matrix[Boolean] = {
    run(OP_removeRows(this,pos,num))(booleanMatFactory)
  }

  def removeCols(pos:Int, num: Int): Matrix[Boolean] = {
    run(OP_removeCols(this,pos,num))(booleanMatFactory)
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
    val d = new Array[Boolean](n)
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

