/* Streaming double matrix datatype
 *
 * author: Bo Wang (bowang@stanford.edu)
 * last modified: 12/13/10
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
import ppl.delite.core.ops.{DeliteOP_ForEach, DeliteOP_MutableSingleTask, DeliteOP_SingleTask}
import ppl.delite.dsl.primitive.DeliteLong
import ppl.delite.core.DeliteUnit

object StreamingDoubleMatrixImpl {

  protected[optiml] case class OP_clone(m: StreamingDoubleMatrixImpl) extends DeliteOP_SingleTask[Matrix[Double]](m) {
    def task = {
      val ret = m.mutableClone
      if (m.frozen) ret.freeze()
      ret
    }
  }

  protected[optiml] case class OP_mutableClone(m: StreamingDoubleMatrixImpl) extends DeliteOP_SingleTask[Matrix[Double]](m) {
    def task = {
      val res = new StreamingDoubleMatrixImpl(m._numRows, m._numCols, m.initializer, m._numChunks)
      // just create an empty matrixImpl since data is streaming and no need to copy
      res
    }
  }

  protected[optiml] case class OP_vview(m: StreamingDoubleMatrixImpl, start: Int, stride: Int, length: Int, is_row: Boolean)
    extends DeliteOP_SingleTask[Vector[Double]] {

    def task = {
      val index  = start / m._numCols
      val offset = start % m._numCols
      val vv = new DoubleVectorViewImpl(m._data2d(index), offset, stride, length, is_row)
      if (m.frozen) vv.freeze()
      vv
    }
  }

  protected[optiml] case class OP_foreachRow(f: Vector[Double]=>Unit, m: StreamingDoubleMatrixImpl, from: Int, to: Int)
    extends DeliteOP_ForEach[Vector[Double],DeliteUnit](m)(){

    val tcoll = Vector[Vector[Double]](false, to - from)
    var i = 0
    val i_end = if(to < m.numRows) to - from else m.numRows - from
    while(i < i_end){
      tcoll(i) = new DoubleVectorImpl(m._data2d(i), true, m._numCols)
      tcoll(i).asInstanceOf[DoubleVectorImpl]._is_initialized = false
      tcoll(i).asInstanceOf[DoubleVectorImpl]._is_streaming = true
      tcoll(i).asInstanceOf[DoubleVectorImpl]._index = from + i
      tcoll(i).asInstanceOf[DoubleVectorImpl]._matrix = m
      i += 1
    }
    val coll: Vector[Vector[Double]] = tcoll
    val out = {
      val x = new DeliteUnit
      x.isComputed = true
      x
    }
    def func = f
  }

}

private[optiml] class StreamingDoubleMatrixImpl extends DoubleMatrix /*with GPUable[Double]*/ {
  import StreamingDoubleMatrixImpl._

  type DSLType = StreamingDoubleMatrixImpl

  def this(nRows:Int, nCols:Int, block:(Int,Int)=>Double, numChunks:Int = 1024) = {
    this()
    initializer = block
    _data2d = new Array[Array[Double]](numChunks)
    var i = 0
    while(i < numChunks){
      _data2d(i)= new Array[Double](nCols)
      i += 1
    }
    init(nRows, nCols, numChunks)
  }

  override def initialize(row:Int, col:Int):Double = {
    initializer(row, col)
  }
  
  def init(nRows: Int, nCols: Int, nChunks: Int) {
    _numRows = nRows
    _numCols = nCols
    _numChunks = nChunks
    cvalue = this
    isComputed = true
  }

  override def concretize = {
    _numRows = cvalue._numRows
    _numCols = cvalue._numCols
    _numChunks = cvalue._numChunks
    initializer = cvalue.initializer
    cvalue = this
  }

  def numRows = force._numRows
  def numCols = force._numCols
  def frozen = force._frozen

  protected[optiml] var _data: Array[Double] = null
  protected[optiml] var _data2d: Array[Array[Double]] = null
  override var _numRows = -1
  override var _numCols = 1
  override protected var _frozen = false

  var _numChunks = 0
  var _startRow = 0
  var _endRow = 0

  var initializer:(Int,Int) => Double = null

  //////////////
  // GPUable
/*
  protected[delite] def gpu_data = _data
  protected[delite] def gpu_datasize = _numRows*_numCols
  protected[delite] def gpu_setdata(elms: Array[Double]) = { _data = elms }
  protected[delite] def gpu_apply(n: Int) = dc_apply(n)
  protected[delite] def gpu_update(n: Int, x: Double) = dc_update(n,x)
*/

  //////////////////////
  // delite collection

  def dc_update(i: Int, x: Double) = {
    //_data(i) = x
    throw new UnsupportedOperationException("dc_update is not supported by StreamingDoubleMatrix")
  }
  def dc_apply(i: Int) : Double = {
    //_data(i)
    throw new UnsupportedOperationException("dc_apply is not supported by StreamingDoubleMatrix")
  }

  //////////////////////
  // public

  def apply(i: Int, j: Int) : Double ={
    throw new UnsupportedOperationException("apply is not supported by StreamingDoubleMatrix")
  }


  def update(row: Int, col: Int, x: Double) = {
    //chkUpdate
    //_data(chkPos(row*numCols+col)) = x
    //_data(row*_numCols+col) = x
    throw new UnsupportedOperationException("update is not supported by StreamingDoubleMatrix")
  }

  override def initRow(row_idx: Int) {
    var i = 0
    val row = row_idx % _numChunks
    val _data = _data2d(row)
    while(i < _numCols){
      _data(i) = initializer(row_idx, i)
      i += 1
    }
  }

  override def getRow(row_id: Int) : Vector[Double] = {
    val real_id = row_id % _numChunks
    val v = new DoubleVectorImpl(_data2d(real_id), true, _numCols)
    v._matrix = this
    v._index = row_id
    v._is_streaming = true
    v._is_initialized = false
    v
  }

  override def foreachRow(f: (Vector[Double],Int) => Unit) {
    var i = 0
    val i_end = (scala.math.ceil(_numRows.toDouble/_numChunks.toDouble)).toInt
    while (i < i_end){
      var j = i*_numChunks
      var j_end = j + _numChunks
      if(j_end > _numRows)  j_end = _numRows
      while (j < j_end){
        // TODO: the function call to block should be replaced by a task submitted to runtime
        f(getRow(j), j)
        j += 1
      }
      // TODO: when running in parallel, synchronization is needed here
      // make sure all tasks created are finished before proceeding to the next chunk
      i += 1
    }
  }

  override def foreachRow(f: Vector[Double] => Unit) {
    var i = 0
    val i_end = (scala.math.ceil(_numRows.toDouble/_numChunks.toDouble)).toInt
    while (i < i_end){
      val from = i*_numChunks
      var to = from + _numChunks
      if(to > _numRows)  to = _numRows
      val op = OP_foreachRow(f, this, from, to)
      val proxy = run(op)
      proxy.force
      i += 1
    }
  }

/* backup version
  override def foreachRow(f: Vector[Double] => Unit) {
    var i = 0
    val i_end = (scala.math.ceil(_numRows.toDouble/_numChunks.toDouble)).toInt
    while (i < i_end){
      var j = i*_numChunks
      var j_end = j + _numChunks
      if(j_end > _numRows)  j_end = _numRows
      while (j < j_end){
        // TODO: the function call to block should be replaced by a task submitted to runtime
        f(getRow(j))
        j += 1
      }
      // TODO: when running in parallel, synchronization is needed here
      // make sure all tasks created are finished before proceeding to the next chunk
      i += 1
    }
  }

*/
/*
  // real row version
  def getRealRow(row: Int, row_idx: Int) : DoubleRowImpl = {
      val r = new DoubleRowImpl(_data2d(row), true, _numCols, row_idx, this)
      r
    }
  override def foreachRow(block: Vector[Double] => Unit) {
    var i = 0
    val i_end = (scala.math.ceil(_numRows.toDouble/_numChunks.toDouble)).toInt
    while (i < i_end){
      var j = 0
      var j_end = _numChunks
      if(j_end + i*_numChunks > _numRows)
        j_end = _numRows - i*_numChunks
      while (j < j_end){
        val row_id = j + i*_numChunks
        // TODO: the function call to block should be replaced by a task submitted to runtime
        block(getRealRow(j, row_id))
        j += 1
      }
      // TODO: when running in parallel, synchronization is needed here
      // make sure all tasks created are finished before proceeding to the next chunk
      i += 1
    }
  }
*/
  override def clone: Matrix[Double] = {
    run(OP_clone(this))(doubleMatFactory)
  }

  def mutableClone: Matrix[Double] = {
    run(OP_mutableClone(this))(doubleMatFactory)
  }

  def vview(start: Int, stride: Int, length: Int, is_row: Boolean) : Vector[Double] = {
    //run(OP_vview(this,start,stride,length,is_row))(doubleVecViewProxyFactory)
    //new DoubleVectorViewImpl(_data, start, stride, length, is_row)
    throw new UnsupportedOperationException("vview is not supported by StreamingDoubleMatrix")
  }

  def insertRow[A <: Double](pos: Int, x: Vector[A]): Matrix[Double] = {
    throw new UnsupportedOperationException("insertRow is not supported by StreamingDoubleMatrix")
  }

  def insertAllRows[A <: Double](pos: Int, xs: Matrix[A]): Matrix[Double] = {
    throw new UnsupportedOperationException("insertAllRows is not supported by StreamingDoubleMatrix")
  }

  def insertCol[A <: Double](pos: Int, x: Vector[A]): Matrix[Double] = {
    throw new UnsupportedOperationException("insertCol is not supported by StreamingDoubleMatrix")
  }

  def insertAllCols[A <: Double](pos: Int, xs: Matrix[A]): Matrix[Double] = {
    throw new UnsupportedOperationException("innsertAllCols is not supported by StreamingDoubleMatrix")
  }

  def removeRows(pos: Int, num: Int): Matrix[Double] = {
    throw new UnsupportedOperationException("removeRows is not supported by StreamingDoubleMatrix")
  }

  def removeCols(pos:Int, num: Int): Matrix[Double] = {
    throw new UnsupportedOperationException("RemoveCols is not supported by StreamingDoubleMatrix")
  }

  override def dist(i:Int, j:Int): Double = {
    throw new UnsupportedOperationException("dist is not supported by StreamingDoubleMatrix")
  }

  protected def chkPos(index: Int) = {
    if (index < 0 || index >= size) throw new IndexOutOfBoundsException
    index
  }

}

