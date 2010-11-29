/* VectorView must be backed by an array reference. It can be used to perform
 * vector operations (e.g. map) on a strided sequence of data (e.g., a row of
 * a row-major matrix).              
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Feb 17, 2010
 * modified: Feb 17, 2010
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
package ppl.delite.dsl.optiml.specialized

import ppl.delite.core.appinclude._
import ppl.delite.nativeGPU.GPUable
import ppl.delite.dsl.optiml.{ArithOps, Vector, VectorView}
import ppl.delite.core.ops.DeliteOP_SingleTask

class BooleanVectorViewImpl
  extends VectorView[Boolean] with BooleanVector with GPUable[Boolean]{

  type DSLType = BooleanVectorViewImpl

  def this(x: Array[Boolean], offset: Int, str: Int, len: Int, row_vec: Boolean) = {
    this()
    init(x, offset, str, len, row_vec)
    isComputed = true
  }

  def init(x: Array[Boolean], offset: Int, str: Int, len: Int, row_vec: Boolean) = {
    _data = x
    _start = offset
    _stride = str
    _length = len
    _is_row = row_vec
    cvalue = this
  }

  override def concretize {
    _data = cvalue._data
    _start = cvalue._start
    _stride = cvalue._stride
    _length = cvalue._length
    _is_row = cvalue._is_row
    _frozen = cvalue._frozen
	if(_data == null) println("ERR")
    cvalue = this
  }

  //assert(len <= x.length)

  def start = force._start
  def stride = force._stride
  def length = force._length
  def is_row = force._is_row
  def frozen = force._frozen

  protected[optiml] var _data: Array[Boolean] = null
  override var _length = -1
  override var _is_row = false
  override protected var _frozen = false
  protected var _start = -1
  protected var _stride = -1

  //////////////
  // GPUable
  protected[delite] def gpu_data = _data
  protected[delite] def gpu_datasize = _length
  protected[delite] def gpu_setdata(elms: Array[Boolean]) = { _data = elms }
  protected[delite] def gpu_apply(n: Int) = apply(n)
  protected[delite] def gpu_update(n: Int, x: Boolean) = update(n,x)

  ///////////////////
  // DeliteCollection

  override def dc_apply(i: Int) = apply(i)
  override def dc_update(i: Int, x: Boolean) = update(i,x)


  //////////////
  // life cycle

  def mutableClone = Vector[Boolean](is_row, 0) ++= this

  def view(start: Int, stride: Int, length: Int, is_row: Boolean) = new BooleanVectorViewImpl(_data, start, stride, length, is_row)

  ////////////
  // data ops

  def apply(n: Int) : Boolean = {
    //_data(chkIndex(idx(n)))
    _data(idx(n))
  }

  def update(n: Int, x: Boolean) {
    //run(OP_update(this,index,x))
    //chkUpdate
    _data(idx(n)) = x
  }

  override protected def chkIndex(index: Int) = {
    if (index < 0 || index >= _data.length)
      throw new IndexOutOfBoundsException
    index
  }
}
