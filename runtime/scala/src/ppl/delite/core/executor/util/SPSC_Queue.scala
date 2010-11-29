package ppl.delite.core.executor.util

import ppl.delite.core.Config


class SPSC_Queue[T <: AnyRef] {
  @volatile
  var read_ptr = 0

  @volatile
  var write_ptr = 0

  val max_size = Config.executorInQueueSize
  assert((max_size & (max_size - 1)) == 0)
  val q_data = new Array[AnyRef](max_size)

  def put(e:T): Boolean = {
    val wr_ptr = write_ptr
    val nextElement = (wr_ptr + 1) & (max_size - 1)
    if(nextElement != read_ptr) {
      q_data(wr_ptr) = e
      write_ptr = nextElement;
      return true
    } else {
      return false
    }
  }

  def poll():T = {
    val rd_ptr = read_ptr
    if(rd_ptr == write_ptr)
      return null.asInstanceOf[T]
    val nextElement = (rd_ptr + 1) & (max_size - 1)
    val res = q_data(rd_ptr)
    read_ptr = nextElement
    return res.asInstanceOf[T]
  }

  def isEmpty: Boolean = {
    return read_ptr == write_ptr
  }

}