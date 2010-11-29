package ppl.delite.core.ops

import ppl.delite.core.{DeliteDSLType, DeliteUnit, DeliteFunc, DeliteCollection}

/**
 * Created by IntelliJ IDEA.
 * User: Kevin
 * Date: May 6, 2010
 * Time: 12:31:42 AM
 * To change this template use File | Settings | File Templates.
 */

abstract class DeliteOP_Map[A,B,C[X] <: DeliteCollection[X]] extends DeliteOP[C[B]] {

  val coll: DeliteCollection[A]

  val out: C[B]

  def func: A => B

  final override def isChunkable = true
  //def func2(a: A) : B = func(a)

  override def getImmutableDeps = {
    val immDeps = if (coll != out) Seq(coll) else Seq()
    func match {
      case f: DeliteFunc => immDeps ++ f.deps
      case _ => immDeps
    }
  }

  override def getMutableDeps: Seq[DeliteDSLType] = Seq(out)

  protected var total = 1

  @volatile
  protected var chunkDone: Array[Boolean] = null

  override def submitChunks(num_chunks: Int, submit: DeliteOP[DeliteUnit] => Unit) {
    chunkDone = new Array[Boolean](num_chunks-1)
    total = num_chunks
    var i = 1
    while (i != num_chunks) {
      submit(new SlaveChunk(i, num_chunks))
      i += 1
    }
  }

  def task = {
    val chunk_size = coll.size / total
    val remainder = coll.size % total
    val end = if (remainder != 0) chunk_size + 1 else chunk_size
    var idx = 0
    while (idx != end) {
      out.dc_update(idx, func(coll.dc_apply(idx)))
      idx += 1
    }

    //barrier
    var ci = 0
    val cend = if (chunkDone != null) chunkDone.length else 0
    while (ci != cend) {
      while (!chunkDone(ci)) { }
      ci += 1
    }
    out
  }

  private class SlaveChunk(pos: Int, total: Int) extends DeliteOP[DeliteUnit] {

    override def getImmutableDeps = null
    override def getMutableDeps = null

    def task = {
      val chunk_size = coll.size / total
      val remainder = coll.size % total
      val sz = if (pos < remainder) chunk_size + 1 else chunk_size
      val off = if (pos < remainder) 0 else pos - remainder
      var idx = pos*(chunk_size+1) - off
      val end = idx + sz
      while (idx != end) {
        out.dc_update(idx, func(coll.dc_apply(idx)))
        idx += 1
      }
      chunkDone(pos-1) = true
    }
  }

}