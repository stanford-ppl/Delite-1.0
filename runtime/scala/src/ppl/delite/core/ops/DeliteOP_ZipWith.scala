package ppl.delite.core.ops

import ppl.delite.core.{DeliteUnit, DeliteFunc, DeliteCollection}

/**
 * Author: Kevin J. Brown
 * Date: Apr 17, 2010
 * Time: 2:16:21 AM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * A ZipWith interface designed for array-based DeliteCollections
 */

abstract class DeliteOP_ZipWith2[A,B,R,C[X] <: DeliteCollection[X]] extends DeliteOP[C[R]] {
  val collA: DeliteCollection[A]
  val collB: DeliteCollection[B]
  val out: C[R]

  protected var total = 1

  @volatile
  protected var chunkDone: Array[Boolean] = null

  final override def isChunkable = true

  def func: (A, B) => R

  override def getImmutableDeps = {
    val immDeps = if (collA != out && collB != out) Seq(collA, collB)
      else if (collA != out) Seq(collA)
      else if (collB != out) Seq(collB)
      else Seq()

    func match {
      case f: DeliteFunc => immDeps ++ f.deps
      case _ => immDeps
    }
  }

  override def getMutableDeps = Seq(out)

  override def submitChunks(num_chunks: Int, submit: DeliteOP[DeliteUnit] => Unit) {
    chunkDone = new Array[Boolean](num_chunks-1)
    total = num_chunks
    var i = 1
    while (i != num_chunks) {
      submit(new SlaveChunk(collA, collB, out, i, num_chunks))
      i += 1
    }
  }

  def task = {
    val chunk_size = collA.size / total
    val remainder = collA.size % total
    val end = if (remainder != 0) chunk_size + 1 else chunk_size
    var idx = 0
    while (idx != end) {
      out.dc_update(idx, func(collA.dc_apply(idx), collB.dc_apply(idx)))
      idx += 1
    }
    //synchronize here //TODO: how to best synchronize on chunkDone (volatile, etc.) (spin, etc.)
    var ci = 0
    val cend = if (chunkDone != null) chunkDone.length else 0
    while (ci != cend) {
      while (!chunkDone(ci)) { }
      ci += 1
    }
    out
  }

  private class SlaveChunk(collA: DeliteCollection[A], collB: DeliteCollection[B], out: C[R], pos: Int, total: Int) extends DeliteOP[DeliteUnit] {

    override def getImmutableDeps = null
    override def getMutableDeps = null

    def task = {
      val chunk_size = collA.size / total
      val remainder = collA.size % total
      val sz = if (pos < remainder) chunk_size + 1 else chunk_size
      val off = if (pos < remainder) 0 else pos - remainder
      var idx = pos*(chunk_size+1) - off
      val end = idx + sz
      while (idx != end) {
        out.dc_update(idx, func(collA.dc_apply(idx), collB.dc_apply(idx)))
        idx += 1
      }
      chunkDone(pos-1) = true
    }
  }

}