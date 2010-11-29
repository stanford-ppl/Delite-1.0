package ppl.delite.core.ops.specialized

import ppl.delite.core.{DeliteUnit, DeliteCollection}
import ppl.delite.core.ops.{DeliteOP, DeliteOP_Map}

abstract class DeliteOP_MapSpec[@specialized(Double,Float,Int)A, C[X] <: DeliteCollection[X]] extends DeliteOP_Map[A, A, C] {

  val coll: DeliteCollection[A]
  val out: C[A]

  override def func: A => A

  override def task = {
    specMapTask(coll, func, 0, total)

    //barrier
    var ci = 0
    val cend = if (chunkDone != null) chunkDone.length else 0
    while (ci != cend) {
      while (!chunkDone(ci)) { }
      ci += 1
      }
    out
  }
  
  override def submitChunks(num_chunks: Int, submit: DeliteOP[DeliteUnit] => Unit) {
    chunkDone = new Array[Boolean](num_chunks-1)
    total = num_chunks
    var i = 1
    while (i != num_chunks) {
      submit(new SlaveChunk(i, num_chunks))
      i += 1
    }
  }

  private class SlaveChunk(pos: Int, total: Int) extends DeliteOP[DeliteUnit] {

    override def getImmutableDeps = null
    override def getMutableDeps = null

    def task = {
      specMapTask(coll, func, pos, total)
      chunkDone(pos-1) = true
    }
  }

  private def specMapTask(coll: DeliteCollection[A], func: A => A, pos: Int, total: Int) {
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
  }
}

