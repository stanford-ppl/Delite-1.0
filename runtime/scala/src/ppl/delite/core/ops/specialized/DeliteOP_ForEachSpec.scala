package ppl.delite.core.ops.specialized

import ppl.delite.core.{DeliteUnit, DeliteCollection, DeliteDSLType, DeliteFunc}
import ppl.delite.core.ops.{DeliteOP_ForEach, DeliteOP}

abstract class DeliteOP_ForEachSpec[@specialized(Double,Float,Int)A, R <: DeliteDSLType](immutable: DeliteDSLType*)(mutable: DeliteDSLType*)(implicit c: ClassManifest[R])
  extends DeliteOP_ForEach[A,R](immutable: _*)(mutable: _*) {

  val coll: DeliteCollection[A]
  val out: R

  override def func: A => Unit
  
  override def task = {
    specForEachTask(coll, func, 0, total)

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
      specForEachTask(coll, func, pos, total)
      chunkDone(pos-1) = true
    }
  }

  private def specForEachTask(coll: DeliteCollection[A], func: A => Unit, pos: Int, total: Int) {
    val chunk_size = coll.size / total
    val remainder = coll.size % total
    val sz = if (pos < remainder) chunk_size + 1 else chunk_size
    val off = if (pos < remainder) 0 else pos - remainder
    var idx = pos*(chunk_size+1) - off
    val end = idx + sz
    while (idx != end) {
      func(coll.dc_apply(idx))
      idx += 1
    }
  }

}