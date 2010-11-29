package ppl.delite.core.ops.specialized

import ppl.delite.core.ops.{DeliteOP, DeliteOP_DynamicMap}
import ppl.delite.core.{DeliteUnit, DeliteCollection}

/**
 * Author: Kevin J. Brown
 * Date: Jul 18, 2010
 * Time: 7:50:46 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

abstract class DeliteOP_DynamicMapSpec[@specialized(Double,Float,Int)A, C[X] <: DeliteCollection[X]] extends DeliteOP_DynamicMap[A, A, C] {

  val coll: DeliteCollection[A]
  val out: C[A]

  override def func: A => A

  override def task = {
    specDynamicMapTask
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
      submit(new SlaveChunk(i))
      i += 1
    }
  }

  private def specDynamicMapTask {
    var idx = ChunkAllocator.getNext
    while (idx != -1) {
      val step = idx + chunkSize
      val end = if (step < size) step else size
      while (idx < end) {
        out.dc_update(idx, func(coll.dc_apply(idx)))
        idx += 1
      }
      idx = ChunkAllocator.getNext
    }
  }

  private class SlaveChunk(pos: Int) extends DeliteOP[DeliteUnit] {

    override def getImmutableDeps = null
    override def getMutableDeps = null

    def task = {
      specDynamicMapTask
      chunkDone(pos-1) = true
    }
  }

}
