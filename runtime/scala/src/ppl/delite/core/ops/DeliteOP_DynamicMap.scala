package ppl.delite.core.ops

import ppl.delite.core.{DeliteUnit, DeliteDSLType, DeliteFunc, DeliteCollection}

/**
 * Author: Kevin J. Brown
 * Date: Jul 18, 2010
 * Time: 7:31:33 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

abstract class DeliteOP_DynamicMap[A,B,C[X] <: DeliteCollection[X]] extends DeliteOP[C[B]] {

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

  protected val size = coll.size

  //TODO: how to choose this value? (currently chosen for LBP using hprof)
  protected val chunkSize = 10

  @volatile
  protected var chunkDone: Array[Boolean] = null

  override def submitChunks(num_chunks: Int, submit: DeliteOP[DeliteUnit] => Unit) {
    chunkDone = new Array[Boolean](num_chunks-1)
    total = num_chunks
    var i = 1
    while (i != num_chunks) {
      submit(new SlaveChunk(i))
      i += 1
    }
  }

  def task = {
    dynamicMapTask
    //barrier
    var ci = 0
    val cend = if (chunkDone != null) chunkDone.length else 0
    while (ci != cend) {
      while (!chunkDone(ci)) { }
      ci += 1
    }
    out
  }

  private def dynamicMapTask {
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
      dynamicMapTask
      chunkDone(pos-1) = true
    }
  }

  protected object ChunkAllocator {
    private var idx = -chunkSize

    def getNext = {
      this.synchronized {
        idx += chunkSize
        val ret = if (idx < size) idx else -1
        ret
      }
    }
  }

}