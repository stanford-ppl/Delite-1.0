package ppl.delite.core.ops

import reflect.ClassManifest
import ppl.delite.core.{DeliteDSLType, DeliteUnit, DeliteFunc, DeliteCollection}

abstract class DeliteOP_ForEach[A, R <: DeliteDSLType](immutable: DeliteDSLType*)(mutable: DeliteDSLType*)(implicit c: ClassManifest[R])
  extends DeliteOP[R] {

  val coll: DeliteCollection[A]

  val out: R

  def func: A => Unit

  override def getImmutableDeps = {
    val immDeps = (if (coll != out) Seq(coll) else Seq()) ++ immutable
    func match {
      case f: DeliteFunc => immDeps ++ f.deps
      case _ => immDeps
    }
  }

  override def getMutableDeps = Seq(out) ++ mutable


  final override def isChunkable = true

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
      func(coll.dc_apply(idx))
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
        func(coll.dc_apply(idx))
        idx += 1
      }
      chunkDone(pos-1) = true
    }
  }

}