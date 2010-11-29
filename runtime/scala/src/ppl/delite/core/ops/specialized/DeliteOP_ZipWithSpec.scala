package ppl.delite.core.ops.specialized

import ppl.delite.core.ops.{DeliteOP_ZipWith2, DeliteOP}
import ppl.delite.core.{DeliteUnit, DeliteFunc, DeliteCollection}

abstract class DeliteOP_ZipWith2Spec[@specialized(Double,Float,Int)A,C[X] <: DeliteCollection[X]] extends DeliteOP_ZipWith2[A, A, A, C] {

  val collA: DeliteCollection[A]
  val collB: DeliteCollection[A]
  val out: C[A]

  override def func: (A, A) => A

  override def task = {
    specZipWithTask(collA, collB, func, 0, total)

    // barrier
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
      specZipWithTask(collA, collB, func, pos, total)
      chunkDone(pos-1) = true
    }
  }
   
  final private def specZipWithTask(collA: DeliteCollection[A], collB: DeliteCollection[A], func: (A, A) => A, pos: Int, total: Int) {
    val outm = out.asInstanceOf[DeliteCollection[A]]

    val chunk_size = collA.size / total
    val remainder = collA.size % total
    val sz = if (pos < remainder) chunk_size + 1 else chunk_size
    val off = if (pos < remainder) 0 else pos - remainder
    var idx = pos*(chunk_size+1) - off
    val end = idx + sz
    while (idx != end) {
      outm.dc_update(idx, func(collA.dc_apply(idx), collB.dc_apply(idx)))
      idx += 1
    }
  }
}


/**
 * These are used to help the JIT perform inlining optimizations. They are identical except for their names.
 * Unfortunately, this makes the DSL bleed into Delite. We need a better solution long-term.
 */
/*
abstract class DeliteOP_ZipWith2SpecMatrix[@specialized(Double,Float,Int)A,C[X] <: DeliteCollection[X]] extends DeliteOP_ZipWith2[A, A, A, C] {

  val collA: DeliteCollection[A]
  val collB: DeliteCollection[A]
  val out: C[A]

  override def func: (A, A) => A

  override def task = {
    specZipWithTask(collA, collB, func, 0, total)

    // barrier
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
      specZipWithTask(collA, collB, func, pos, total)
      chunkDone(pos-1) = true
    }
  }

  private def specZipWithTask(collA: DeliteCollection[A], collB: DeliteCollection[A], func: (A, A) => A, pos: Int, total: Int) {
    val outm = out.asInstanceOf[DeliteCollection[A]]

    val chunk_size = collA.size / total
    val remainder = collA.size % total
    val sz = if (pos < remainder) chunk_size + 1 else chunk_size
    val off = if (pos < remainder) 0 else pos - remainder
    var idx = pos*(chunk_size+1) - off
    val end = idx + sz
    while (idx != end) {
      outm.dc_update(idx, func(collA.dc_apply(idx), collB.dc_apply(idx)))
      idx += 1
    }
  }
}

abstract class DeliteOP_ZipWith2SpecVector[@specialized(Double,Float,Int)A,C[X] <: DeliteCollection[X]] extends DeliteOP_ZipWith2[A, A, A, C] {

  val collA: DeliteCollection[A]
  val collB: DeliteCollection[A]
  val out: C[A]

  override def func: (A, A) => A

  override def task = {
    specZipWithTask(collA, collB, func, 0, total)

    // barrier
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
      specZipWithTask(collA, collB, func, pos, total)
      chunkDone(pos-1) = true
    }
  }

  private def specZipWithTask(collA: DeliteCollection[A], collB: DeliteCollection[A], func: (A, A) => A, pos: Int, total: Int) {
    val outm = out.asInstanceOf[DeliteCollection[A]]

    val chunk_size = collA.size / total
    val remainder = collA.size % total
    val sz = if (pos < remainder) chunk_size + 1 else chunk_size
    val off = if (pos < remainder) 0 else pos - remainder
    var idx = pos*(chunk_size+1) - off
    val end = idx + sz
    while (idx != end) {
      outm.dc_update(idx, func(collA.dc_apply(idx), collB.dc_apply(idx)))
      idx += 1
    }
  }
}
*/