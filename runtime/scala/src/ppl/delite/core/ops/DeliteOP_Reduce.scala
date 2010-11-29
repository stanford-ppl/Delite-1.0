package ppl.delite.core.ops

import ppl.delite.core.{DeliteUnit, DeliteCollection, DeliteDSLType}

/**
 * Author: Kevin J. Brown
 * Date: Apr 18, 2010
 * Time: 11:56:11 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

abstract class DeliteOP_Reduce[A,B <: DeliteDSLType](implicit conv: A => B) extends DeliteOP[B] {

  //here the main OP must be the final reduction step and chunk creation must allocate tree levels
  //maintain internal state for reduction steps: tasks have fixed references, submitChunks assigns the references to data structures as appropriate

  val coll: DeliteCollection[A]

  final override val associative = true

  final override def isChunkable = true

  def func: (A,A) => A

  override def getImmutableDeps = Seq(coll)
  override def getMutableDeps = null

  @volatile
  protected var chunkDone: Array[Boolean] = null

  // stores the intermediate results of the reduction tree
  protected var last: IDCollection[A] = null
  protected var numChunks: Int = 1
  protected var TREE_REDUCE = false

  override def submitChunks(num_chunks: Int, submit: DeliteOP[DeliteUnit] => Unit) {
    if (num_chunks == 1) return
    assert(num_chunks % 2 == 0)
    numChunks = num_chunks
    if (numChunks > 8) TREE_REDUCE = true
    last = new IDCollection[A](num_chunks)
    chunkDone = new Array[Boolean](num_chunks)
    var i = 1
    while (i != num_chunks) {
      submit(new SlaveChunk(coll, last, i, num_chunks))
      i += 1
    }
  }

  def task = {
    if (last == null) last = new IDCollection[A](1)
    reduceTask(coll, last, 0, numChunks)

    var acc : A = null.asInstanceOf[A]

    if (numChunks == 1){
      acc = last.dc_apply(0)
    }
    else {
      if (TREE_REDUCE){
        var lastPos = numChunks / 2
        while (!chunkDone(lastPos)) {}
        acc = func(last.dc_apply(0), last.dc_apply(lastPos))
      } else {
        var ci = 0
        val cend = if (chunkDone != null) chunkDone.length else 0
        while (ci != cend) {
          while (!chunkDone(ci)) { }
          ci += 1
        }

        acc = last.dc_apply(0)
        var idx = 1
        val end = last.size
        while (idx != end) {
          acc = func(acc, last.dc_apply(idx))
          idx += 1
        }
      }
    }

    conv(acc)
  }

  private class SlaveChunk(coll: DeliteCollection[A], out: DeliteCollection[A], pos: Int, total: Int) extends DeliteOP[DeliteUnit] {

    override def getImmutableDeps = null
    override def getMutableDeps = null

    def task = reduceTask(coll, out, pos, total)
  }

  // for reduction level l (0-based), chunk pos is still in if pos % 2^l == 0
  // if still in, can proceed when chunkDone(pos+2^(l-1)) == true
  // store intermediate results back into out, which contains num_chunks elements

  // this is the simplest version of a tree reduction: always reduces with neighbor
  // (half the threads drop out at every stage -- no additional batching)
  private def reduceTask(coll: DeliteCollection[A], out: DeliteCollection[A], pos: Int, total: Int) {
    // level 0, everyone processes their input and produces a result
    val chunk_size = coll.size / total
    val remainder = coll.size % total
    val sz = if (pos < remainder) chunk_size + 1 else chunk_size
    val off = if (pos < remainder) 0 else pos - remainder
    var idx = pos*(chunk_size+1) - off
    val end = idx + sz
    if (idx >= end) { //no work for this chunk => escape
      chunkDone(pos) = true
      return
    }

    var acc = coll.dc_apply(idx)
    idx += 1
    while (idx != end) {
      acc = func(acc, coll.dc_apply(idx))
      idx += 1
    }
    out.dc_update(pos, acc)

    // remaining levels
    if (TREE_REDUCE) {
      var lev2 = 2
      var numRemaining = total / 2
      while ((pos % lev2 == 0) && (numRemaining > 1)){
        // barrier
        val neighbor = pos + lev2/2
        while (!chunkDone(neighbor)) {}

        var acc = func(out.dc_apply(pos), out.dc_apply(neighbor))
        out.dc_update(pos, acc)

        lev2 *= 2
        numRemaining = numRemaining / 2
      }
    }

    if (total > 1) chunkDone(pos) = true
  }

  protected class IDCollection[A](val size: Int) extends DeliteCollection[A] {
    private val elements = new Array[Any](size)

    def dc_apply(i: Int) = elements(i).asInstanceOf[A]
    def dc_update(i: Int, x: A) { elements(i) = x }

    def chunk(start: Int, end: Int) = throw new RuntimeException
    type DSLType = IDCollection[A]
    type Shape = Nothing
    //override def force = this
  }

}
