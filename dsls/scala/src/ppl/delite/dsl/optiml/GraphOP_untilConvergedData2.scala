package ppl.delite.dsl.optiml

import ppl.delite.core.ops._
import ppl.delite.core.{DeliteDSLType, DeliteUnit, DeliteFunc, DeliteCollection}
import collection.mutable.{HashSet, ArrayBuffer}
import java.util.concurrent.ConcurrentHashMap
import Graph.Consistency
import java.util.concurrent.locks.{ReentrantReadWriteLock, ReentrantLock}

/**
 * Created by IntelliJ IDEA.
 * User: mmwu
 * Date: Dec 28, 2010
 * Time: 3:42:27 PM
 * To change this template use File | Settings | File Templates.
 */

class GraphOP_untilConvergedData2[V: ClassManifest, E](g: Graph[V, E], consistency: Consistency.Consistency, coll: DeliteCollection[V], func: Graph[V, E]#Vertex => Unit, locks: ConcurrentHashMap[V, ReentrantReadWriteLock]) extends DeliteOP[Vector[V]] {
  //val coll: DeliteCollection[V]

  //val out: Vector[V]

  final override def isChunkable = true

  override def getImmutableDeps = {
    val immDeps = Seq(coll)
    func match {
      case f: DeliteFunc => immDeps ++ f.deps
      case _ => immDeps
    }
  }

  override def getMutableDeps: Seq[DeliteDSLType] = Seq()

  protected var total = 1

  @volatile
  protected var chunkDone: Array[Boolean] = null

  @volatile
  protected var tasks: Array[Seq[V]] = null

  override def submitChunks(num_chunks: Int, submit: DeliteOP[DeliteUnit] => Unit) {
    chunkDone = new Array[Boolean](num_chunks - 1)
    tasks = new Array[Seq[V]](num_chunks)
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
    val localTasks = new ArrayBuffer[V]
    while (idx != end) {
      val data = coll.dc_apply(idx)
      val vertex = g.generateVertex(data)
      val vertices = g.consistencyVertices(vertex, consistency, locks)
      g.lockVertices(vertices)
      func(vertex)
      g.unlockVertices(vertices)
      localTasks ++= vertex.tasks
      idx += 1
    }

    tasks(0) = localTasks

    // Barrier
    var ci = 0
    val cend = if (chunkDone != null) chunkDone.length else 0
    while (ci != cend) {
      while (!chunkDone(ci)) {}
      ci += 1
    }

    val taskSet = new HashSet[V]

    for (taskList <- tasks) {
      for (task <- taskList) {
        taskSet += task
      }
    }

    val out = Vector[V](taskSet.size)

    var i = 0
    for (task <- taskSet) {
      out(i) = task
      i += 1
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
      var idx = pos * (chunk_size + 1) - off
      val end = idx + sz
      val localTasks = new ArrayBuffer[V]
      while (idx != end) {
        val data = coll.dc_apply(idx)
        val vertex = g.generateVertex(data)
        val vertices = g.consistencyVertices(vertex, consistency, locks)
        g.lockVertices(vertices)
        func(vertex)
        g.unlockVertices(vertices)
        localTasks ++= vertex.tasks
        idx += 1
      }

      tasks(pos) = localTasks
      chunkDone(pos - 1) = true
    }
  }
}