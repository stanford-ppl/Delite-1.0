package ppl.delite.dsl.optiml

import collection.Set
import ppl.delite.core.ops._
import ppl.delite.core.{DeliteProxyFactory, DeliteDSLType, Delite, DeliteUnit, DeliteFunc, DeliteCollection}
import collection.mutable.{Queue, ArrayBuffer, Map, HashSet}
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.ConcurrentHashMap

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 11/29/10
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object IndexGraph {
  object Consistency extends Enumeration {
    type Consistency = Value
    val Auto, Vertex, Edge, Full = Value
  }

  case class OP_untilConvergedSingle[V, E](g: Graph[V, E], f: (Graph[V, E]#Vertex) => Unit, c: Consistency.Consistency, sched: Scheduler[V] = new FifoScheduler[V]) extends DeliteOP_SingleTask[Graph[V, E]]() {
    val locks = new ConcurrentHashMap[V, ReentrantLock]

    def task = {
      sched.addTasks(g.vertexList)

      while (sched.hasTask) {
        val vertexData = sched.getTask()

        val vertex = g.generateVertex(vertexData)
        val sortedVertices = g.fullVertices(vertex)
        f(vertex)

        if (vertex.tasks.size > 0) {
          sched.addTasks(vertex.tasks)
        }
      }

      g
    }
  }


}

trait IndexGraph[V, E] {
  def vertex(v: Int): V
  def vertices(): Seq[V]

  def edges(): Set[E]

  def addVertex(v: V) : Int

  def addEdge(e: E, a: Int, b: Int)

  def adjacent(a: Int, b: Int): Boolean

  def neighborIdsOf(a: Int): Seq[Int]

  def edgesOf(a: Int): Seq[E]

  def containsEdge(e: E): Boolean

  def containsVertex(v: V): Boolean

  protected var _sorted = false
  def sorted: Boolean = _sorted
  def sort(): Unit

  class Vertex(val id: Int) {
    val data = vertex(id)
    var edgeAccess = false
    var neighborAccess = false
    val tasks = new ArrayBuffer[Int]

    val es = edgesOf(id)
    val nbrs = neighborIdsOf(id)

    def edges = {
      edgeAccess = true
      es
    }

    def neighbors = {
      neighborAccess = true
      nbrs
    }

    def addTask(id: Int) {
      tasks += id
    }
  }

  def sortedVertices(v: Vertex): Seq[Int] = {
    val vertices = new Array[Int](v.neighbors.size + 1)

    var i = 0
    var j = 0
    while(i + j < vertices.length) {
      if(j < 1 && v.id < v.neighbors(i)) {
        vertices(i) = v.id
        j += 1
      }
      else {
        vertices(i + j) = v.neighbors(i)
        i += 1
      }
    }
    vertices
  }

  def lockVerticesFull(vertices: Seq[Int], locks: Array[ReentrantLock]) = {
    var i = 0

    while (i < vertices.length) {
      locks(i).lock()
      i += 1
    }
  }

  def unlockVerticesFull(vertices: Seq[Int], locks: Array[ReentrantLock]) = {
    var i = vertices.length - 1

    while (i >= 0) {
      locks(i).unlock()
      i -= 1
    }
  }

  def untilConvergedTask(f: (Vertex) => Unit)(implicit mV: ClassManifest[V]): Unit = {
    if(!sorted) {
      sort()
    }

    implicit val proxyFactory = new Vector.ProxyFactory[Int]
    val locks = Array.fill[ReentrantLock](vertices.size)({new ReentrantLock})
    val queue = new Queue[Int]
    queue ++= (0 until vertices.size)

    case class OP_untilConvergedTask(v: Vertex) extends DeliteOP_SingleTask[Vector[Int]]() {
      def task = {
        val vertices = sortedVertices(v)
        lockVerticesFull(vertices, locks)
        f(v)
        unlockVerticesFull(vertices, locks)

        val tasks = Vector[Int](v.tasks.length)

        var i = 0
        while (i < v.tasks.length) {
          tasks(i) = v.tasks(i)
          i += 1
        }

        tasks
      }
    }

    while (!queue.isEmpty) {
      val newTasks = new ArrayBuffer[Vector[Int]]

      do {
        val id = queue.dequeue()

        val vertex = new Vertex(id)
        newTasks += Delite.run(OP_untilConvergedTask(vertex))
      } while (!queue.isEmpty)

      val taskSet = new HashSet[Int]

      for (local <- newTasks) {
        val len = local.length
        var i = 0
        while (i < len) {
          val v = local.dc_apply(i)
          if (!taskSet.contains(v)) {
            taskSet += v
            queue += v
          }
          i += 1
        }
      }
    }
  }
}