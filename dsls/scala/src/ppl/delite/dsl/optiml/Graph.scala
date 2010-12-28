package ppl.delite.dsl.optiml

import collection.Set
import collection.mutable.{ArrayBuffer, Map}
import ppl.delite.core.ops._
import ppl.delite.core.{DeliteProxyFactory, DeliteDSLType, Delite}

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 11/29/10
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Graph {
  abstract class ProxyFactory[V, E] extends DeliteProxyFactory[Graph[V,E]] {
    def newProxy() : Graph[V,E]
  }

  object Consistency extends Enumeration {
    type Consistency = Value
    val Auto, Vertex, Edge, Full = Value
  }

  case class OP_untilConverged[V, E](g: Graph[V, E], f: (Graph[V, E]#Vertex) => Unit, c: Consistency.Consistency, sched: Scheduler[V] = new FifoScheduler[V]) extends DeliteOP_MutableSingleTask[Graph[V, E]]()(g) {
    def task = {
      sched.addTasks(g.vertexList)

      while (sched.hasTask) {
        val vertexData = sched.getTask()

        val vertex = g.generateVertex(vertexData)
        f(vertex)

        if (vertex.tasks.size > 0) {
          sched.addTasks(vertex.tasks)
        }
      }

      g
    }
  }
}

trait Graph[V, E] extends DeliteDSLType {
  import Graph._

  //type DSLType = Graph[V,E]

  def vertexList(): List[V]

  def vertexSet(): Set[V]

  def edgeSet(): Set[E]

  def addVertex(v: V)

  def addEdge(e: E, a: V, b: V)

  def removeEdge(a: V, b: V)

  def adjacent(a: V, b: V): Boolean

  def neighborsOf(a: V): Set[V]

  def edgesOf(a: V): Set[E]

  def containsEdge(e: E): Boolean

  def containsVertex(v: V): Boolean

  class Vertex(v: V) {
    val data = v
    var edgeAccess = false
    var neighborAccess = false
    val tasks = new ArrayBuffer[V]

    val es = edgesOf(v)
    val nbrs = neighborsOf(v)

    def edges = {
      edgeAccess = true
      es
    }

    def neighbors = {
      neighborAccess = true
      nbrs
    }

    def addTask(v: V) {
      tasks += v
    }
  }

  def generateVertex(v: V): Vertex

  def untilConverged(c: Consistency.Consistency, sched: Scheduler[V] = new FifoScheduler[V])(f: (Graph[V, E]#Vertex) => Unit)(implicit pFact: Graph.ProxyFactory[V,E]) {
    Delite.run(OP_untilConverged[V,E](this, f, c, sched))
  }
}
