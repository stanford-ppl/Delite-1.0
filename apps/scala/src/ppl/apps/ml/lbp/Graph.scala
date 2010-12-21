package ppl.apps.ml.lbp

import collection.Set
import collection.mutable.{ArrayBuffer, Map}
import ppl.delite.dsl.optiml.{Scheduler, FifoScheduler}

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 11/29/10
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Graph {
  object Consistency extends Enumeration {
    type Consistency = Value
    val Auto, Vertex, Edge, Full = Value
  }
}

trait Graph[V, E] {
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

  def untilConverged(c: Graph.Consistency.Consistency, sched: Scheduler[V] = new FifoScheduler[V])(f: (Vertex) => Unit) {
    sched.addTasks(vertexList)

    while (sched.hasTask) {
      val vertexData = sched.getTask()

      val vertex = new Vertex(vertexData)
      f(vertex)

      if(vertex.tasks.size > 0) {
        sched.addTasks(vertex.tasks)
      }
    }
  }
}

trait DirectedGraph[V, E] extends Graph[V, E] {
}

trait UndirectedGraph[V, E] extends Graph[V, E] {
}

class UndirectedGraphImpl[V, E] extends UndirectedGraph[V, E] {
  val edge_map = Map[E, (V, V)]()
  val vertices_edge_map = Map[(V, V), E]()
  val vertex_edge_list = Map[V, Set[(E, V)]]()
  val vertex_list = ArrayBuffer[V]()

  def vertexList() = vertex_list.toList

  def vertexSet() = vertex_edge_list.keySet

  def edgeSet() = edge_map.keySet

  def neighborsOf(v: V) = {
    if (!vertex_edge_list.contains(v)) {
      Set()
    }
    else {
      vertex_edge_list(v) map (_._2)
    }
  }

  def edgesOf(v: V) = {
    if (!vertex_edge_list.contains(v)) {
      Set()
    }
    else {
      vertex_edge_list(v) map (_._1)
    }
  }

  def adjacent(a: V, b: V) = vertices_edge_map.contains((a, b))

  def addVertex(v: V) = {
    if (!vertex_edge_list.contains(v)) {
      vertex_list += v
      vertex_edge_list(v) = Set()
    }
  }

  def addEdge(e: E, a: V, b: V) = {
    if (!edge_map.contains(e)) {
      vertices_edge_map((a, b)) = e
      vertex_edge_list(a) += ((e, b))
      vertex_edge_list(b) += ((e, a))
    }
  }

  def containsEdge(e: E) = edge_map.contains(e)

  def containsVertex(v: V) = vertex_edge_list.contains(v)

  def removeEdge(a: V, b: V): Unit = {
    if (vertices_edge_map.contains((a, b))) {
      val e = vertices_edge_map((a, b))

      vertices_edge_map.remove(edge_map(e))
      vertex_edge_list(a) -= ((e, b))
      vertex_edge_list(b) -= ((e, a))
      edge_map.remove(e)
    }
  }
}

class DirectedGraphImpl[V, E] extends DirectedGraph[V, E] {
  val edge_map = Map[E, (V, V)]()
  val vertices_edge_map = Map[(V, V), E]()

  val in_edge_list = Map[V, Set[(E, V)]]()
  val out_edge_list = Map[V, Set[(E, V)]]()

  def vertexList() = in_edge_list.keySet.toList

  def vertexSet() = in_edge_list.keySet

  def edgeSet() = edge_map.keySet

  def edgesOf(v: V) = {
    if (!out_edge_list.contains(v)) {
      Set()
    }
    else {
      out_edge_list(v) map (_._1)
    }
  }

  def neighborsOf(v: V) = {
    if (!out_edge_list.contains(v)) {
      Set()
    }
    else {
      out_edge_list(v) map (_._2)
    }
  }

  def adjacent(a: V, b: V) = vertices_edge_map.contains((a, b))

  def addVertex(v: V) = {
    if (!in_edge_list.contains(v)) {
      in_edge_list(v) = Set()
      out_edge_list(v) = Set()
    }
  }

  def addEdge(e: E, a: V, b: V) = {
    if (!edge_map.contains(e)) {
      vertices_edge_map((a, b)) = e
      out_edge_list(a) += ((e, b))
      in_edge_list(b) += ((e, a))
    }
  }

  def containsEdge(e: E) = edge_map.contains(e)

  def containsVertex(v: V) = in_edge_list.contains(v)

  def removeEdge(a: V, b: V): Unit = {
    if (vertices_edge_map.contains((a, b))) {
      val e = vertices_edge_map((a, b))

      vertices_edge_map.remove(edge_map(e))
      out_edge_list(a) -= ((e, b))
      in_edge_list(b) -= ((e, a))
      edge_map.remove(e)
    }
  }
}