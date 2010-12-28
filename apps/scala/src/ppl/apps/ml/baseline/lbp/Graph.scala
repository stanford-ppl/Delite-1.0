package ppl.apps.ml.baseline.lbp

import collection.Set
import collection.mutable.{ArrayBuffer, Map}
import ppl.delite.dsl.optiml.{FifoScheduler, Scheduler}

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

  def neighbors(a: V): Set[V]

  def edgesOf(a: V): Set[E]

  def containsEdge(e: E): Boolean

  def containsVertex(v: V): Boolean

  class Vertex(v: V, g: Graph[V, E], f : (Vertex) => Unit, s: Scheduler[V]) {
    val data = v
    var edgeAccess = false
    var neighborAccess = false

    val es = g.edgesOf(v)
    val nbrs = g.neighbors(v)

    def edges = {
      edgeAccess = true
      es
    }

    def neighbors = {
      neighborAccess = true
      nbrs
    }

    def addTask(c: Graph.Consistency.Consistency, v: V) {
      s.addTask(v)
    }
  }

  def untilConverged(c: Graph.Consistency.Consistency)(f: (Vertex) => Unit) {
    val sched = new FifoScheduler[V]
    sched.addTasks(vertexList)

    while (sched.hasTask) {
      val v = sched.getTask()

      val scope = new Vertex(v, this, f, sched)
      f(scope)
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

  def neighbors(v: V) = {
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

  def neighbors(v: V) = {
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


// Edge that can pass message back and forth between two vertices
class MessageEdge[V, E](val v1: V, val message1: E, val v2: V, val message2: E) {
  // In edge
  def in(v: V): E = {
    assert(v == v1 || v == v2)

    if (v == v1) message1 else message2
  }

  def in(v: Graph[V, MessageEdge[V, E]]#Vertex): E = {
    in(v.data)
  }

  // Out edge
  def out(v: V): E = {
    assert(v == v1 || v == v2)

    if (v == v1) message2 else message1
  }

  def out(v: Graph[V, MessageEdge[V, E]]#Vertex): E = {
    out(v.data)
  }

  def target(v: V): V = {
    assert(v == v1 || v == v2)

    if (v == v1) v2 else v1
  }

  def target(v: Graph[V, MessageEdge[V, E]]#Vertex): V = {
    target(v.data)
  }
}


class MessageGraph[V, E] extends UndirectedGraphImpl[V, MessageEdge[V, E]] {
  def addEdge(e1: E, e2: E, a: V, b: V): Unit = {
    addEdge(new MessageEdge(a, e1, b, e2), a, b)
  }
}