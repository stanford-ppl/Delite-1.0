package ppl.apps.ml.baseline.lbp

import collection.mutable.Queue
import collection.mutable.Map
import collection.Set
import collection.mutable.{Set => MSet}

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 11/29/10
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Graph {
    class Scope[V, E](v : V, es: Set[E], nbrs: Set[V], g: Graph[V, E], t: Queue[V], ts: MSet[V]) {
      val vertexData = v
      var edgeAccess = false
      var neighborAccess = false

      def edgeData = {
        edgeAccess = true
        es
      }

      def neighborData = {
        neighborAccess = true
        nbrs
      }

      def enqueueUpdateFunctionVertex(c: Consistency.Consistency, v: V)(f: (V, Scope[V, E])  => Unit) {
        if(!ts.contains(v)) {
          t += v
          ts += v
        }
      }
    }

  // Fix this scheduler, don't allow repeated tasks for one vertex
    def runUpdateFunction[V, E](g: Graph[V,E], c: Consistency.Consistency)(f: (V, Scope[V, E]) => Unit) {
      val taskSet = MSet[V]()
      val tasks = Queue[V]()
      tasks ++= g.vertexSet
      taskSet ++= g.vertexSet

      while(!tasks.isEmpty) {
        val v = tasks.dequeue
        taskSet -= v

        val scope = new Scope(v, g.edgesOf(v), g.neighbors(v), g, tasks, taskSet)
        f(v, scope)
      }
    }

    def untilConverged[V, E](g: Graph[V,E], c: Consistency.Consistency, f: (V, Scope[V, E]) => Unit, max_iter: Int = 1000, cf: Graph[V, E] => Boolean) {
      var iter = 0

      do {
        runUpdateFunction(g, c)(f)
        iter += 1
      } while(!cf(g) && iter < max_iter)
    }

    object Consistency extends Enumeration {
      type Consistency = Value
      val Auto, Vertex, Edge, Full = Value
    }
}

trait Graph[V, E] {
  def vertexSet() : Set[V]
  def edgeSet() : Set[E]

  def addVertex(v: V)
  def addEdge(e: E, a: V, b: V)
  def removeEdge(a: V, b: V)

  def adjacent(a: V, b: V) : Boolean
  def neighbors(a: V) : Set[V]
  def edgesOf(a: V) : Set[E]

  def containsEdge(e: E) : Boolean
  def containsVertex(v: V) : Boolean
}

trait DirectedGraph[V, E] extends Graph[V, E] {
}

trait UndirectedGraph[V, E] extends Graph[V, E] {
}

class UndirectedGraphImpl[V, E] extends UndirectedGraph[V,E] {
  val edge_map = Map[E, (V, V)]()
  val vertices_edge_map = Map[(V, V), E]()
  val vertex_edge_list = Map[V, Set[(E, V)]]()

  val tasks = Queue[V]()

  def vertexSet() = vertex_edge_list.keySet
  def edgeSet() = edge_map.keySet

  def neighbors(v: V) = {
    if(!vertex_edge_list.contains(v)) {
      Set()
    }
    else {
      vertex_edge_list(v) map(_._2)
    }
  }

  def edgesOf(v: V) = {
    if(!vertex_edge_list.contains(v)) {
      Set()
    }
    else {
      vertex_edge_list(v) map(_._1)
    }
  }

  def adjacent(a: V, b: V) = vertices_edge_map.contains((a,b))

  def addVertex(v: V) = {
    if(!vertex_edge_list.contains(v)) {
      vertex_edge_list(v) = Set()
    }
  }

  def addEdge(e: E, a: V, b: V) = {
    if(!edge_map.contains(e)) {
      vertices_edge_map((a,b)) = e
      vertex_edge_list(a) += ((e, b))
      vertex_edge_list(b) += ((e, a))
    }
  }

  def containsEdge(e: E) = edge_map.contains(e)
  def containsVertex(v: V) = vertex_edge_list.contains(v)

  def removeEdge(a: V, b: V) : Unit = {
    if(vertices_edge_map.contains((a, b))) {
      val e = vertices_edge_map((a, b)) 

      vertices_edge_map.remove(edge_map(e))
      vertex_edge_list(a) -= ((e, b))
      vertex_edge_list(b) -= ((e, a))
      edge_map.remove(e)
    }
  }
}

class DirectedGraphImpl[V, E] extends DirectedGraph[V,E] {
  val edge_map = Map[E, (V, V)]()
  val vertices_edge_map = Map[(V, V), E]()

  val in_edge_list = Map[V, Set[(E, V)]]()
  val out_edge_list = Map[V, Set[(E, V)]]()

  val tasks = Queue[V]()

  def vertexSet() = in_edge_list.keySet
  def edgeSet() = edge_map.keySet

  def edgesOf(v: V) = {
    if(!out_edge_list.contains(v)) {
      Set()
    }
    else {
      out_edge_list(v) map(_._1)
    }
  }

  def neighbors(v: V) = {
    if(!out_edge_list.contains(v)) {
      Set()
    }
    else {
      out_edge_list(v) map(_._2)
    }
  }

  def adjacent(a: V, b: V) = vertices_edge_map.contains((a,b))

  def addVertex(v: V) = {
    if(!in_edge_list.contains(v)) {
      in_edge_list(v) = Set()
      out_edge_list(v) = Set()
    }
  }

  def addEdge(e: E, a: V, b: V) = {
    if(!edge_map.contains(e)) {
      vertices_edge_map((a,b)) = e
      out_edge_list(a) += ((e, b))
      in_edge_list(b) += ((e, a))
    }
  }

  def containsEdge(e: E) = edge_map.contains(e)
  def containsVertex(v: V) = in_edge_list.contains(v)

  def removeEdge(a: V, b: V) : Unit = {
    if(vertices_edge_map.contains((a, b))) {
      val e = vertices_edge_map((a, b))

      vertices_edge_map.remove(edge_map(e))
      out_edge_list(a) -= ((e, b))
      in_edge_list(b) -= ((e, a))
      edge_map.remove(e)
    }
  }
}