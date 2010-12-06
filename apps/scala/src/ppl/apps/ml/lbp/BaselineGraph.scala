package ppl.apps.ml.lbp

import collection.mutable.Queue
import collection.mutable.Map
import collection.Set

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 11/29/10
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object BaselineGraph {
    class Scope[V, E](v : V, es: Set[E], nbrs: Set[V], g: BaselineGraph[V, E], t: Queue[V]) {
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
        t += v
      }
    }

    def runUpdateFunction[V, E](g: BaselineGraph[V,E], c: Consistency.Consistency)(f: (V, Scope[V, E]) => Unit) {
      val tasks = Queue[V]()
      tasks ++= g.getVertices

      while(!tasks.isEmpty) {
        val v = tasks.dequeue

        val scope = new Scope(v, g.edges(v), g.neighbors(v), g, tasks)
        f(v, scope)
      }
    }

    // def untilConverged[V, E](c: Consistency.Consistency, f: (V, Scope[V, E]) => Unit, max_iter: Int = 1000) (block: V => V)

    object Consistency extends Enumeration {
      type Consistency = Value
      val Auto, Vertex, Edge, Full = Value
    }
}

trait BaselineGraph[V, E] {
  def getVertices() : Set[V]
  def getEdges() : Set[E]

  def addVertex(v: V)
  def addEdge(e: E, a: V, b: V)
  def removeEdge(a: V, b: V)

  def adjacent(a: V, b: V) : Boolean
  def neighbors(a: V) : Set[V]
  def edges(a: V) : Set[E]

  def containsEdge(e: E) : Boolean
  def containsVertex(v: V) : Boolean
  /*
  run update function (consistencymodel: default is auto compiler picks based on what you access) (scope)
  {
    block that can access things in scope, not the main graph.
     scope.vertex
     scope.edges
     scope.neighbors
   }

   map reduce part:
   sync(
      fold(vertex, value)
      merge(value, value)
   )

   def untilConverged( maxiter ) (consistencymodel: default compiler picks) (scope) {
       run update function(consistency model)
       value = sync

       if( test(value) ) {
          break;
       }
   }
    */
}

class BaselineGraphImpl[V, E] extends BaselineGraph[V,E] {
  val edge_map = Map[E, (V, V)]()
  val vertices_edge_map = Map[(V, V), E]()
  val vertex_edge_list = Map[V, Set[(E, V)]]()

  val tasks = Queue[V]()

  def getVertices() = vertex_edge_list.keySet
  def getEdges() = edge_map.keySet

  def neighbors(v: V) = {
    if(!vertex_edge_list.contains(v)) {
      Set()
    }
    else {
      vertex_edge_list(v) map(_._2)
    }
  }

  def edges(v: V) = {
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