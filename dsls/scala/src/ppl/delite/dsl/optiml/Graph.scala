/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 11/29/10
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
/*

package ppl.delite.dsl.optiml

import ppl.delite.core._
import ppl.delite.core.ops._
import scala.collection.mutable.Queue

object Graph {
  protected[optiml] case class OP_updateFunction[V, E](g: Graph[V,E], func: V => Unit) extends DeliteOP_SingleTask[Graph[V,E]] {
    def task = {
      val vertices = getVertices

      for(v <- vertices) {
        func(v)
      }
    }
  }

  protected[optiml] case class OP_updateFunctionVertex[V, E](g: Graph[V,E], v: V, func: V => Unit) extends DeliteOP_SingleTask[Graph[V,E]] {
    def task = {
      func(v)
    }
  }

  protected[optiml] case class OP_map[V, E, B](g: Graph[V,E], func: V => B) extends DeliteOP_Map[V, B, Vector[B]] {
    val vertices = g.getVertices.toList
    var tcoll = Vector[V](false, vertices.length)
    for (i <- 0 until vertices.length){
      tcoll(i) = vertices(i)
    }

    val coll = tcoll
  }
}

object Consistency extends Enumeration {
  type Consistency = Value
  val Auto, Vertex, Edge, Full = Value
}

trait Graph[V, E] extends DeliteDSLType with Cloneable[Graph[V, E]] {
  def apply(i: Int) : V = getVertex
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

  def runUpdateFunction(c: Consistency.Consistency)(f: V => Unit)
  def enqueueUpdateFunctionVertex(c: Consistency.Consistency, v: V)(f: V => Unit)

  def untilConverged(c: Consistency.Consistency, f: V => Unit, max_iter: Int = 1000) (block: V => V)

  class Scope(v : V, es: Set[E], nbrs: Set[V]) {
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
  }
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

class EasyGraph[V, E] extends Graph[V,E] {
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
      vertex_edge_list(v) map(_(2))
    }
  }

  def edges(v: V) = {
    if(!vertex_edge_list.contains(v)) {
      Set()
    }
    else {
      vertex_edge_list(v) map(_(1))
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
      vertices_edge_list(a) ::= (e, b)
      vertices_edge_list(b) ::= (e, a)
    }
  }

  def removeEdge(e: E, a: V, b: V) = {
    if(edge_map.contains(e)) {
      val a, b = edge_map(e)

      vertices_edge_map.remove(edge_map(e))
      vertices_edge_list(a).remove((e, b))
      vertices_edge_list(b).remove((e, a))
      edge_map.remove(e)
    }
  }

  def runUpdateFunction(c: Consistency.Consistency)(f: V => Unit) = {
      tasks ++= getVertices

      while(!tasks.isEmpty) {
        v = tasks.dequeue
        f(v)
      }
  }

  def enqueueUpdateFunctionVertex(c: Consistency.Consistency, v: V)(f: V => Unit) = {
      tasks ++= v
  }
}

class DeliteGraph[V, E] extends Graph[V, E] {

} */