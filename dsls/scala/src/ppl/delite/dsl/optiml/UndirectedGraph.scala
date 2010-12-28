package ppl.delite.dsl.optiml

import collection.mutable.{ArrayBuffer, Map}

/**
 * Created by IntelliJ IDEA.
 * User: mmwu
 * Date: Dec 27, 2010
 * Time: 3:36:13 PM
 * To change this template use File | Settings | File Templates.
 */

object UndirectedGraph {
  class ProxyFactory[V, E] extends Graph.ProxyFactory[V,E] {
    def newProxy() : UndirectedGraph[V,E] = {
      new UndirectedGraphImpl[V,E]
    }
  }
}

trait UndirectedGraph[V, E] extends Graph[V, E] {
}

class UndirectedGraphImpl[V, E] extends UndirectedGraph[V, E] {
  override type DSLType = UndirectedGraphImpl[V,E]

  protected var edge_map = Map[E, (V, V)]()
  protected var vertices_edge_map = Map[(V, V), E]()
  protected var vertex_edge_list = Map[V, Set[(E, V)]]()
  protected var vertex_list = ArrayBuffer[V]()

  override def concretize = {
    edge_map = cvalue.edge_map
    vertices_edge_map = cvalue.vertices_edge_map
    vertex_edge_list = cvalue.vertex_edge_list
    vertex_list = cvalue.vertex_list
    cvalue = this
  }

  def vertexList() = vertex_list.toList

  def vertexSet() = {
    force
    vertex_edge_list.keySet
  }

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

  def generateVertex(v: V): Vertex = {
    new Vertex(v)
  }
}
