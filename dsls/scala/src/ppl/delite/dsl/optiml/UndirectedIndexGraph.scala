package ppl.delite.dsl.optiml

import collection.mutable.{ArrayBuffer, Map}

/**
 * Created by IntelliJ IDEA.
 * User: mmwu
 * Date: Dec 27, 2010
 * Time: 3:36:13 PM
 * To change this template use File | Settings | File Templates.
 */

object UndirectedIndexGraph {
}

trait UndirectedIndexGraph[V, E] extends IndexGraph[V, E] {
}

class UndirectedIndexGraphImpl[V, E] extends UndirectedIndexGraph[V, E] {
  protected var edge_map = Map[E, (Int, Int)]()
  protected var vertices_edge_map = Map[(Int, Int), E]()
  protected var vertex_edge_list = ArrayBuffer[List[(E, Int)]]()
  protected var vertex_list = ArrayBuffer[V]()
  protected var vertex_map = Map[V, Int]()

  def vertex(v: Int) = vertex_list(v)

  def vertices() = vertex_list

  def edges() = edge_map.keySet

  def neighborIdsOf(v: Int) = {
    if (v >= vertex_list.length) {
      Seq()
    }
    else {
      vertex_edge_list(v) map (_._2)
    }
  }

  def edgesOf(v: Int) = {
    if (v >= vertex_list.length) {
      Seq()
    }
    else {
      vertex_edge_list(v) map (_._1)
    }
  }

  def adjacent(a: Int, b: Int) = vertices_edge_map.contains((a, b))

  def addVertex(v: V) = {
    val i = vertex_list.length

    assert(!vertex_list.contains(v))

    vertex_map(v) = i
    vertex_list += v
    vertex_edge_list += List()
    i
  }

  def addEdge(e: E, a: Int, b: Int) = {
    if (!edge_map.contains(e)) {
      vertices_edge_map((a, b)) = e
      vertex_edge_list(a) ::= ((e, b))
      vertex_edge_list(b) ::= ((e, a))
    }
  }

  def containsEdge(e: E) = edge_map.contains(e)

  def containsVertex(v: Int) = {v < vertex_list.length}
  def containsVertex(v: V) = vertex_map.contains(v)

  def sort() : Unit = {
    for(i <- 0 until vertex_list.length) {
      vertex_edge_list(i) = vertex_edge_list(i).sortBy{_._2}
    }
    _sorted = true
  }
}