package ppl.delite.dsl.optiml

/**
 * Created by IntelliJ IDEA.
 * User: mmwu
 * Date: Dec 27, 2010
 * Time: 2:52:27 PM
 * To change this template use File | Settings | File Templates.
 */

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
