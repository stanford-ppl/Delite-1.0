package ppl.delite.dsl.optiml

/**
 * Created by IntelliJ IDEA.
 * User: mmwu
 * Date: Dec 27, 2010
 * Time: 2:51:30 PM
 * To change this template use File | Settings | File Templates.
 */

object MessageGraph {
  class ProxyFactory[V, E] extends Graph.ProxyFactory[V,MessageEdge[V, E]] {
    def newProxy() : MessageGraph[V,E] = {
      new MessageGraph[V,E]
    }
  }
}

class MessageGraph[V, E] extends UndirectedGraphImpl[V, MessageEdge[V, E]] {
  import MessageGraph._

  def addEdge(e1: E, e2: E, a: V, b: V): Unit = {
    addEdge(new MessageEdge(a, e1, b, e2), a, b)
  }

  class MessageVertex(v: V) extends super.Vertex(v) {
    def target(e: MessageEdge[V, E]): V = {
      e.target(data)
    }
  }
}