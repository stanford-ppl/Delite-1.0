package ppl.apps.ml.baseline.lbp

import ppl.apps.ml.baseline.lbp.Graph.Scope

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 12/05/2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */


class TestVertex(var value: Int = 0)

class TestEdge(var value: Int = 0)

object GraphTest {
  def main(args: Array[String]) : Unit = {
    val g = new GraphImpl[TestVertex, TestEdge]()

    val vs = List(new TestVertex(1), new TestVertex(2), new TestVertex(1))
    val es = List(new TestEdge(3), new TestEdge(0))

    for(v <- vs) g.addVertex(v)

    g.addEdge(es(0), vs(0), vs(1))
    g.addEdge(es(1), vs(1), vs(2))

    def updateFunction(v: TestVertex, scope: Scope[TestVertex, TestEdge]) : Unit = {
       v.value += 1;

      if(v.value < 3) {
        scope.enqueueUpdateFunctionVertex(Graph.Consistency.Auto, v) (updateFunction)
      }
    }

    Graph.runUpdateFunction(g, Graph.Consistency.Auto) (updateFunction)

    val values = g.vertexSet.toList map {_.value}
    println(values)
    val total = values.foldLeft(0)((acc, value) => acc + value)
    println(total)

    println(vs(0).value)
    println(vs(1).value)
    println(vs(2).value)
  }
}
