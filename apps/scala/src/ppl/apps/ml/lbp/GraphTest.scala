package ppl.delite.dsl.optiml

import ppl.apps.ml.lbp.BaselineGraph.Scope
import ppl.apps.ml.lbp.{BaselineGraph, BaselineGraphImpl}

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
  def main = {
    val g = new BaselineGraphImpl[TestVertex, TestEdge]()

    val vs = List(new TestVertex(1), new TestVertex(2), new TestVertex(1))
    val es = List(new TestEdge(3), new TestEdge(0))

    for(v <- vs) g.addVertex(v)

    g.addEdge(es(0), vs(0), vs(1))
    g.addEdge(es(1), vs(1), vs(2))

    def updateFunction(v: TestVertex, scope: Scope[TestVertex, TestEdge]) : Unit = {
       v.value += 1;

      if(v.value < 3) {
        scope.enqueueUpdateFunctionVertex(BaselineGraph.Consistency.Auto, v) (updateFunction)
      }
    }

    BaselineGraph.runUpdateFunction(g, BaselineGraph.Consistency.Auto) (updateFunction)

    val values = g.getVertices map {_.value}
    val total = values.foldLeft(0)((acc, value) => acc + value)
  }
}
