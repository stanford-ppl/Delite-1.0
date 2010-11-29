package ppl.delite.core.executor.dag

import ppl.delite.core.{DeliteDSLType}
import ppl.delite.core.executor.{OpHelper, ActiveBlock, RootSet}

/**
 * Author: Kevin J. Brown
 * Date: Apr 8, 2010
 * Time: 8:09:32 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

abstract class SimpleGraphBuilder extends ActiveBlock[DeliteDSLType] {

  def process(v: DeliteDSLType) { }

  def target: ActiveBlock[RootSet]
}