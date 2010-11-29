package ppl.delite.core.executor.cpu

import ppl.delite.core.executor.{RootSet,ActiveBlock}
import ppl.delite.core.DeliteDSLType

abstract class SequentialCPUScheduler extends ActiveBlock[RootSet] {

  def process(rs: RootSet) {
    println("RootSet submitted to Sequential CPU Scheduler");
  }

  def target: ActiveBlock[DeliteDSLType]
}