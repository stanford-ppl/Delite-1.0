package ppl.delite.core.executor.cpu

import ppl.delite.core.executor.ActiveBlock
import ppl.delite.core.DeliteDSLType

abstract class SimpleThreadPoolManager extends ActiveBlock[DeliteDSLType] {
  def process(proxy: DeliteDSLType) {
    println("Submitting OP to CPU Thread Pool Manager")
  }

  //stub
  def start { }

  //stub
  def shutdown { }
    
}