package ppl.delite.core.executor.splitter

import ppl.delite.core.executor.{RootSet,ActiveBlock}


abstract class SimpleDeviceSplitter extends ActiveBlock[RootSet] {

  def process(rs: RootSet) {
    println("RootSet submitted to Device Splitter")
  }
  
  def CPUTarget: ActiveBlock[RootSet]
  def GPUTarget: ActiveBlock[RootSet]
}