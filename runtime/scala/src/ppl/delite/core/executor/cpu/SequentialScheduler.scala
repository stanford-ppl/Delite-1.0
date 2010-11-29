package ppl.delite.core.executor.cpu

import ppl.delite.core.executor.DeliteExecutorConfig
import ppl.delite.core.DeliteDSLType

/**
 * Author: Kevin J. Brown
 * Date: May 8, 2010
 * Time: 10:13:27 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class SequentialScheduler extends DeliteExecutorConfig {

  println("Sequential Scheduler is starting")

  def submit(proxy: DeliteDSLType) = process(proxy)

  def process(proxy: DeliteDSLType) {
    proxy.scheduled = true
    proxy.cvalue = proxy.op.seq
    proxy.concretize
    proxy.isComputed = true
    proxy.op = null

    proxy.synchronized {
      if (proxy.forcing)
        proxy.notify
    }
  }

  def force { }

  def start { }

  def shutdown { }
  
}