package ppl.delite.core.executor.configs

import ppl.delite.core.executor.dag.SimpleGraphBuilder
import ppl.delite.core.executor.splitter.SimpleDeviceSplitter
import ppl.delite.core.executor.cpu.{SimpleThreadPoolManager, SequentialCPUScheduler}
import ppl.delite.core.DeliteDSLType
import ppl.delite.core.executor.{DeliteExecutorConfig, RootSet, CoupledBlock}

object CoupledConfig extends DeliteExecutorConfig {

  def submit(proxy: DeliteDSLType) = GB.submit(proxy)

  def start = cpu_workers.start

  def shutdown = cpu_workers.shutdown

  //stub: should really call cpu_scheduler.force (at least)
  def force = { }

  object GB extends SimpleGraphBuilder with CoupledBlock[DeliteDSLType] {
    def target = splitter
  }

  object splitter extends SimpleDeviceSplitter with CoupledBlock[RootSet] {
    def GPUTarget = null
    def CPUTarget = cpu_scheduler
  }

  object cpu_scheduler extends SequentialCPUScheduler with CoupledBlock[RootSet] {
    def target = cpu_workers
  }

  object cpu_workers extends SimpleThreadPoolManager with CoupledBlock[DeliteDSLType]

}