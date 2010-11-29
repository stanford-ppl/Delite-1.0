package ppl.delite.core.executor

import java.util.concurrent.ArrayBlockingQueue
import ppl.delite.core.Config

trait DecoupledBlock[T] extends ActiveBlock[T] {
  val queue =  new ArrayBlockingQueue[T](Config.activeBlockDefaultQueueSize)

  def submit(v:T) = queue.put(v)

  val t = new Thread() {
    override def run() {
      while(true) {
        process(queue.take)
      }
    }
  }
  t.setDaemon(true)
  t.start

}