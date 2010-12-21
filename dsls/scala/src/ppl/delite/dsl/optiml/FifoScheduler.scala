package ppl.delite.dsl.optiml

import collection.mutable.{Queue, Set => MSet}

/**
 * Created by IntelliJ IDEA.
 * User: mmwu
 * Date: Dec 20, 2010
 * Time: 3:11:32 PM
 * To change this template use File | Settings | File Templates.
 */

class FifoScheduler[V] extends Scheduler[V] {
  val taskQueue = Queue[V]()
  val taskSet = MSet[V]()

  def addTasks(tasks: Traversable[V]) = {
    for (v <- tasks) {
      addTask(v)
    }
  }

  def addTask(v: V) = {
    if (!taskSet.contains(v)) {
      taskQueue += v
      taskSet += v
    }
  }

  def hasTask(): Boolean = {
    !taskQueue.isEmpty
  }

  def getTask() : V = {
    val v = taskQueue.dequeue
    taskSet -= v
    v
  }
}