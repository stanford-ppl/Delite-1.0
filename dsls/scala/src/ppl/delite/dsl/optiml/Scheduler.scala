package ppl.delite.dsl.optiml

/**
 * Created by IntelliJ IDEA.
 * User: mmwu
 * Date: Dec 20, 2010
 * Time: 3:12:29 PM
 * To change this template use File | Settings | File Templates.
 */

trait Scheduler[V] {
  def addTasks(tasks: Traversable[V])

  def addTask(v: V)

  def hasTask() : Boolean

  def getTask() : V
}