package ppl.delite.dsl.optiml

import collection.mutable.HashMap
import collection.mutable.PriorityQueue

/**
 * Created by IntelliJ IDEA.
 * User: mikemwu
 * Date: Dec 19, 2010
 * Time: 10:28:33 PM
 * To change this template use File | Settings | File Templates.
 */
class PriorityDict[T]() {
  class ItemPriority(val item: T, var priority: Double)
  implicit object ItemPriorityOrdering extends Ordering[ItemPriority] {
    def compare(a: ItemPriority, b: ItemPriority) = {
      java.lang.Double.compare(a.priority - b.priority, 0.0)
    }
  }

  private val queue = new PriorityQueue[ItemPriority]
  private val priorityMap = new HashMap[T, Double]

  def insert(item: T, priority: Double) = {
    if (priorityMap.contains(item)) {
      reprioritize(item, priority)
    }
    else {
      priorityMap(item) = priority
      queue += new ItemPriority(item, priority)
    }
  }

  def promoteInsert(item: T, priority: Double) = {
    if (priorityMap.contains(item)) {
      if(priorityMap(item) < priority) {
        reprioritize(item, priority)
      }
    }
    else {
      priorityMap(item) = priority
      queue += new ItemPriority(item, priority)
    }
  }

  def reprioritize(item: T, priority: Double) = {
    // Don't remove old item, too much work
    if (queue.length < 2 * priorityMap.size)
      queue += new ItemPriority(item, priority)
    else {
      // When the heap grows larger than 2 * len(self), rebuild from scratch
      queue.clear()
      queue ++= priorityMap.iterator.map((e: (T, Double)) => new ItemPriority(e._1, e._2))
    }
  }

  def max(): T = {
    var max : ItemPriority = queue.max
    if (!priorityMap.contains(max.item) || priorityMap(max.item) != max.priority) {
      max = queue.dequeue()
    }
    max.item
  }

  def dequeue(): T = {
    var max = queue.dequeue()
    if (!priorityMap.contains(max.item) || priorityMap(max.item) != max.priority) {
      max = queue.dequeue()
    }

    priorityMap.remove(max.item)
    max.item
  }
}
