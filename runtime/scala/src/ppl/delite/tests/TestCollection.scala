/* Description
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Feb 8, 2010
 * modified: Feb 8, 2010
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
package ppl.delite.tests

import ppl.delite.core.{DeliteCollectionFactory, DeliteProxyFactory, DeliteCollection}

object TestCollection{
  def apply[A : ClassManifest](len: Int) : TestCollection[A] = new TestCollectionImpl[A](len)
  
  class Proxy[T] extends TestCollection[T] {
    def apply(n: Int) : T = force.apply(n)
    def update(index: Int, x: T) = force.update(index,x)
    def size = force.size
  }

  class ProxyFactory[T] extends DeliteProxyFactory[TestCollection[T]] {
    def newProxy = new TestCollection.Proxy[T]
  }

  class Builder extends DeliteCollectionFactory[TestCollection] {
    def newInstance[A](size: Int, shape: Option[Any] = None)(implicit m: ClassManifest[A]) = TestCollection[A](size)
  }
}
  
trait TestCollection[T] extends DeliteCollection[T] {
  type DSLType = TestCollection[T]

  def dc_apply(n: Int) = apply(n)
  def dc_update(index: Int, x: T) = update(index,x)

  def apply(n: Int) : T
  def update(index: Int, x: T)
  def size : Int

  def chunk(start: Int, end: Int) = new Iterator[T] {
    private var index = start
    private val ceil = if (end+1 <= TestCollection.this.size) end+1 else TestCollection.this.size+1
    def hasNext = (index < ceil)
    def next = {
      index += 1
      TestCollection.this(index-1)
    }
  }

}

class TestCollectionImpl[T : ClassManifest](length: Int) extends TestCollection[T] {
  protected var _data = new Array[T](length)

  //override def force : TestCollection[T] = this

  def apply(n: Int) : T = {
     _data(n)
   }

   def update(index: Int, x: T) {
     _data(index) = x
   }

  def size = length
}
