package ppl.delite.dsl.optiml.unreliable

import ppl.delite.core.include._
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.core.{DeliteDSLType, DeliteCore}
import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.specialized.IntVectorImpl

private[optiml] class BestEffortIntVectorImpl extends IntVectorImpl with BestEffortVector[Int] {

  def this(row_vec: Boolean, len: Int) = {
    this()
    init(row_vec, len)
    policy = new DefaultBestEffortPolicy[Int](len)
    isComputed = true
  }

  private var policy : BestEffortPolicy[Int] = null

  def getPolicy = policy
  
  def setPolicy(p: BestEffortPolicy[Int]) = {
    policy = p
  }

  // should we skip an operation on this element?
  def skip(n: Int) : Boolean = {
    policy.skip(_data, n)
  }

  def always_update(n: Int, x: Int){
    _data(n) = x
  }

  override def update(n: Int, x: Int) = always_update(n, x)

  def be_update(n: Int, x: () => Int){
    //println("called best effort update")
    if (skip(n) == false){
      _data(n) = x()
    }
  }
  
}
