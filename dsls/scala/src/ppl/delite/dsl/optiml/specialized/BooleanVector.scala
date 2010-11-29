/* Description
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  May 5, 2010
 * modified: May 5, 2010
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
package ppl.delite.dsl.optiml.specialized

import ppl.delite.core.ops.DeliteOP_SingleTask
import ppl.delite.dsl.optiml.{ArithOps, Vector, Matrix}
import ppl.delite.core.appinclude._
import ppl.delite.dsl.primitive.DeliteBoolean
import ppl.delite.core.DeliteUnit

object BooleanVector {

  protected[optiml] case class OP_apply(val collA: BooleanVector, n: Int)
    extends DeliteOP_SingleTask[DeliteBoolean](collA){

    def task = {
      collA(n)
    }
  }

  protected[optiml] case class OP_update(val collA: BooleanVector, index: Int, x: Boolean)
    extends DeliteOP_SingleTask[DeliteUnit](collA){

    def task = {
      collA(index) = x
    }
  }

  protected[optiml] case class OP_trans(val collA: BooleanVector)
    extends DeliteOP_SingleTask[Vector[Boolean]](collA) {

    def task = {
      val out = new BooleanVectorImpl(!collA.is_row, collA.length)
      var i = 0
      while (i < out.length){
        out(i) = collA(i)
        i += 1
      }
      out
    }
  }
}

trait BooleanVector extends Vector[Boolean] {
  import BooleanVector._

  protected[optiml] var _data: Array[Boolean]

  override def trans(implicit c: ClassManifest[Boolean]) : Vector[Boolean] = {
    run(OP_trans(this))
  }
  
  def apply(n: Int) : Boolean
  override def lifted_apply(n: Int) : Boolean  = {
    run(OP_apply(this,n))
  }


  def update(index: Int, x: Boolean) : Unit
  override def lifted_update(index: Int, x: Boolean) : Unit = {
    run(OP_update(this,index,x))
  }

}
