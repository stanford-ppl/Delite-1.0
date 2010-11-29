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
import ppl.delite.dsl.optiml.{ArithOps, Matrix}
import ppl.delite.core.appinclude._
import ppl.delite.core.DeliteUnit
import ppl.delite.dsl.primitive.DeliteBoolean

object BooleanMatrix{
  protected[optiml] case class OP_apply(val collA: BooleanMatrix, i: Int, j: Int)
    extends DeliteOP_SingleTask[DeliteBoolean](collA){

    def task = {
      collA(i,j)
    }
  }

  protected[optiml] case class OP_update(val collA: BooleanMatrix, i: Int, j: Int, x: Boolean)
    extends DeliteOP_SingleTask[DeliteUnit](collA){

    def task = {
      collA(i,j) = x
    }
  }
}

trait BooleanMatrix extends Matrix[Boolean] {
  import BooleanMatrix._

  protected[optiml] var _data: Array[Boolean]
  
  override def apply(i: Int, j: Int) : Boolean
  override def lifted_apply(i: Int, j: Int) : Boolean  = {
    run(OP_apply(this,i,j))
  }


  override def update(row: Int, col: Int, x: Boolean)
  override def lifted_update(row: Int, col:Int, x: Boolean) : Unit = {
    run(OP_update(this,row,col,x))
  }
}
