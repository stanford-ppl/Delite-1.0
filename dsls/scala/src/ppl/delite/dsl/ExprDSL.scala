package ppl.delite.dsl

import ppl.delite.core.ops.{DeliteOP, DeliteOP_SingleTask}
//import ppl.delite.core.optimizer.DelitePatternMatchingOptimizer
import ppl.delite.core.include._
import ppl.delite.core.appinclude._
import ppl.delite.core.{DeliteProxyFactory, DeliteDSLType}

/**
 * Each DSL type provides an object that performs a couple of functions as well as holds
 * various other types used by the DSL, check inline comments
 **/
object Expr {
  implicit val proxyFactory = new ProxyFactory{}
  
  // The injection method, it hides the details of constructing a concrete type and returning it as
  // the interface type
  def apply(value: Long): Expr = new Impl(value)

  // The extraction method, is used for pattern matching.  Note that we don't
  // attempt to extract the value from a proxy.
  def unapply(e: Expr): Option[Double] = e match {
    case x: Impl => Some(x.value)
    case _ => None
  }

  // This is the concrete type that actually holds the DSL data type's data
  class Impl(override val value: Long) extends Expr {
    //override def force = this

    //TODO need to pull this out of what the DSL writer has to provide
    override def nodeAttr = "[shape=ellipse]"
    override def toString = "Expr:Impl{" + value + "}"
  }

  // This is the proxy object handed back whenever computation has been deferred
  class Proxy extends Expr {
    override def nodeAttr = "[shape=box]"
    override def toString = "Expr" + super.toString
  }

  class ProxyFactory extends DeliteProxyFactory[Expr] {
    def newProxy() = new Proxy
  }

  /*
  *  The following are the OPs that this DSL defines, the purpose of OPs is delayed execution and optimization
  * */
  case class OP_+(var e1: Expr, var e2: Expr) extends DeliteOP_SingleTask[Expr](e1, e2) {
    //if (developmentMode) DTRACER trace "[Expr:OP_+] " + this.toString + "[" + hex8(e1) + "," + hex8(e2) + "]"
    def task = new Impl(e1.value + e2.value)
    /*
    override def update(args: Seq[AnyRef]) {
      e1 = args(0).asInstanceOf[Expr]
      e2 = args(1).asInstanceOf[Expr]
      //updateDeps(args)
    }
    */
  }

  case class OP_-(e1: Expr, e2: Expr) extends DeliteOP_SingleTask[Expr](e1, e2) {
    def task = new Impl(e1.value - e2.value)
  }

  case class OP_*(var e1: Expr, var e2: Expr) extends DeliteOP_SingleTask[Expr](e1, e2) {
    def task = new Impl(e1.value * e2.value)
    /*
    override def update(args: Seq[AnyRef]) {
      e1 = args(0).asInstanceOf[Expr]
      e2 = args(1).asInstanceOf[Expr]
      //updateDeps(args)
    }
    */
  }

  case class OP_++(e1:Expr, e2:Expr,e3:Expr) extends DeliteOP_SingleTask[Expr](e1, e2,e3) {
    def task = new Impl(e1.value + e2.value + e3.value)
  }

  /*
  * The following are optimization artifacts that DELITE can use to apply Domain
  * specific optimization to the resulting DAG from
  * */

  /*
  object opt extends DelitePatternMatchingOptimizer[Expr] {

    def optimize(op: DeliteOP[Expr]) = {
      op match {

        case OP_+(Expr(0), e) => {
          println( "[Expr.optimize] Adding zero to an expression." )
          DeliteOpWrap[Expr](e)
        }


        case OP_+(e1, P(OP_+(e2,e3))) => {
          OP_++(e1,e2,e3)
        }


        case OP_*(_, e@Expr(0)) => {
          DTRACER debug "[Expr.optimize] Multiplying by zero."
          DeliteOpWrap[Expr](e)
        }


        case OP_+(P(OP_*(a1, b)), P(OP_*(a2, c))) if a1 == a2 => {
          DTRACER debug "[Expr.optimize] Expression can be simplified!"
          (a1 * (b + c)).asInstanceOf[DeliteProxy[Expr]].op
        }

        case _ => {
          println( "[Expr.optimize] Nothing to Optimize.")
          op
        }
      }
    }
  }
  */
}


/**
 *  The trait defines the methods that are provided by this DSL type.
 *  It uses OPs to defer their execution
 **/
trait Expr extends DeliteDSLType {
  // Convenient imports and partially applied function to save us typing
  import Expr._

  type DSLType = Expr

  //override val optimizer = Some(opt)
  def byteSize: Long = 8

  //if (developmentMode) DTRACER trace "[Expr trait] " + this

  def +(rhs: Expr): Expr = {
    //println("first plus")
    run(OP_+(this, rhs))
  }

  def ++(rhs: Expr): Expr = {
    //println("second plus")
    run(OP_+(this, rhs))
  }

  def -(rhs: Expr): Expr = run(OP_-(this, rhs))

  def *(rhs: Expr): Expr = {
    //println("multiply")
    run(OP_*(this, rhs))
  }

  def value: Long = force.value

}
