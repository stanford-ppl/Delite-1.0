package ppl.delite.dsl.primitive

/* DeliteBoolean is a DeliteDSLType primitive used for returning scalar results as a proxy.
 * DeliteBooleans can be used interchangeably with regular Scala Booleans using implicit conversions.
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Jul 29, 2009
 * modified: Jul 29, 2009
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

import ppl.delite.core.ops.DeliteOP_SingleTask
import ppl.delite.core.appinclude._
import ppl.delite.core.{DeliteProxyFactory, DeliteDSLType}

object DeliteBoolean {
  ////////////////
  // constructors

  def apply(value: Boolean): DeliteBoolean = new Impl(value)

  // The extraction method, is used for pattern matching.  Note that we don't
  // attempt to extract the value from a proxy.
  def unapply(d: DeliteBoolean): Option[Boolean] = d match {
    case x: Impl => Some(x.value)
    case _ => None
  }

  // This is the concrete type that actually holds the DSL data type's data
  class Impl extends DeliteBoolean {

    type DSLType = Impl

    def this(num: Boolean) = {
      this()
      _value = num
      cvalue = this
      isComputed = true
    }

    var _value : Boolean = false

    def value = force._value

    override def concretize {
      _value = cvalue._value
      cvalue = this
    }

    override def clone = new Impl(value)
  }

  val proxyFactory = new ProxyFactory{}

  trait ProxyFactory extends DeliteProxyFactory[DeliteBoolean] {
    def newProxy() = new Impl()
  }

  /*
   *  The following are the OPs that this DSL defines, the purpose of OPs is delayed execution and optimization
   */

  protected case class OP_!=(p1: DeliteBoolean, p2: DeliteBoolean) extends DeliteOP_SingleTask[DeliteBoolean](p1, p2) {
    def task = {
      DeliteBoolean(p1.value != p2.value)
    }
  }

  protected case class OP_&(p1: DeliteBoolean, p2: DeliteBoolean) extends DeliteOP_SingleTask[DeliteBoolean](p1, p2) {
    def task = {
      DeliteBoolean(p1.value & p2.value)
    }
  }

  protected case class OP_&&(p1: DeliteBoolean, p2: DeliteBoolean) extends DeliteOP_SingleTask[DeliteBoolean](p1, p2) {
    def task = {
      DeliteBoolean(p1.value && p2.value)
    }
  }

  protected case class OP_==(p1: DeliteBoolean, p2: DeliteBoolean) extends DeliteOP_SingleTask[DeliteBoolean](p1, p2) {
    def task = {
      DeliteBoolean(p1.value == p2.value)
    }
  }

  protected case class OP_^(p1: DeliteBoolean, p2: DeliteBoolean) extends DeliteOP_SingleTask[DeliteBoolean](p1, p2) {
    def task = {
      DeliteBoolean(p1.value ^ p2.value)
    }
  }

  protected case class OP_unary_!(p: DeliteBoolean) extends DeliteOP_SingleTask[DeliteBoolean](p) {
    def task = {
      DeliteBoolean(p.value.unary_!)
    }
  }

  protected case class OP_BitOr(p1: DeliteBoolean, p2: DeliteBoolean) extends DeliteOP_SingleTask[DeliteBoolean](p1, p2) {
    def task = {
      DeliteBoolean(p1.value | p2.value)
    }
  }

  protected case class OP_||(p1: DeliteBoolean, p2: DeliteBoolean) extends DeliteOP_SingleTask[DeliteBoolean](p1, p2) {
    def task = {
      DeliteBoolean(p1.value || p2.value)
    }
  }
}


trait DeliteBoolean extends DelitePrimitive[Boolean] with DeliteDSLType {
  import DeliteBoolean._
  
  def value : Boolean

  def !=(p: DeliteBoolean) = run(OP_!=(this, p))
  def !=(p: Boolean) = (this.value != p)
  def &(p: DeliteBoolean) = run(OP_&(this, p))
  def &(p: Boolean) = (this.value & p)
  def &&(p: DeliteBoolean) = run(OP_&&(this, p))
  def &&(p: Boolean) = (this.value && p)
  def ==(p: DeliteBoolean) = (this.value == p.value)
  def ==(p: Boolean) = (this.value == p)
  def equals(p: DeliteBoolean) = run(OP_==(this, p))
  def equals(p: Boolean) = (this.value == p)
  def ^(p: DeliteBoolean) = run(OP_^(this, p))
  def ^(p: Boolean) = (this.value ^ p)
  def unary_! = run(OP_unary_!(this))
  def |(p: DeliteBoolean) = run(OP_BitOr(this, p))
  def |(p: Boolean) = (this.value | p)
  def ||(p: DeliteBoolean) = run(OP_||(this, p))
  def ||(p: Boolean) = (this.value || p)
}
