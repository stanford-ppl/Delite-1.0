package ppl.delite.dsl.optiml

import ppl.delite.dsl.optiml.specialized.IntVectorImpl
import ppl.delite.core.{DeliteFunc, DeliteProxyFactory}

/* IndexVector is an IntVector whose elements represent indices (e.g., of another vector).
 * They provide a vector construction operator { } that takes a function mapping an index to a value, producing
 * a new vector.
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: Jun 28, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

class IndexVector extends IntVectorImpl {
  private var _start = 0
  private var _end = 0

  def start = _start
  def end = _end  

  def this(st: Int, en: Int) = {
    this()
    _start = st
    _end = en

    assert(_start == 0) // temporary
    if (_start > _end) throw new IndexOutOfBoundsException
    init(true, _end-_start)
    for (i <- _start until _end){
      _data(i) = i
    }
    isComputed = true
  }

  def this(xs: Int*) = {
    this()
    _start = 0
    _end = xs.length

    init(true, xs.length)
    for (i <- _start until _end){
      _data(i) = xs(i)
    }
    isComputed = true
  }

  /**
   * Vector construction
   */

  // TODO: with compiler plugin, swap names apply and applyimpl

  // stub is replaced by impl by compiler plugin
  def applyimpl[@specialized(Double)A](block : => A)(implicit pFact : DeliteProxyFactory[Vector[A]], c: ClassManifest[A]) : Vector[A] = {
    null
  }

  // TODO: compiler plugin to:
  // TODO: 1) transform block (: => A) into a fixedBlock (Int => A)
  // TODO: 2) replace call to apply(block) above with call to applyimpl(fixedBlock) here
  def apply[@specialized(Double)A](block : Int => A)(implicit pFact : DeliteProxyFactory[Vector[A]], c: ClassManifest[A]) : Vector[A] = {
    val deps = block match {
      case f: DeliteFunc => f.deps
      case _ => Seq()
    }

    map(DeliteFunc(j => block(j), deps: _*))
  }

}