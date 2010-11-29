package ppl.delite.dsl.optiml.train

import reflect.ClassManifest
import ppl.delite.dsl.optiml.{VectorImpl, Vector}
import ppl.delite.dsl.optiml.specialized._

/* A minimal abstract interface for a set of labels. 
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: Jun 4, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */


object Labels {
  def apply[A](m: Vector[A])(implicit man: ClassManifest[A]) : Labels[A] = {
     man match {
      case ClassManifest.Double => (new DoubleLabelsImpl(m.asInstanceOf[DoubleVector])).asInstanceOf[Labels[A]]
      case ClassManifest.Int => (new IntLabelsImpl(m.asInstanceOf[IntVector])).asInstanceOf[Labels[A]]
      case ClassManifest.Boolean => (new BooleanLabelsImpl(m.asInstanceOf[BooleanVector])).asInstanceOf[Labels[A]]
      case _ => throw new UnsupportedOperationException()
    }
  }

}

trait Labels[T] extends Vector[T] {
  val numLabels : Int = _length
}

class DoubleLabelsImpl(v: DoubleVector) extends DoubleVectorImpl(v._data, v.is_row, v.length) with Labels[Double]

class IntLabelsImpl(v: IntVector) extends IntVectorImpl(v._data, v.is_row, v.length) with Labels[Int]

class BooleanLabelsImpl(v: BooleanVector) extends BooleanVectorImpl(v._data, v.is_row, v.length) with Labels[Boolean]

