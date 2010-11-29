package ppl.delite.dsl.optiml.train

import reflect.ClassManifest
import ppl.delite.dsl.optiml.{MatrixImpl, Matrix}
import ppl.delite.dsl.optiml.specialized._

/* A minimal abstract interface for a training set. All training sets loaded by
 * OptiML should extend this trait.
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: Jun 4, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */


object TrainingSet {
  def apply[A](m: Matrix[A])(implicit man: ClassManifest[A]) : TrainingSet[A] = {
     man match {
      case ClassManifest.Double => (new DoubleTrainingSetImpl(m.asInstanceOf[DoubleMatrix])).asInstanceOf[TrainingSet[A]]
      case ClassManifest.Int => (new IntTrainingSetImpl(m.asInstanceOf[IntMatrix])).asInstanceOf[TrainingSet[A]]
      case ClassManifest.Boolean => (new BooleanTrainingSetImpl(m.asInstanceOf[BooleanMatrix])).asInstanceOf[TrainingSet[A]]
      case _ => throw new UnsupportedOperationException()
    }
  }

}

// TODO: make training set best effort by default
trait TrainingSet[T] extends Matrix[T] {
  type LabelType

  val numSamples : Int = _numRows
  val numFeatures : Int = _numCols
  def labels : Labels[LabelType] = _labels.get

  var _labels : Option[Labels[LabelType]] = None
}

class DoubleTrainingSetImpl(m: DoubleMatrix) extends DoubleMatrixImpl(m._data, m.numRows, m.numCols) with TrainingSet[Double]

class IntTrainingSetImpl(m: IntMatrix) extends IntMatrixImpl(m._data, m.numRows, m.numCols) with TrainingSet[Int]

class BooleanTrainingSetImpl(m: BooleanMatrix) extends BooleanMatrixImpl(m._data, m.numRows, m.numCols) with TrainingSet[Boolean]

