package ppl.delite.dsl.optiml.data

import ppl.delite.dsl.optiml.Matrix
import reflect.ClassManifest
import ppl.delite.dsl.optiml.specialized.{DoubleMatrixImpl, FloatMatrix, DoubleMatrix}

/* GrayScaleImage is an image where each pixel is represented by a single double value.
 * GrayScaleImage is a DoubleMatrix with some additional semantic information.
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: Jun 21, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

object GrayScaleImage {

  def apply[A](m: Matrix[A])(implicit man: ClassManifest[A]) : Image = {
     man match {
      case ClassManifest.Double => (new GrayScaleImage(m.asInstanceOf[DoubleMatrix])).asInstanceOf[Image]
      case ClassManifest.Float => (new GrayScaleImage(m.asInstanceOf[FloatMatrix].toDouble.asInstanceOf[DoubleMatrix])).asInstanceOf[Image]
      case _ => throw new UnsupportedOperationException()
    }
  }

}

class GrayScaleImage(m: DoubleMatrix) extends DoubleMatrixImpl(m._data, m.numRows, m.numCols) with Image {
  def isColor = false
  def isCompressed = false
}