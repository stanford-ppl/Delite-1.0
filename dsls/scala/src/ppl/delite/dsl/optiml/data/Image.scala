package ppl.delite.dsl.optiml.data

import ppl.delite.dsl.optiml.Matrix
import reflect.ClassManifest
import ppl.delite.dsl.optiml.specialized.{FloatMatrix, DoubleMatrix, DoubleMatrixImpl}

/* Image defines a basic interface for any image data type.
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: Jun 21, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */


// should all images support matrix like operations? do all matrix like operations make sense?
trait Image extends ODataSource {
  def isColor : Boolean
  def isCompressed : Boolean

  /* TODO: Utility methods for image conversions */
}



//class RGBImage extends Image
//class JPEGImage extends Image