/* Description
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Feb 3, 2010
 * modified: Feb 3, 2010
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
package ppl.delite.dsl.optiml.classifier

import ppl.delite.dsl.optiml.io.OInputSource

trait Classifier {
  def train(data: OInputSource)
  def classify(sample: OInputSource)
}