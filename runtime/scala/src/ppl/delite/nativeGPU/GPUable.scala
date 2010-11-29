package ppl.delite.nativeGPU

/**
 * Author: Kevin J. Brown
 * Date: Feb 8, 2010
 * Time: 7:20:49 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait GPUable[T <: Any] {

  // DirectArray should be the backing store of any GPUable collection
  protected[delite] def gpu_data: Array[T]
  protected[delite] def gpu_setdata(elms: Array[T])
  protected[delite] def gpu_datasize: Int
  protected[delite] def gpu_apply(i: Int)
  protected[delite] def gpu_update(i: Int, x: T)
}
