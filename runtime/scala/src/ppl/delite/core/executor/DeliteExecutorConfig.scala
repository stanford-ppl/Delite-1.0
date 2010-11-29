package ppl.delite.core.executor

import ppl.delite.core.DeliteDSLType

/**
 * Author: Kevin J. Brown
 * Date: Apr 22, 2010
 * Time: 4:56:05 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * This trait provides a common interface between all the different executor designs for the sake of initialization and submission of proxies by object Delite
 */

trait DeliteExecutorConfig {

  /**
   * The top-most submit function of the executor block(s): this is the method that will be called by object Delite
   */
  def submit(proxy: DeliteDSLType): Unit

  /**
   * A request by the main thread to force execution of all outstanding OPs
   * This method must be implemented correctly for schedulers that runs in the main thread
   * Can do nothing for schedulers that run asynchronously
   */
  def force: Unit

  /**
   *  Method called by DeliteApplication at beginning of execution
   *  Responsible for starting up any other threads that the Config requires
   */
  def start: Unit

  /**
   * Method called by DeliteApplication at end of execution
   * Responsible for shutting down all threads the Config created
   */
  def shutdown: Unit
  
}