package ppl.delite.core

import executor.DeliteGPUThreadPool
import ppl.delite.core.ops.DeliteOP
import java.util.{ArrayList, LinkedList}

/**
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * This is a key class of the Delite class hierarchy, as it serves both as the root of all DSL objects
 * as well as a node in our task execution graph. Each DeliteDSLType has a set of inputs and outputs.
 *
 * Inputs/Producers are other objects on which the associated OP of this object depends on to execute
 * Outputs/Consumers are other objects that depend on this object
 *
 */

trait DeliteDSLType {

  //TODO: we have the system constructed so this is always the DSLType, but can we do this such that the type system understands???
  type DSLType <: DeliteDSLType

  final def force: DSLType = {
    if (!isReady) 
      reallyForce
    cvalue
  }

  private def isReady = isComputed && (lastMutatingOP == null || !Delite.isMainThread)

  /* CPU only force function
  def reallyForce: DSLType = {
    Delite.force
    this.synchronized {
      forcing = true
      while (!isComputed)
        this.wait
    }
    this
  }
  */

  private def reallyForce {

    Delite.force

    // Wait until this object is scheduled
	  // (Don't know whether this object will be scheduled on CPU or GPU
	  // TODO: fix this!
    if (!isComputed) {
      var i = 0
      while (!scheduled) {
        i += 1
        if ((i & 0x1ffff) == 0)
          Thread.`yield`
      }
    }

 	  // CPU forces CPU object, or GPU forces CPU object
    if (!isScheduledOnGPU) {
      this.synchronized {
        while (!isReady) {
          forcing = true
          this.wait
        }
      }
    }
    else if (Delite.isGPUManager.get == false) { //CPU forces GPU object
      this.synchronized {
        while (!isReady) {
          forcing = true
          DeliteGPUThreadPool.threads(GPUdevIdx).fastQueue.put(this)
          //println("Inserting to fast Queue!")
          this.wait
        }
      }
    }
//    else { //GPU forces GPU object (Assume sequential scheduling)
//      //if (cvalue != null) concretize
//      //if(cvalue == null) println("ERRORRRRR"+this.op.toString)
//      cvalue
//    }
  }

  def concretize { }

  var cvalue: DSLType = _

  /**
   * Producers of the Object
   * List of Input dependencies and anti-dependencies that must be satisfied before this object can become concrete
   */
  var inputs = new LinkedList[DeliteDSLType]
  var antiDeps = new LinkedList[DeliteDSLType]

  /**
   * Consumers of this Object
   * Trackers for OPs that consume this object so mutations can be handled in sequential order
   */
  val outstandingOPs = new LinkedList[DeliteDSLType]
  var lastMutatingOP: DeliteDSLType = null
  //TODO: do we still need this construct:
  var outputs = new ArrayList[DeliteDSLType]

  /**
   * This is the OP associated with the creation of this object
   * Executing it will make this object concrete
   */
  var op: DeliteOP[DSLType] = null

  /**
   * Indicate that the data in this object is valid
   */
  @volatile
  var isComputed = false

  /**
	* Indicate that this object is scheduled to run on GPU
	*/
  var isScheduledOnGPU = false

  /**
   * Indicate which GPU device is handling this object
   * Only meaningful when (isScheduledOnGPU == true)
   */
  var GPUdevIdx = -1
  
  /**
   *   This section is for various flags used by the Delite runtime when executing
   */
  @volatile
  var scheduled = false

  var forcing = false

  /**
   * Pre(post) tasks that may need to run before(after) the object's op
   * These tasks do not produce any result, simply synchronize
   */
  def header {
    val it = inputs.iterator
    while (it.hasNext) {
      it.next.force //force on all dependencies
    }

    if (antiDeps.isEmpty) return
    val iter = antiDeps.iterator
    while (iter.hasNext) {
      iter.next.force //force on all anti-dependencies
    }
  }

  //def footer { }

  def isSchedulable = {
    var schedulable = true
    var idx = 0
    val iter = inputs.iterator
    while (schedulable && iter.hasNext) {
      if (!iter.next.scheduled) schedulable = false
    }
    schedulable
  }

  /**
   * The main submission function for OPs to be submitted to Delite
   */
  def run[T <: DeliteDSLType](op: DeliteOP[T])(implicit proxyFactory: DeliteProxyFactory[T]): T = {
    Delite.run(op)
  }

  /* TODO: stubs, implement or remove call-site */
  var relaxPct = 0.0
  def nodeAttr = ""

  override def toString = {
    super.toString + " => " + (if(op == null) "" else op.toString)
  }

}


class DeliteUnit extends DeliteDSLType {
  type DSLType = DeliteUnit
}
