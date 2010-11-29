package ppl.delite.core

/**
 * This is a key class of the Delite class hierarchy, as it serves both as the root of all Proxies
 * as well as a node in our task execution graph. Each proxy has a set of inputs and outputs.
 *
 * Inputs are other objects on which the associated OP of this proxy depends on
 * Outputs are other objects that depend on the result of the OP associated with this proxy
 *
 */

/*

trait DeliteProxy[T] {

  def force: T = {
    if (isComputed) cvalue
    else reallyForce
  }

  /* CPU only force function
  def reallyForce: T = {
    Delite.force
    this.synchronized {
      forcing = true
      while (!isComputed)
        this.wait
    }
    cvalue
  }
  */

  //TODO: make this method abstract and implement in all subclasses
  def concretize { }

  def reallyForce: T = {

    Delite.force

    // Wait until this proxy is scheduled
	  // (Don't know whether this proxy will be scheduled on CPU or GPU
	  // TODO: fix this!
    while(!scheduled) {}

 	  // CPU forces CPU proxy, or GPU forces CPU proxy
    if(!isScheduledOnGPU) {
      this.synchronized {
        while (!isComputed) {
          forcing = true
          this.wait
        }
      }
      cvalue
    }
    else if(Delite.isGPUManager.get == false) { //CPU forces GPU proxy
      this.synchronized {
        while (!isComputed) {
          forcing = true
          DeliteGPUThreadPool.threads(0).fastQueue.put(this)
          println("Inserting to fast Queue!")
          this.wait
        }
      }
      cvalue
    }
    else { //GPU forces GPU proxy (Assume sequential scheduling)
      //if(cvalue == null) println("ERROR! DeliteProxy: cvalue should not be null here!")
      if(cvalue != null)
		  concretize
	  // else, already concretized
	  cvalue
    }
  }

  var inputs = new LinkedList[DeliteDSLType]
  // there is no point in having an output to a non-proxy type
  var outputs= new ArrayList[DeliteProxy[t] forSome {type t <: DeliteDSLType}]

  /**
   * This is the actual operation that when run will generate an instance of an implementation
   * of whatever DeliteDSLType the proxy is actually representing
   */
  var op: DeliteOP[T] = null

  /**
   * The resulting cvalue from executing the op associated with this proxy
   */
  var cvalue:T = _

  /**
   * Indicate tha the result in cvalue is the correct value. Now you can use it.
   */
  @volatile
  var isComputed = false

  /**
	* Indicate that this proxy is scheduled to run on GPU 
	*/
  var isScheduledOnGPU = false
  
  /**
   *  This section is for various flags used by the Delite runtime when executing
   */
  @volatile
  var scheduled = false
  
  var forcing = false


  //anti-dependency list
  var antiDeps = new LinkedList[DeliteProxy[_]]

  /**
   * Pre(post) tasks that may need to run before(after) the proxy op
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
    while(schedulable && idx != inputs.size()) {
      if(inputs.get(idx).isInstanceOf[DeliteProxy[_]]) {
        if(inputs.get(idx).asInstanceOf[DeliteProxy[_]].scheduled == false)
          schedulable = false
      }
      idx +=1
    }
    schedulable
  }

  override def toString = {
    super.toString + " => " + (if(op == null) "" else op.toString)
  }

}
*/
