package ppl.delite.core.executor

import ppl.delite.core.DeliteDSLType
import java.util.concurrent.ArrayBlockingQueue
import ppl.delite.core.{Delite, Config}
import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.specialized._
import ppl.delite.cuda._
import ppl.delite.core.ops.DeliteOP_MutableSingleTask
import ppl.delite.dsl.primitive.DeliteDouble
import ppl.delite.nativeGPU.GPUable
import ppl.delite.dsl.primitive._

//TODO: change call to elms.getAddr to appropriate Java array version

class DeliteGPUExecutionThread(val id:Int) extends Thread {

  class GPUTaskNode(val proxy:DeliteDSLType, val stream:Int, val timeStamp:Int) {}

  @volatile
  private var _shutdown = false;

  @volatile
  var cleanup = false;

  @volatile
  var cleanup_done = false;
  
  // Call myself something descriptive
  setName("GPU-Exec-Thread[" + id + "]")

  val argsQueue = new ArrayBlockingQueue[AnyRef](1000)

  // Create a task queue for each GPU manager
  //TODO:Set the queue size Config properly
  val taskQueue = new ArrayBlockingQueue[DeliteDSLType](Config.executorInQueueSize*1000)
  
  // The queue for handling outside force request
  //val fastQueue = new ArrayBlockingQueue[DeliteDSLType](Config.GPUThreadInQueueSize)
  val fastQueue = new ArrayBlockingQueue[DeliteDSLType](Config.executorInQueueSize*1000)
  
  // Number of streams = 30
  val NumStreams = 30
  var currentStream = 0

  // Variables for termination check
  val terminationCheckChunk = 200
  var terminationCheckCnt = terminationCheckChunk
  
  // HashMap for GPU Cache (Java objct -> device mem addr)
  // What would be the better structure instead of keeping 2 cache block
  // cacheBack can just be an array indexed by (device address - offset)/scalar
  var cache = new java.util.IdentityHashMap[AnyRef, Tuple2[Long,Int]]
  var cacheBack = new java.util.HashMap[Long, AnyRef]
  
  // List of outstanding kernels
  //var kernelCache = new java.util.IdentityHashMap[AnyRef, Tuple2[Int, Int]]
  var issuedQueue = new Array[ArrayBlockingQueue[GPUTaskNode]](NumStreams)
  val timeStamp = new Array[Int](NumStreams)
  val timeStampPtr = new Array[Long](NumStreams)

  // Device Memroy Size (1GB) and cache block size (16MB)
  // TODO: These values should come from Config file
  //val devMemSize = 1024 * 1024 * 1024
  val devMemSize = 1536 * 1024 * 1024
  val devMemSize1 = Config.GPUMemSize1 * 1024
  val devMemSize2 = Config.GPUMemSize2 * 1024
  //val devMemSize1 = 256 * 1024 * 1024
  //val devMemSize2 = 1280 * 1024 * 1024
  //val devMemSize1 = 512 * 1024 * 1024
  //val devMemSize2 = 1024 * 1024 * 1024
  val cacheBlockSize = Array(1024*Config.GPUMemBlockSize1, 1024*Config.GPUMemBlockSize2)
  var numCacheBlocks = new Array[Int](2)
  numCacheBlocks(0) = devMemSize1/cacheBlockSize(0)
  numCacheBlocks(1) = devMemSize2/cacheBlockSize(1)
  val FreeCacheThreashold = 1
  val AllocCacheEvict = 1

  // To manage device memory manually, keep track of allocated blocks and free blocks list
  // TODO: Investigate which data structure would be the best(ArrayBlockingQueue/LinkedList/etc). Same for taskQueue/fastQueue/cache
  val cacheFreeList = new Array[ArrayBlockingQueue[Long]](2)
  val cacheAllocList = new Array[ArrayBlockingQueue[Long]](2)
  for(i <- 0 until 2) {
    cacheFreeList(i) = new ArrayBlockingQueue[Long](numCacheBlocks(i))
    cacheAllocList(i) = new ArrayBlockingQueue[Long](numCacheBlocks(i))
  }

  //val cacheFreeList = new ArrayBlockingQueue[Long](numCacheBlocks)
  //val cacheAllocList = new ArrayBlockingQueue[Long](numCacheBlocks)

  var driver: DeliteCudaDriver = null
  var gKernelHandler: Array[Long] = null
  
  // Returns a cache block index that is currently free to use
  def getCacheBlock(size: Int):Long = {
    //println("calling getcache " + size)

    val sizeIdx = if(size <= cacheBlockSize(0)/8) 0 else 1

    if(cacheFreeList(sizeIdx).size < FreeCacheThreashold)
		  runEviction(sizeIdx)

	  if(cacheFreeList(sizeIdx).isEmpty == false) {
	    val ptr = cacheFreeList(sizeIdx).poll()
	    cacheAllocList(sizeIdx).put(ptr)
	    //if(cacheFreeList(sizeIdx).size < FreeCacheThreashold)
		  //  runEviction(sizeIdx)
	    ptr
	  }
	  else {
	    println("ERROR: No more GPU Device Memory can be allocated")
      println("size is " + size)
	    0
	  }
  }

	// Deallocate GPU Device Mem 
  def runEviction(sizeIdx:Int) = {
		//var cacheBlock:java.lang.Long = 0
		var idx = AllocCacheEvict 
		while(idx > 0) {
			if(cacheAllocList(sizeIdx).size == 0) println("EVIC ERR : 1")
			val cacheBlock = cacheAllocList(sizeIdx).poll()
			cacheFreeList(sizeIdx).put(cacheBlock)
			//if(!cache.containsValue(cacheBlock)) println("EVIC ERR : 2")
			if(!cache.containsKey(cacheBack.get(cacheBlock))) println("EVIC ERR : 2")
			cache.remove(cacheBack.get(cacheBlock)) 
			cacheBack.remove(cacheBlock) 
			idx = idx - 1
		}
	}

  def getSizeIdx(size:Int):Int = {
    val sizeIdx = if(size <= cacheBlockSize(0)/8) 0 else 1
    sizeIdx
  }

  val PLMemCnt = new Array[Int](2)
  val PageLockedMem = new Array[Array[Long]](2)
  var PageLockedMem_Out:Long = 0
  var timeStampDevPtr = new Array[Long](NumStreams)
  var timeStampHostPtr = new Array[Long](NumStreams)
	val stream = new Array[Long](30)


  var dev_ptr: Long = 0

  def initDataStructures {

    if(devMemSize1+devMemSize2 > devMemSize)
		println("ERROR! Not Enough GPU Memory!")

  	// Allocate 1GB of GPU Device memory at initialization stage
    dev_ptr = driver.cudaMemAlloc(devMemSize)
    
    // Allocate PageLockedMem for copy between device mem and host mem
    // TODO: Not needed anymore with nativearray
    // TODO: Need to consider the case where a PL Meme is used before previous usage is finished
    //       Should implement this using the queue and block the allocation when no more left.
    //       Deallocate as a kernel (transfer) is done.
    //val PLMemCnt = new Array[Int](2)
    //val PageLockedMem = new Array[Array[Long]](2)
    for(i <- 0 until 2) {
      PLMemCnt(i) = 0
      PageLockedMem(i) = new Array[Long](numCacheBlocks(i))
      PageLockedMem(i)(0) = driver.cudaMemAllocHost(cacheBlockSize(i)*numCacheBlocks(i))
      for(j <- 1 until numCacheBlocks(i))
        PageLockedMem(i)(j) = PageLockedMem(i)(0) + j*cacheBlockSize(i)
      //for(j <- 0 until numCacheBlocks(i))
      //  PageLockedMem(i)(j) = driver.cudaMemAllocHost(cacheBlockSize(i))
    }
    // PageLocked Memory for fast queue (Only need a memory chunk for each stream)
    PageLockedMem_Out = driver.cudaMemAllocHost(cacheBlockSize(1))

    // Create Streams
    for(i <- 0 until 30)
			stream(i) = driver.cudaCreateStream

    // Allocate Pinned Memory for kernel termination checking
    //var timeStampDevPtr = new Array[Long](NumStreams)
    //var timeStampHostPtr = new Array[Long](NumStreams)
    timeStampHostPtr(0) = driver.cudaMemAllocHost(4*NumStreams)
    timeStampDevPtr(0) = driver.cudaMemHostGetDevicePointer(timeStampHostPtr(0))
    for(i <- 0 until NumStreams) {
      timeStampDevPtr(i) = timeStampDevPtr(0) + 4*i
      timeStampHostPtr(i) = timeStampHostPtr(0) + 4*i
    }

    for(i <- 0 until NumStreams)
      issuedQueue(i) = new ArrayBlockingQueue[GPUTaskNode](1000)

    //Initialize cache Free List
    for(i <- 0 until numCacheBlocks(0)) {
      cacheFreeList(0).put(dev_ptr + i*cacheBlockSize(0))
    }
    for(i <- 0 until numCacheBlocks(1)) {
      cacheFreeList(1).put(dev_ptr + devMemSize1 + i*cacheBlockSize(1))
    }

    // Allocate device memory for timestamps (kernel synchronization)
    timeStampPtr(0) = driver.cudaMemAlloc(8*NumStreams)

    // Initialize stream timestamps to 0 and devptr for timestamp
    for(i <- 0 until NumStreams) {
      timeStamp(i) = 0
      timeStampPtr(i) = timeStampPtr(0) + 8*i
      driver.cudaSched(gKernelHandler(1), timeStampPtr(i), timeStamp(0), stream(0))
      driver.cudaSched(gKernelHandler(1), timeStampDevPtr(i), timeStamp(0), stream(0))
    }
    driver.cudaStreamSync(stream(0))
  }

  def cleanup_start {
    cache = new java.util.IdentityHashMap[AnyRef, Tuple2[Long,Int]]
    cacheBack = new java.util.HashMap[Long, AnyRef]

    //kernelCache = new java.util.IdentityHashMap[AnyRef, Tuple2[Int, Int]]

    for(i <- 0 until NumStreams)
      issuedQueue(i) = new ArrayBlockingQueue[GPUTaskNode](1000)

    for(i <- 0 until 2) {
      cacheFreeList(i) = new ArrayBlockingQueue[Long](numCacheBlocks(i))
      cacheAllocList(i) = new ArrayBlockingQueue[Long](numCacheBlocks(i))
    }
     //Initialize cache Free List
    for(i <- 0 until numCacheBlocks(0)) {
      cacheFreeList(0).put(dev_ptr + i*cacheBlockSize(0))
    }
    for(i <- 0 until numCacheBlocks(1)) {
      cacheFreeList(1).put(dev_ptr + devMemSize1 + i*cacheBlockSize(1))
    }

    for(i <- 0 until 2) {
      PLMemCnt(i) = 0
    }
  }

  // Main code for GPU management thread
  override def run = {

    Delite.isGPUManager.set(true)
	
	  //TODO: Really need this?
	  //Delite.isExecutor.set(true)

    println("Starting GPU Executor Thread: " + id)
    println("The block size is " + cacheBlockSize(0) + ", " + cacheBlockSize(1))

    // Initialize GPU Device
    val (driver1, gKernelHandler1) = DeliteCuda.initialize(id)
    driver = driver1
    gKernelHandler = gKernelHandler1
    initDataStructures
    
    println("GPU Executor Thread: " + id + ", init done!")

    // Let DeliteGPUThreadPool know that initilazation is done!
    DeliteGPUThreadPool.initDone

    var cacheMissCount = 0

    // Main Loop : Keep iterating until no more work to do AND shutdown request from threadpool thread
    while ((!_shutdown) || (taskQueue.isEmpty == false)) {

      // Check for outside force request
      // TODO: Too much overhead to check for every iteration?
      handleFastQueue

      val proxy = taskQueue.poll() // Non blocking: returns null is empty

      if(proxy != null) {

        // Force all the dependencies
        DeliteGPUThreadPool.gpuCleanupThread(id).gpuOpHeader(proxy)
        while(!DeliteGPUThreadPool.gpuCleanupThread(id).forcing_done) {
          // Do Kernel Termination Check
          handleFastQueue
        }

        var inSize = 0
        var outSize = 0
        var DevPtrs:List[Long] = Nil

        val inputs = proxy.op.getGPUInputs

        Profiler.start("PREE")

        if(inputs != null) {

          // Decide current stream
          //currentStream = (currentStream + 1) % NumStreams
          val iter = inputs.elements
          while(iter.hasNext) {
            val in = iter.next
            if(in.isInstanceOf[Matrix[_]]) {
              val data = in.asInstanceOf[GPUable[_]].gpu_data
              val input = in.asInstanceOf[Matrix[_]]
              inSize = input.size
              if(cache.containsKey(data)) {  // Cache Hit
                val cacheData = cache.get(data)
                val devptr = cacheData._1
                val sizeIdx = cacheData._2
                DevPtrs = DevPtrs ::: List(devptr) 
                if(!cacheAllocList(sizeIdx).remove(devptr)) println("ERROR : Not allocated")
                  cacheAllocList(sizeIdx).put(devptr)
                //Post process for synchronization
                //if(!kernelCache.containsKey(data)) println("ERR: No Key in it")
                //val epoch = kernelCache.get(data)
                //driver.cudaSched(DeliteCuda.gKernelHandler(0), timeStampPtr(epoch._1), epoch._2, stream(currentStream))
              }
              else {
                cacheMissCount += 1
                val newDevPtr = getCacheBlock(data.length)
                val sizeIdx = getSizeIdx(data.length)
                if(in.isInstanceOf[FloatMatrix])
                  driver.cudaMemCpyHtoDAsyncFloat(newDevPtr, data.asInstanceOf[Array[Float]], 0, data.length, PageLockedMem(sizeIdx)(PLMemCnt(sizeIdx)), stream(currentStream))
                else if(in.isInstanceOf[DoubleMatrix])
                  driver.cudaMemCpyHtoDAsync(newDevPtr, data.asInstanceOf[Array[Double]], 0, data.length, PageLockedMem(sizeIdx)(PLMemCnt(sizeIdx)), stream(currentStream))
                else if(in.isInstanceOf[IntMatrix])
                  driver.cudaMemCpyHtoDAsyncInt(newDevPtr, data.asInstanceOf[Array[Int]], 0, data.length, PageLockedMem(sizeIdx)(PLMemCnt(sizeIdx)), stream(currentStream))
                else
                  println("Error! Not supported DeliteDSLType 1!")
                //driver.cudaStreamSync(stream(currentStream))
                
                PLMemCnt(sizeIdx) = (PLMemCnt(sizeIdx) + 1) % numCacheBlocks(sizeIdx)
                cache.put(data, Tuple2(newDevPtr,getSizeIdx(data.length)))
                cacheBack.put(newDevPtr, data)
                DevPtrs = DevPtrs ::: List(newDevPtr)
                //Post process for synchronization
                //timeStamp(currentStream) = timeStamp(currentStream) + 1
                //driver.cudaSched(DeliteCuda.gKernelHandler(1), timeStampPtr(currentStream), timeStamp(currentStream), stream(currentStream))
                //kernelCache.put(data, Tuple2(currentStream, timeStamp(currentStream)))
                //issuedQueue(currentStream).put(Tuple2(in, timeStamp(currentStream)))
              }
            }
            else if(in.isInstanceOf[Vector[_]]) {
              val data = in.asInstanceOf[GPUable[_]].gpu_data
              var input = in.asInstanceOf[Vector[_]]
              inSize = input.length
              if(cache.containsKey(data)) {  // Cache Hit
                val cacheData = cache.get(data)
                val devptr = cacheData._1
                val sizeIdx = cacheData._2
                if(in.isInstanceOf[VectorView[_]]) {
                  if(in.isInstanceOf[DoubleVector])
                    DevPtrs = DevPtrs ::: List(devptr + input.asInstanceOf[VectorView[_]].start*8)
                  else
                    DevPtrs = DevPtrs ::: List(devptr + input.asInstanceOf[VectorView[_]].start*4)
                }
                else
                  DevPtrs = DevPtrs ::: List(devptr) 
                if(!cacheAllocList(sizeIdx).remove(devptr)) println("ERROR : Not allocated")
                  cacheAllocList(sizeIdx).put(devptr)
                //Post process for synchronization
                //if(!kernelCache.containsKey(data)) println("ERR: No Key in it")
                //val epoch = kernelCache.get(data)
                //driver.cudaSched(DeliteCuda.gKernelHandler(0), timeStampPtr(epoch._1), epoch._2, stream(currentStream))
              }
              else {
                cacheMissCount += 1
                val newDevPtr = getCacheBlock(data.length)
                val sizeIdx = getSizeIdx(data.length)
                if(in.isInstanceOf[DoubleVectorViewImpl])
                  driver.cudaMemCpyHtoDAsync(newDevPtr, data.asInstanceOf[Array[Double]], 0, data.length, PageLockedMem(sizeIdx)(PLMemCnt(sizeIdx)), stream(currentStream))
                else if(in.isInstanceOf[DoubleVector])
                  driver.cudaMemCpyHtoDAsync(newDevPtr, data.asInstanceOf[Array[Double]], 0, data.length, PageLockedMem(sizeIdx)(PLMemCnt(sizeIdx)), stream(currentStream))
                else if(in.isInstanceOf[FloatVectorViewImpl])
                  driver.cudaMemCpyHtoDAsyncFloat(newDevPtr, data.asInstanceOf[Array[Float]], 0, data.length, PageLockedMem(sizeIdx)(PLMemCnt(sizeIdx)), stream(currentStream))
                else if(in.isInstanceOf[FloatVector])
                  driver.cudaMemCpyHtoDAsyncFloat(newDevPtr, data.asInstanceOf[Array[Float]], 0, data.length, PageLockedMem(sizeIdx)(PLMemCnt(sizeIdx)), stream(currentStream))
                else if(in.isInstanceOf[IntVectorViewImpl])
                  driver.cudaMemCpyHtoDAsyncInt(newDevPtr, data.asInstanceOf[Array[Int]], 0, data.length, PageLockedMem(sizeIdx)(PLMemCnt(sizeIdx)), stream(currentStream))
                else if(in.isInstanceOf[IntVector])
                  driver.cudaMemCpyHtoDAsyncInt(newDevPtr, data.asInstanceOf[Array[Int]], 0, data.length, PageLockedMem(sizeIdx)(PLMemCnt(sizeIdx)), stream(currentStream))
                else
                  println("Error!! Not supported DeliteDSLType 2!" + in.toString + proxy.op.toString)
                //driver.cudaStreamSync(stream(currentStream))
                PLMemCnt(sizeIdx) = (PLMemCnt(sizeIdx) + 1) % numCacheBlocks(sizeIdx)
                cache.put(data, Tuple2(newDevPtr,getSizeIdx(data.length)))
                cacheBack.put(newDevPtr, data)
                if(input.isInstanceOf[VectorView[_]])
                  if(input.isInstanceOf[DoubleVector])
                    DevPtrs = DevPtrs ::: List(newDevPtr + in.asInstanceOf[VectorView[_]].start*8)
                  else
                    DevPtrs = DevPtrs ::: List(newDevPtr + in.asInstanceOf[VectorView[_]].start*4)
                else
                  DevPtrs = DevPtrs ::: List(newDevPtr)
                //Post process for synchronization
                //timeStamp(currentStream) = timeStamp(currentStream) + 1
                //driver.cudaSched(DeliteCuda.gKernelHandler(1), timeStampPtr(currentStream), timeStamp(currentStream), stream(currentStream))
                //kernelCache.put(data, Tuple2(currentStream, timeStamp(currentStream)))
                //issuedQueue(currentStream).put(Tuple2(in, timeStamp(currentStream)))
              }
            }
            else {
              println("ERROR! Not Supported Input Type!")
              //TODO: Do something for DelitePrimitives
            }
          }
        }

        Profiler.stop("PREE")

        /*** Now all inputs are ready in GPU memory ***/
        val gout = proxy.op.getGPUOutput
        val elms = gout.asInstanceOf[GPUable[_]].gpu_data
        outSize = gout.asInstanceOf[GPUable[_]].gpu_datasize

        // Get a cache block for the result (only when this is not a mutable OP)
        //val isMutableOp = proxy.op.isInstanceOf[DeliteOP_MutableSingleTask[_]]
        val isMutableOp = if(proxy.op.getMutableDeps!=null) true else false
        val devptr_out = if(isMutableOp) 0 else getCacheBlock(outSize)
        if(!isMutableOp) {
          DevPtrs = (DevPtrs ::: List(devptr_out))
          // Add output array to the cache
          cache.put(elms, Tuple2(devptr_out,getSizeIdx(outSize)))
          cacheBack.put(devptr_out, elms)
        }

        // Fill in the kernel argument list (device pointers)
        argsQueue.clear
        val numPtrs = DevPtrs.length
        var i = 0
        while(i < numPtrs){
          argsQueue.put(new java.lang.Long(DevPtrs(i)))
          i += 1
        }

        // Fill in the kernel argument list (constants)
        val gpuConsts = proxy.op.getGPUConsts
        if(gpuConsts != null) {
          val numConsts = gpuConsts.length
          i=0
          while(i < numConsts) {
            if(gpuConsts(i).isInstanceOf[Int])
              argsQueue.put(new java.lang.Integer(gpuConsts(i).asInstanceOf[Int]))
            else if(gpuConsts(i).isInstanceOf[Long])
              argsQueue.put(new java.lang.Long(gpuConsts(i).asInstanceOf[Long]))
            else if(gpuConsts(i).isInstanceOf[Float])
              argsQueue.put(new java.lang.Float(gpuConsts(i).asInstanceOf[Float]))
            else if(gpuConsts(i).isInstanceOf[Double])
              argsQueue.put(new java.lang.Double(gpuConsts(i).asInstanceOf[Double]))
            i += 1
          }
        }
        
        /*** Launch GPU Kernels ***/
        //Profiler.startKernel(proxy.op.getGPUKernelId(0))
        //println("Here : " + proxy.op.toString)

        proxy.op.getGPUKernelId(1) match {
          case DeliteCuda.AsyncLaunch1D => {
            val dims = proxy.op.getGPUKernelDims
            if(dims.length == 1)
              driver.cudaAsyncLaunch1D(gKernelHandler(proxy.op.getGPUKernelId(0)), argsQueue, dims(0), stream(currentStream))
            else if(dims.length == 2)
              driver.cudaAsyncLaunch1DSpec(gKernelHandler(proxy.op.getGPUKernelId(0)), argsQueue, dims(0), dims(1), stream(currentStream))
            else
              println("ERROR(AsyncLaunch1D): Incorrect number of dimension sizes")
          }
          case DeliteCuda.AsyncLaunch2D => {
            val dims = proxy.op.getGPUKernelDims
            if(dims.length == 2)
              driver.cudaAsyncLaunch2D(gKernelHandler(proxy.op.getGPUKernelId(0)), argsQueue, dims(0), dims(1), stream(currentStream))
            else if(dims.length == 4)
              driver.cudaAsyncLaunch2DSpec(gKernelHandler(proxy.op.getGPUKernelId(0)), argsQueue, dims(0), dims(1), dims(2), dims(3), stream(currentStream))
            else
              println("ERROR(AsyncLaunch2D): Incorrect number of dimension sizes")
          }
          case DeliteCuda.Async3D3I => {
            val const = proxy.op.getGPUConsts
            driver.cudaAsync3D3I(gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), DevPtrs(2), const(0).asInstanceOf[Int], const(1).asInstanceOf[Int], const(2).asInstanceOf[Int], stream(currentStream))
          }
          case _ => println("ERROR! Not supported GPU kernel launcher!")
        }

        //driver.cudaStreamSync(stream(currentStream))
        //Profiler.stopKernel(proxy.op.getGPUKernelId(0))

        // Execute PostKernel
        timeStamp(currentStream) = timeStamp(currentStream) + 1
        driver.cudaSched(gKernelHandler(1), timeStampDevPtr(currentStream), timeStamp(currentStream), stream(currentStream))

        // Create GPUTaskNode instance for later cleanup
        val newTaskNode = new GPUTaskNode(proxy, currentStream, timeStamp(currentStream))
		while(issuedQueue(currentStream).remainingCapacity < 1) { processPostKernel }
        issuedQueue(currentStream).put(newTaskNode)

        // Save new output to proxy cvalue
        saveProxyValue(gout, proxy)

        processPostKernel
        

      } // if(proxy != null)
      else {
        if(cleanup == true) {
          this.synchronized {
            cleanup = false
            cleanup_start
            cleanup_done = true
          }
          println("GPU cache cleanup is done!")
        }
      }
    } // while loop

    println("Cache miss count("+id+") : " + cacheMissCount)
    Profiler.printAll

    if(id == 0)
      driver.cudaDestroyContext(id)
    
    //for(i <- 0 until NumStreams)
    //	println("TS is " + driver.readDevTimeStamp(timeStampHostPtr(i)))

    println("Exiting GPU Executor Thread: " + id)
  } // End of run() fucntion


  def processPostKernel {
    // TODO: Add stuffs whatever needs to be done when a kernel terminates
    if(terminationCheckCnt == 0) {
      terminationCheckCnt = terminationCheckChunk
      var iter = 0
      while(iter < 1) {  //while(iter < NumStreams) {
        val ts = driver.readDevTimeStamp(timeStampHostPtr(iter))
        var GPUTask = issuedQueue(iter).peek()
        while( (GPUTask!=null) && (GPUTask.timeStamp<ts) ) {
          //println("Cleanup: stream " + iter + ", timestamp " + queueVal._2)
          clearDeps(GPUTask.proxy)
          cleanup(GPUTask.proxy)
          issuedQueue(iter).poll()
          GPUTask = issuedQueue(iter).peek()
        }
        iter = iter + 1
      }
    }
    else {
      terminationCheckCnt = terminationCheckCnt - 1
    }
  }
  
  def clearDeps(proxy: DeliteDSLType) {
    if (proxy.op.getImmutableDeps != null) {
      val iter = proxy.op.getImmutableDeps.iterator
      while (iter.hasNext) {
        val dep = iter.next
        dep.synchronized {
          dep.outstandingOPs.remove(proxy) //completed, so remove from outstanding list
        }
      }
    }
    if (proxy.op.getMutableDeps != null) {
      val iter = proxy.op.getMutableDeps.iterator
      while (iter.hasNext) {
        val dep = iter.next
        dep.synchronized {
          dep.outstandingOPs.remove(proxy)
          if (dep.lastMutatingOP == proxy) { //speedup future pure ops
            dep.lastMutatingOP = null
          }
        }
      }
    }
  }

  def cleanup(proxy: DeliteDSLType) {
    val iter = proxy.inputs.iterator
    while (iter.hasNext) {
      val input = iter.next
      input.outputs.remove(proxy)
    }
    proxy.op = null
    proxy.antiDeps = null
    proxy.inputs = null
  }
  
  def shutdown {
    println("Shutting down GPU Thread: "+ id)

    _shutdown = true
    this.synchronized {
      this.notify
    }
  }

  def saveProxyValue(gout: DeliteDSLType, proxy: DeliteDSLType) {
	//println("Saving proxy for " + proxy.op.toString)
	//if(gout == null) println("NUL")
    proxy.synchronized {
      proxy.cvalue = gout.asInstanceOf[proxy.DSLType]
		  //if(proxy.cvalue==null) println("This is null")
      proxy.concretize
	  //if(proxy.cvalue == null) println("This is null")
    }
  }

  def notifyWaiters[T <: DeliteDSLType](proxy:T) {
    proxy.synchronized {
      proxy.concretize
      proxy.isComputed = true
      if(proxy.forcing)
        proxy.notifyAll
    }
  }

  def handleFastQueue {
    if(fastQueue.isEmpty == false) {
      val proxy = fastQueue.peek
			if(proxy.cvalue != null) {  //Sent to device
				//Profiler.start("POST")
        //TODO: Check issuedQueue to avoid synchronization
        if(proxy.op.getMutableDeps == null) {  // Immutable OP
				//Profiler.start("POST2")
          driver.cudaStreamSync(stream(0))
				//Profiler.stop("POST2")
          val data = proxy.cvalue.asInstanceOf[GPUable[_]].gpu_data
          val size = proxy.cvalue.asInstanceOf[GPUable[_]].gpu_datasize
          var elms: Array[_] = null
          if(cache.containsKey(data)) {
            val devptr = cache.get(data)._1
            //TODO: set the stream counter to appropriate value
            if(proxy.cvalue.isInstanceOf[DoubleVector] || proxy.cvalue.isInstanceOf[DoubleMatrix]) {
              val newArray = new Array[Double](size)
              proxy.cvalue.asInstanceOf[GPUable[Double]].gpu_setdata(newArray)
              driver.cudaMemCpyDtoHAsync(newArray, devptr, 0, size, PageLockedMem_Out, stream(0))
              elms = newArray
            }
            else if(proxy.cvalue.isInstanceOf[FloatVector] || proxy.cvalue.isInstanceOf[FloatMatrix]) {
              val newArray = new Array[Float](size)
              proxy.cvalue.asInstanceOf[GPUable[Float]].gpu_setdata(newArray)
              driver.cudaMemCpyDtoHAsyncFloat(newArray, devptr, 0, size, PageLockedMem_Out, stream(0))
              elms = newArray
            }
            else if(proxy.cvalue.isInstanceOf[IntVector] || proxy.cvalue.isInstanceOf[IntMatrix]) {
              val newArray = new Array[Int](size)
              proxy.cvalue.asInstanceOf[GPUable[Int]].gpu_setdata(newArray)
              driver.cudaMemCpyDtoHAsyncInt(newArray, devptr, 0, size, PageLockedMem_Out, stream(0))
              elms = newArray
            }
            else {
              println("ERROR: Not supported DSLType!")
            }
            // Update cache entries
            cache.remove(data)
            cache.put(elms,Tuple2(devptr,getSizeIdx(size)))
            cacheBack.put(devptr,elms)
            notifyWaiters(proxy)
						fastQueue.remove(proxy)
            //println("Fast Queue Done!")
          }
          else {
						println("ERROR: The data requested is already evicted!")
					}
        }
        else { // Mutable OP
          driver.cudaStreamSync(stream(0))
          val iter = proxy.op.getMutableDeps.iterator
          while(iter.hasNext) {
            val dep = iter.next
            val data = dep.asInstanceOf[GPUable[_]].gpu_data
            val size = dep.asInstanceOf[GPUable[_]].gpu_datasize
            if(cache.containsKey(data)) {
              val devptr = cache.get(data)._1
              if(dep.isInstanceOf[DoubleVector] || dep.isInstanceOf[DoubleMatrix]) {
                driver.cudaMemCpyDtoHAsync(data.asInstanceOf[Array[Double]], devptr, 0, size, PageLockedMem_Out, stream(0))
              }
              else if(proxy.cvalue.isInstanceOf[FloatVector] || proxy.cvalue.isInstanceOf[FloatMatrix]) {
                driver.cudaMemCpyDtoHAsyncFloat(data.asInstanceOf[Array[Float]], devptr, 0, size, PageLockedMem_Out, stream(0))
              }
              else if(proxy.cvalue.isInstanceOf[IntVector] || proxy.cvalue.isInstanceOf[IntMatrix]) {
                driver.cudaMemCpyDtoHAsyncInt(data.asInstanceOf[Array[Int]], devptr, 0, size, PageLockedMem_Out, stream(0))
              }
              else {
                println("ERROR: Not supported DSLType!")
              }
            }
            else {
              println("ERROR: The data requested is already evicted!")
            }
          }
          notifyWaiters(proxy)
          fastQueue.remove(proxy)
        }
				//Profiler.stop("POST")
      }
    }
  }//def handleFastQueue
  
}



 ///////////////////////////////////////////////////////////////////////
 /////////////////////////// Old Codes /////////////////////////////////
 ///////////////////////////////////////////////////////////////////////

/*
        if(proxy.isInstanceOf[DelitePrimitive[_]]) {
          kernelCache.put(elms(0).asInstanceOf[DelitePrimitive[_]], Tuple2(currentStream, timeStamp(currentStream)))
          issuedQueue(currentStream).put(Tuple2(elms(0).asInstanceOf[DelitePrimitive[_]], timeStamp(currentStream)))
          // Add postkernel for kernel termination check
          // TODO: Call this postkernel more coarse-grained manner
          driver.cudaSched(DeliteCuda.gKernelHandler(1), timeStampDevPtr(currentStream), timeStamp(currentStream), DeliteCuda.stream(currentStream))
          // Put the output devptr to cache
          cache.put(elms(0).asInstanceOf[DelitePrimitive[_]], Tuple2(devptr_out,getSizeIdx(outSize)))
          cacheBack.put(devptr_out, elms(0).asInstanceOf[DelitePrimitive[_]])
        }
        else {
          if(!isMutableOp) {
            kernelCache.put(elms, Tuple2(currentStream, timeStamp(currentStream)))
            issuedQueue(currentStream).put(Tuple2(elms, timeStamp(currentStream)))
            // Add postkernel for kernel termination check
            // TODO: Call this postkernel more coarse-grained manner
            driver.cudaSched(DeliteCuda.gKernelHandler(1), timeStampDevPtr(currentStream), timeStamp(currentStream), DeliteCuda.stream(currentStream))
            // Put the output devptr to cache
            cache.put(elms, Tuple2(devptr_out,getSizeIdx(outSize)))
            cacheBack.put(devptr_out, elms)
          }
        }
        */

        /*
        else if(proxy.cvalue.isInstanceOf[FloatVector]) {
          val data = proxy.cvalue.asInstanceOf[GPUable[_]].gpu_data
					val cvalue = proxy.cvalue.asInstanceOf[FloatVector]
					val elms = new Array[Float](cvalue.length)
					if(cache.containsKey(data)) {
            val devptr = cache.get(data)._1
						driver.cudaStreamSync(DeliteCuda.stream(0))
						driver.cudaMemCpyDtoHAsyncFloat(elms, devptr, 0, cvalue.length, DeliteCuda.PageLockedMem_Out, DeliteCuda.stream(0))
						// Update cache entries
            cache.remove(data)
            cache.put(elms,Tuple2(devptr,getSizeIdx(cvalue.length)))
            cacheBack.put(devptr,elms)
            saveProxyValueWithWakeup(proxy.asInstanceOf[Vector[Float]], Vector[Float](elms, cvalue.is_row, cvalue.length))
						fastQueue.remove(proxy)
            //println("Fast Queue Done!")
					}
					else { // We are in trouble
            println("OP1: " + proxy.op.toString)
						println("ERROR: OOPS! The value you need is evicted already!")
					}
				}
				else if(proxy.cvalue.isInstanceOf[FloatMatrix]) {
          val data = proxy.cvalue.asInstanceOf[GPUable[_]].gpu_data
          val cvalue = proxy.cvalue.asInstanceOf[FloatMatrix]
          val elms = new Array[Float](cvalue.size)
          if(cache.containsKey(data)) {
            val devptr = cache.get(data)._1
            driver.cudaStreamSync(DeliteCuda.stream(0))
            driver.cudaMemCpyDtoHAsyncFloat(elms, devptr, 0, cvalue.size, DeliteCuda.PageLockedMem_Out, DeliteCuda.stream(0))
            // Update cache entries
            cache.remove(data)
            cache.put(elms,Tuple2(devptr,getSizeIdx(cvalue.size)))
            cacheBack.put(devptr,elms)
            saveProxyValueWithWakeup(proxy.asInstanceOf[Matrix[Float]], Matrix[Float](elms, cvalue.numRows, cvalue.numCols))
            fastQueue.remove(proxy)
            //println("Fast Queue Done!")
          }
          else { // We are in trouble
            println("OP2: " + proxy.op.toString)
            println("ERROR: OOPS! The value you need is evicted already!")
          }
        }
        else if(proxy.cvalue.isInstanceOf[IntVector]) {
          val data = proxy.cvalue.asInstanceOf[GPUable[_]].gpu_data
          val cvalue = proxy.cvalue.asInstanceOf[IntVector]
          val elms = new Array[Int](cvalue.length)
          if(cache.containsKey(data)) {
            val devptr = cache.get(data)._1
            driver.cudaStreamSync(DeliteCuda.stream(0))
            driver.cudaMemCpyDtoHAsyncInt(elms, devptr, 0, cvalue.length, DeliteCuda.PageLockedMem_Out, DeliteCuda.stream(0))
            // Update cache entries
            cache.remove(data)
            cache.put(elms,Tuple2(devptr,getSizeIdx(cvalue.length)))
            cacheBack.put(devptr,elms)
            saveProxyValueWithWakeup(proxy.asInstanceOf[Vector[Int]], Vector[Int](elms, cvalue.is_row, cvalue.length))
            fastQueue.remove(proxy)
            //println("Fast Queue Done!")
          }
          else { // We are in trouble
            println("OP1: " + proxy.op.toString)
            println("ERROR: OOPS! The value you need is evicted already!")
          }
				}
				*/
				//else {
				//	println("ERROR: This is not supported DeliteDSLType!" + proxy.op.toString)
				//}
			//}
    //}
  //} //def handleFastQueue

  /*
  def wakeupMutableOp[T <: DeliteDSLType](proxy: T) {
	  proxy.synchronized {
	  proxy.isComputed = true
		if (proxy.forcing)
			proxy.notifyAll
	  }
  }
   */
  /*
  //def saveProxyValueWithWakeup[T <: DeliteDSLType](proxy: DeliteProxy[T], elms:T ) {
  def saveProxyValueWithWakeup[T <: DeliteDSLType](proxy: T, elms:T ) {
	  proxy.synchronized {
		proxy.cvalue = elms.asInstanceOf[proxy.DSLType]
		proxy.concretize
	  proxy.isComputed = true
		if (proxy.forcing)
			proxy.notifyAll
	  }
	  //proxy.op = null
  }
  */
  
  /*
  def handleFastQueue {
    if(fastQueue.isEmpty == false) {
      val proxy = fastQueue.peek
			if(proxy.cvalue != null) {  //Sent to device
				//println("Here")
        //val data = proxy.cvalue.asInstanceOf[GPUable[_]].gpu_data
				if(proxy.cvalue.isInstanceOf[DoubleVector]) {
          val data = proxy.cvalue.asInstanceOf[GPUable[_]].gpu_data
					val cvalue = proxy.cvalue.asInstanceOf[DoubleVector]
					val elms = new Array[Double](cvalue.length)
					if(cache.containsKey(data)) {
            val devptr = cache.get(data)._1
						driver.cudaStreamSync(DeliteCuda.stream(0))
						driver.cudaMemCpyDtoHAsync(elms, devptr, 0, cvalue.length, DeliteCuda.PageLockedMem_Out, DeliteCuda.stream(0))
						// Update cache entries
            cache.remove(data)
            cache.put(elms,Tuple2(devptr,getSizeIdx(cvalue.length)))
            cacheBack.put(devptr,elms)
            saveProxyValueWithWakeup(proxy.asInstanceOf[Vector[Double]], Vector[Double](elms, cvalue.is_row, cvalue.length))
						fastQueue.remove(proxy)
            //println("Fast Queue Done!")
					}
					else { // We are in trouble
            println("OP1: " + proxy.op.toString)
						println("ERROR: OOPS! The value you need is evicted already!")
					}
				}
				else if(proxy.cvalue.isInstanceOf[DoubleMatrix]) {
          //if(proxy.originalImpl.isEmpty) { //Immutable OP
            val data = proxy.cvalue.asInstanceOf[GPUable[_]].gpu_data
            val cvalue = proxy.cvalue.asInstanceOf[DoubleMatrix]
            val elms = new Array[Double](cvalue.size)
            if(cache.containsKey(data)) {
              val devptr = cache.get(data)._1
              driver.cudaStreamSync(DeliteCuda.stream(0))
              driver.cudaMemCpyDtoHAsync(elms, devptr, 0, cvalue.size, DeliteCuda.PageLockedMem_Out, DeliteCuda.stream(0))
              // Update cache entries
              cache.remove(data)
              cache.put(elms,Tuple2(devptr,getSizeIdx(cvalue.size)))
              cacheBack.put(devptr,elms)
              saveProxyValueWithWakeup(proxy.asInstanceOf[Matrix[Double]], Matrix[Double](elms, cvalue.numRows, cvalue.numCols))
              fastQueue.remove(proxy)
              //println("Fast Queue Done!")
            }
            else { // We are in trouble
              println("OP2: " + proxy.op.toString)
              println("ERROR: OOPS! The value you need is evicted already!")
            }
          //}
          /*
          else {
            val elms = proxy.originalImpl.get(0).asInstanceOf[GPUable[_]].gpu_data.asInstanceOf[Array[Double]]
            val data = elms
            if(cache.containsKey(data)) {
              val devptr = cache.get(data)._1
              driver.cudaStreamSync(DeliteCuda.stream(0))
              driver.cudaMemCpyDtoHAsync(elms, devptr, 0, elms.length , DeliteCuda.PageLockedMem_Out, DeliteCuda.stream(0))
              // Todo: Update cache entries
              wakeupMutableOp(proxy.asInstanceOf[Matrix[Double]])
              fastQueue.remove(proxy)
              //println("Fast Queue Done!" )
            }
            else { // We are in trouble
              println("OP2: " + proxy.op.toString)
              println("ERROR: OOPS! The value you need is evicted already!")
            }
          }
          */
        }
        else if(proxy.cvalue.isInstanceOf[FloatVector]) {
          val data = proxy.cvalue.asInstanceOf[GPUable[_]].gpu_data
					val cvalue = proxy.cvalue.asInstanceOf[FloatVector]
					val elms = new Array[Float](cvalue.length)
					if(cache.containsKey(data)) {
            val devptr = cache.get(data)._1
						driver.cudaStreamSync(DeliteCuda.stream(0))
						driver.cudaMemCpyDtoHAsyncFloat(elms, devptr, 0, cvalue.length, DeliteCuda.PageLockedMem_Out, DeliteCuda.stream(0))
						// Update cache entries
            cache.remove(data)
            cache.put(elms,Tuple2(devptr,getSizeIdx(cvalue.length)))
            cacheBack.put(devptr,elms)
            saveProxyValueWithWakeup(proxy.asInstanceOf[Vector[Float]], Vector[Float](elms, cvalue.is_row, cvalue.length))
						fastQueue.remove(proxy)
            //println("Fast Queue Done!")
					}
					else { // We are in trouble
            println("OP1: " + proxy.op.toString)
						println("ERROR: OOPS! The value you need is evicted already!")
					}
				}
				else if(proxy.cvalue.isInstanceOf[FloatMatrix]) {
          //if(proxy.originalImpl.isEmpty) { //Immutable OP
            val data = proxy.cvalue.asInstanceOf[GPUable[_]].gpu_data
            val cvalue = proxy.cvalue.asInstanceOf[FloatMatrix]
            val elms = new Array[Float](cvalue.size)
            if(cache.containsKey(data)) {
              val devptr = cache.get(data)._1
              driver.cudaStreamSync(DeliteCuda.stream(0))
              driver.cudaMemCpyDtoHAsyncFloat(elms, devptr, 0, cvalue.size, DeliteCuda.PageLockedMem_Out, DeliteCuda.stream(0))
              // Update cache entries
              cache.remove(data)
              cache.put(elms,Tuple2(devptr,getSizeIdx(cvalue.size)))
              cacheBack.put(devptr,elms)
              saveProxyValueWithWakeup(proxy.asInstanceOf[Matrix[Float]], Matrix[Float](elms, cvalue.numRows, cvalue.numCols))
              fastQueue.remove(proxy)
              //println("Fast Queue Done!")
            }
            else { // We are in trouble
              println("OP2: " + proxy.op.toString)
              println("ERROR: OOPS! The value you need is evicted already!")
            }
          //}
          /*
          else {
            val elms = proxy.originalImpl.get(0).asInstanceOf[GPUable[_]].gpu_data.asInstanceOf[Array[Float]]
            val data = elms
            if(cache.containsKey(data)) {
              val devptr = cache.get(data)._1
              driver.cudaStreamSync(DeliteCuda.stream(0))
              driver.cudaMemCpyDtoHAsyncFloat(elms, devptr, 0, elms.length , DeliteCuda.PageLockedMem_Out, DeliteCuda.stream(0))
              // Todo: Update cache entries
              wakeupMutableOp(proxy.asInstanceOf[Matrix[Float]])
              fastQueue.remove(proxy)
              //println("Fast Queue Done!" )
            }
            else { // We are in trouble
              println("OP2: " + proxy.op.toString)
              println("ERROR: OOPS! The value you need is evicted already!")
            }
          }
          */
        }
        else if(proxy.cvalue.isInstanceOf[IntVector]) {
            val data = proxy.cvalue.asInstanceOf[GPUable[_]].gpu_data
            val cvalue = proxy.cvalue.asInstanceOf[IntVector]
            val elms = new Array[Int](cvalue.length)
            if(cache.containsKey(data)) {
              val devptr = cache.get(data)._1
              driver.cudaStreamSync(DeliteCuda.stream(0))
              driver.cudaMemCpyDtoHAsyncInt(elms, devptr, 0, cvalue.length, DeliteCuda.PageLockedMem_Out, DeliteCuda.stream(0))
              // Update cache entries
              cache.remove(data)
              cache.put(elms,Tuple2(devptr,getSizeIdx(cvalue.length)))
              cacheBack.put(devptr,elms)
              saveProxyValueWithWakeup(proxy.asInstanceOf[Vector[Int]], Vector[Int](elms, cvalue.is_row, cvalue.length))
              fastQueue.remove(proxy)
              //println("Fast Queue Done!")
            }
            else { // We are in trouble
              println("OP1: " + proxy.op.toString)
              println("ERROR: OOPS! The value you need is evicted already!")
            }
				  }
        else if(proxy.cvalue.isInstanceOf[DeliteDouble]) {
          val data = proxy.cvalue.asInstanceOf[GPUable[_]].gpu_data
					val cvalue = proxy.cvalue.asInstanceOf[DeliteDouble]
					val elms = new Array[Double](1)
					if(cache.containsKey(data(0))) {
						val devptr = cache.get(data(0))._1
						driver.cudaStreamSync(DeliteCuda.stream(0))
						driver.cudaMemCpyDtoHAsync(elms, devptr, 0, 1, DeliteCuda.PageLockedMem_Out, DeliteCuda.stream(0))
           // println("value is " + elms(0))
						saveProxyValueWithWakeup(proxy.asInstanceOf[DeliteDouble], DeliteDouble(elms(0)))
            //TODO: Replace cache reference
						fastQueue.remove(proxy)
            //println("Fast Queue Done!")
					}
					else { // We are in trouble
            println("OP2: " + proxy.op.toString)
						println("ERROR: OOPS! The value you need is evicted already!")
					}
				}  
				else {
					println("ERROR: This is not supported DeliteDSLType!" + proxy.op.toString)
				}
			}
    }
  } //def handleFastQueue
  */



/*
 if(fastQueue.isEmpty == false) {

      val proxy = fastQueue.peek
			if(proxy.cvalue != null) {  //Sent to device
				val data = proxy.cvalue.asInstanceOf[GPUable[_]].gpu_data

        /*
        if(proxy.cvalue.isInstanceOf[DelitePrimitive]) {
          val cvalue = proxy.cvalue.asInstanceOf[proxy.DSLType]
					val elms = new Array[Int](cvalue.length)

					if(cache.containsKey(data)) {
						val devptr = cache.get(data)
						driver.cudaStreamSync(DeliteCuda.stream(0))
						//TODO: Check this offset
						driver.cudaMemCpyDtoHAsyncInt(elms, devptr, 0, cvalue.length, DeliteCuda.PageLockedMem_Out, DeliteCuda.stream(0))
						saveProxyValueWithWakeup(proxy.asInstanceOf[proxy.DSLType], Vector[Int](elms, cvalue.is_row, cvalue.length))
						fastQueue.remove(proxy)
						//println("DONE: Fast queue done!")
					}
					else { // We are in trouble
						println("ERROR: OOPS! The value you need is evicted already!")
					}
				}
				*/
        if(proxy.cvalue.isInstanceOf[IntVector]) {

          val cvalue = proxy.cvalue.asInstanceOf[IntVector]
					val elms = new Array[Int](cvalue.length)

					if(cache.containsKey(data)) {
						val devptr = cache.get(data)
						driver.cudaStreamSync(DeliteCuda.stream(0))
						//TODO: Check this offset
						driver.cudaMemCpyDtoHAsyncInt(elms, devptr, 0, cvalue.length, DeliteCuda.PageLockedMem_Out, DeliteCuda.stream(0))
						saveProxyValueWithWakeup(proxy.asInstanceOf[Vector[Int]], Vector[Int](elms, cvalue.is_row, cvalue.length))
						///////////////////TEST////////////////////
            //proxy.isScheduledOnGPU = false
						//println("DONE: Fast queue done!")
					}
					else { // We are in trouble
						println("ERROR: OOPS! The value you need is evicted already!")
					}

				}
        else if(proxy.cvalue.isInstanceOf[FloatVector]) {
					val cvalue = proxy.cvalue.asInstanceOf[FloatVector]
					val elms = new Array[Float](cvalue.length)

					if(cache.containsKey(data)) {
						val devptr = cache.get(data)
						driver.cudaStreamSync(DeliteCuda.stream(0))
						//TODO: Check this offset
						driver.cudaMemCpyDtoHAsyncFloat(elms, devptr, 0, cvalue.length, DeliteCuda.PageLockedMem_Out, DeliteCuda.stream(0))
						//saveProxyValueWithWakeup(proxy.asInstanceOf[DeliteProxy[Vector[Float]]], Vector[Float](elms, cvalue.is_row, cvalue.offset, cvalue.stridee, cvalue.length))
						saveProxyValueWithWakeup(proxy.asInstanceOf[Vector[Float]], Vector[Float](elms, cvalue.is_row, cvalue.length))
						fastQueue.remove(proxy)
						//println("DONE: Fast queue done!")
					}
					else { // We are in trouble
						println("ERROR: OOPS! The value you need is evicted already!")
					}
				}
				else if(proxy.cvalue.isInstanceOf[FloatMatrix]) {
					val cvalue = proxy.cvalue.asInstanceOf[FloatMatrix]
					val elms = new Array[Float](cvalue.size)

					if(cache.containsKey(data)) {
						val devptr = cache.get(data)
						driver.cudaStreamSync(DeliteCuda.stream(0))
						driver.cudaMemCpyDtoHAsyncFloat(elms, devptr, 0, cvalue.size, DeliteCuda.PageLockedMem_Out, DeliteCuda.stream(0))
						//saveProxyValueWithWakeup(proxy.asInstanceOf[DeliteProxy[Matrix[Float]]], Matrix[Float](elms, cvalue.size/cvalue.numRows, cvalue.numRows))
						saveProxyValueWithWakeup(proxy.asInstanceOf[Matrix[Float]], Matrix[Float](elms, cvalue.numRows, cvalue.numCols))
						fastQueue.remove(proxy)
						//println("DONE: Fast queue done!")
					}
					else { // We are in trouble
						println("ERROR: OOPS! The value you need is evicted already!")
					}
				}
				else if(proxy.cvalue.isInstanceOf[DoubleVector]) {
					val cvalue = proxy.cvalue.asInstanceOf[DoubleVector]
					val elms = new Array[Double](cvalue.length)

					if(cache.containsKey(data)) {
						val devptr = cache.get(data)
						driver.cudaStreamSync(DeliteCuda.stream(0))
						//TODO: Check this offset
						driver.cudaMemCpyDtoHAsync(elms, devptr, 0, cvalue.length, DeliteCuda.PageLockedMem_Out, DeliteCuda.stream(0))
            //println("OUT is " + elms(0) + "," + elms(1))
						//saveProxyValueWithWakeup(proxy.asInstanceOf[DeliteProxy[Vector[Double]]], Vector[Double](elms, cvalue.is_row, cvalue.offset, cvalue.stridee, cvalue.length))
            ////////////TEST////////////////
            //proxy.isScheduledOnGPU = false
						saveProxyValueWithWakeup(proxy.asInstanceOf[Vector[Double]], Vector[Double](elms, cvalue.is_row, cvalue.length))
						fastQueue.remove(proxy)

						//println("DONE: Fast queue done!")
					}
					else { // We are in trouble
						println("ERROR: OOPS! The value you need is evicted already!")
					}
				}
				else if(proxy.cvalue.isInstanceOf[DoubleMatrix]) {
					val cvalue = proxy.cvalue.asInstanceOf[DoubleMatrix]
					val elms = new Array[Double](cvalue.size)

					if(cache.containsKey(data)) {
						val devptr = cache.get(data)
						driver.cudaStreamSync(DeliteCuda.stream(0))
						driver.cudaMemCpyDtoHAsync(elms, devptr, 0, cvalue.size, DeliteCuda.PageLockedMem_Out, DeliteCuda.stream(0))
            //println("OUT is " + elms(0) + "," + elms(1))
						//saveProxyValueWithWakeup(proxy.asInstanceOf[DeliteProxy[Matrix[Double]]], Matrix[Double](elms, cvalue.size/cvalue.numRows, cvalue.numRows))
						saveProxyValueWithWakeup(proxy.asInstanceOf[Matrix[Double]], Matrix[Double](elms, cvalue.numRows, cvalue.numCols))
						fastQueue.remove(proxy)
						//println("DONE: Fast queue done!")
					}
					else { // We are in trouble
						println("ERROR: OOPS! The value you need is evicted already!")
					}
				}
				else {
					println("ERROR: This is not supported DeliteDSLType!")
				}
			}

	  	}
 */

  /*
          // K-means MAP call
          /*
          case DeliteCuda.AsyncKM1 => {
            val const = proxy.op.getGPUConsts
			      driver.cudaAsyncKM1(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), DevPtrs(2), const(0).asInstanceOf[Int], const(1).asInstanceOf[Int], const(2).asInstanceOf[Int], const(3).asInstanceOf[Int], DeliteCuda.stream(currentStream))
		      }
		      */
          //case DeliteCuda.AsyncKM2 => driver.cudaAsyncKM2(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), DevPtrs(2), 500, 200, 32, DeliteCuda.stream(currentStream))
          //case DeliteCuda.AsyncKM3 => driver.cudaAsyncKM3(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), DevPtrs(2), 32, 200, DeliteCuda.stream(currentStream))
          // RBM JNI calls
          //case DeliteCuda.AsyncRBM_1I => driver.cudaAsyncRBM_1I(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), outSize, DeliteCuda.stream(currentStream))
          /*
          case DeliteCuda.AsyncRBM_1I1S => {
            if(proxy.op.getGPUConsts(0).isInstanceOf[Double])
              driver.cudaAsyncRBM_1I1S(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), proxy.op.getGPUConsts(0).asInstanceOf[Double], outSize, DeliteCuda.stream(currentStream))
            else
              driver.cudaAsyncRBM_1I1S_Float(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), proxy.op.getGPUConsts(0).asInstanceOf[Float], outSize, DeliteCuda.stream(currentStream))
          }
          */
          //case DeliteCuda.AsyncRBM_2I => driver.cudaAsyncRBM_2I(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), DevPtrs(2), outSize, DeliteCuda.stream(currentStream))
          //case DeliteCuda.AsyncRBM_Repmat => driver.cudaAsyncRBM_Repmat(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), inSize, proxy.op.getGPUConsts(0).asInstanceOf[Int], proxy.op.getGPUConsts(1).asInstanceOf[Int], DeliteCuda.stream(currentStream))
          //case DeliteCuda.AysncRBM_1I2D => driver.cudaAsyncRBM_1I2D(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), proxy.op.getGPUConsts(0).asInstanceOf[Int], proxy.op.getGPUConsts(1).asInstanceOf[Int], DeliteCuda.stream(currentStream))
          //case DeliteCuda.AysncRBM_1I3D => driver.cudaAsyncRBM_1I3D(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), DevPtrs(2), proxy.op.getGPUConsts(0).asInstanceOf[Int], proxy.op.getGPUConsts(1).asInstanceOf[Int], DeliteCuda.stream(currentStream))

          //case DeliteCuda.Async2I2D => driver.cudaBLAS2DAsync(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), DevPtrs(2), inSize, DeliteCuda.stream(currentStream))
          //case DeliteCuda.Async1I1D => driver.cudaBLAS1DAsync(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), 0, DevPtrs(1), inSize, DeliteCuda.stream(currentStream))
          //case DeliteCuda.Async2I1D => driver.cudaBLAS1DAsync(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), DevPtrs(2), inSize, DeliteCuda.stream(currentStream))
          /*
          case DeliteCuda.AsyncMAP => {
            val const = proxy.op.getGPUConsts
            driver.cudaMapAsync(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), DevPtrs(2), outSize, const(1).asInstanceOf[Int], const(2).asInstanceOf[Int], const(3).asInstanceOf[Double], DeliteCuda.stream(currentStream))
          }
          */
          //case DeliteCuda.AsyncMAPLR => driver.cudaMapLRAsync(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), proxy.op.asInstanceOf[Vector.OP_mapLR[_,_]].x_cur, proxy.op.asInstanceOf[Vector.OP_mapLR[_,_]].tau, outSize, DeliteCuda.stream(currentStream))
          //case DeliteCuda.Async3D2I => driver.cudaAsync3D2I(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), 0, 0, 0, 4, DeliteCuda.stream(currentStream))
		      //case DeliteCuda.AsyncMatDotV => driver.cudaAsyncMatDotV(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), DevPtrs(2), gout.asInstanceOf[Matrix[_]].numCols, gout.asInstanceOf[Matrix[_]].numRows, DeliteCuda.stream(currentStream))
          /*
          case DeliteCuda.AsyncMdotV => {
            if(proxy.op.isInstanceOf[DoubleMatrix.OPGPU_vmult_single])
              driver.cudaAsyncMdotV(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), DevPtrs(2), proxy.op.asInstanceOf[DoubleMatrix.OPGPU_vmult_single].m.numRows, proxy.op.asInstanceOf[DoubleMatrix.OPGPU_vmult_single].m.numCols, DeliteCuda.stream(currentStream))
            else
              driver.cudaAsyncMdotV(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), DevPtrs(2), proxy.op.asInstanceOf[IntMatrix.OPGPU_vmult_single].m.numRows, proxy.op.asInstanceOf[IntMatrix.OPGPU_vmult_single].m.numCols, DeliteCuda.stream(currentStream))
          }
          */
          case DeliteCuda.Async3D3I => {
            val kernel=  proxy.op.getGPUKernelId(0)
            kernel match {
              /*
              case DeliteCuda.MatTransDouble=> {
                driver.cudaAsync3D3I(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), 0, gout.asInstanceOf[Matrix[_]].numRows, 0, gout.asInstanceOf[Matrix[_]].numCols, DeliteCuda.stream(currentStream))
              }
              case DeliteCuda.MatTransFloat=> {
                driver.cudaAsync3D3I(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), 0, gout.asInstanceOf[Matrix[_]].numRows, 0, gout.asInstanceOf[Matrix[_]].numCols, DeliteCuda.stream(currentStream))
              }
              */
              case _ => {
                if(proxy.op.isInstanceOf[DoubleMatrix.OPGPU_mult_single])
                  driver.cudaAsync3D3I(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), DevPtrs(2), proxy.op.asInstanceOf[DoubleMatrix.OPGPU_mult_single].collA.numCols, proxy.op.asInstanceOf[DoubleMatrix.OPGPU_mult_single].collB.numCols, proxy.op.asInstanceOf[DoubleMatrix.OPGPU_mult_single].collA.numRows, DeliteCuda.stream(currentStream))
                else
                  driver.cudaAsync3D3I(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), DevPtrs(2), proxy.op.asInstanceOf[FloatMatrix.OPGPU_mult_single].collA.numCols, proxy.op.asInstanceOf[FloatMatrix.OPGPU_mult_single].collB.numCols, proxy.op.asInstanceOf[FloatMatrix.OPGPU_mult_single].collA.numRows, DeliteCuda.stream(currentStream))
              }
            }
          }
          case DeliteCuda.Async3D3IReg => driver.cudaAsync3D3IReg(DeliteCuda.gKernelHandler(proxy.op.getGPUKernelId(0)), DevPtrs(0), DevPtrs(1), DevPtrs(2), proxy.op.asInstanceOf[DoubleMatrix.OPGPU_mult_single].collA.numCols, proxy.op.asInstanceOf[DoubleMatrix.OPGPU_mult_single].collB.numCols, proxy.op.asInstanceOf[DoubleMatrix.OPGPU_mult_single].collA.numRows, DeliteCuda.stream(currentStream))
          case _ => println("ERROR! Not supported GPU kernel launcher!")
            */
