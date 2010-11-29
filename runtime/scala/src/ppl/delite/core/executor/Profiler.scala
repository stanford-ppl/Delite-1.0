/* Profiler Object */

package ppl.delite.core.executor

import java.util.concurrent._
import java.util._
import ppl.delite.cuda._

class Bucket {
	var start:Long = 0;
	var stop:Long = 0;
	var elapsed:Long = 0;
	var count:Int = 0;

	def reset = {
		start = 0;
		stop = 0;
		elapsed = 0;
		count = 0;
	}
}

object Profiler  {
	val hashmap = new ConcurrentHashMap[String, Bucket];

	def start(key: String):Unit = { 
		if(hashmap.containsKey(key)) {
			val bucket = hashmap.get(key);
			bucket.start = System.nanoTime();
		}
		else {
      val newBucket = new Bucket;
      hashmap.put(key, newBucket);
      newBucket.start = System.nanoTime();
			//println("[ERROR:start] Key string (" + key + ") is not initialized!");
    }
	}

  def startKernel(kernelID: Int): Unit = {
    start(DeliteCuda.gKernelName(kernelID))
  }

	def stop(key: String):Unit = { 
		if(hashmap.containsKey(key)) {
			val bucket = hashmap.get(key);
			bucket.stop = System.nanoTime();
			bucket.elapsed += (bucket.stop-bucket.start) / 1000;
			bucket.count += 1;
		}
		else
			println("[ERROR:stop] Key string (" + key + ") is not initialized!");
	}

  def stopKernel(kernelID: Int): Unit = {
    stop(DeliteCuda.gKernelName(kernelID))
  }

	def reset(key: String):Unit = {
		if(hashmap.containsKey(key)) {
			val bucket = hashmap.get(key);
			bucket.reset;
		}
		else {
			println("[ERROR:reset] Key string (" + key + ") is not initialized!");
		}
	}

	def print(key: String):Unit = { 
		if(hashmap.containsKey(key)) {
			val bucket = hashmap.get(key);
			println(key + ": " + bucket.elapsed + " us, " + bucket.count + " triggers");
		}
		else {
			println("[ERROR] No such named profiler!");
		}
	}

	def printAll = {
		val total:Set[Map.Entry[String,Bucket]] = hashmap.entrySet;
		val iter:Iterator[Map.Entry[String,Bucket]] = total.iterator;
		while(iter.hasNext) {
			val next = iter.next;
			println(next.getKey + ": " + next.getValue.elapsed + " us, " + next.getValue.count + " triggers");
		}
	}

}
