package ppl.delite.core.executor

import ppl.delite.core.{DeliteDSLType, Delite}
import java.io.{FileWriter, BufferedWriter}
import ppl.delite.dsl.primitive.DeliteInt

/**
 * Author: Kevin J. Brown
 * Date: Mar 15, 2010
 * Time: 4:10:07 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class ExecutorVisualizer extends DeliteExecutor {

  //create output file
  val file = new BufferedWriter(new FileWriter("vis.txt"))

  override def run() {
    println("Visualization Executor is starting")
    //Delite.isExecutor.set(true)

    while(!_shutdown || submitQueue.isEmpty == false) {
      // var useful = false;
      val proxy = submitQueue.poll
      if(proxy != null) {
        processProxy(proxy)
      }
    }
    file.close
  }

  def processProxy(proxy: DeliteDSLType) {
    proxy.scheduled = true
    proxy.cvalue = proxy.op.seq
    proxy.concretize
    proxy.isComputed = true

    writeFile(proxy)

    proxy.op = null

    //check if you need to wake up main thread
    proxy.synchronized {
      if(proxy.forcing)
        proxy.notifyAll
    }
  }

  def objString(o: AnyRef): String = o.getClass.getName + "@" + o.hashCode.toHexString

  def writeFile(proxy: DeliteDSLType) {

    var name: String = proxy.op.toString
    file.write("name: " + name.substring(0, name.indexOf("(")))

    if (proxy.op.getImmutableDeps != null) {
      val iter = proxy.op.getImmutableDeps.iterator
      while (iter.hasNext) {
        val in: String = objString(iter.next)
        val begin = in.indexOf("@")
        var end = in.indexOf(" ", begin)
        end = if (end == -1) in.length else end
        file.write(" input: " + in.substring(begin, end))
        //file.write(" input: " + in)
      }
    }

    if (proxy.op.getMutableDeps != null) {
      val iter2 = proxy.op.getMutableDeps.iterator
      while (iter2.hasNext) {
        val in: String = objString(iter2.next)
        val begin = in.indexOf("@")
        var end = in.indexOf(" ", begin)
        end = if (end == -1) in.length else end
        file.write(" input: " + in.substring(begin, end))
        //file.write(" input: " + in)
      }
    }

    val out: String = objString(proxy)
    val begin = out.indexOf("@")
    var end = out.indexOf(" ", begin)
    end = if (end == -1) out.length else end
    file.write(" output: " + out.substring(begin, end))
    //file.write(" output: " + out)
    file.write(" cost: " + proxy.op.cost)
    file.write(" ")
    file.newLine
  }

}
