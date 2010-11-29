package ppl.delite.statistics

import java.io.FileWriter
import ppl.delite.core.executor._
import ppl.delite.core.include._
import ppl.delite.core.DeliteDSLType
// Statistics collection and visualization.

object DeliteStats extends Thread {

  private val SAMPLING_STATISTICS = false

  // print all the collected statistics
  def logStats() {
    if (researchMode) dumpText()
  }

  private val OL: Long = 0
  private val ML: Long = Integer.MAX_VALUE
  private val visualization = false
  private val frameSide = 100
  private val frameTail = 100
  private val statsFileName = "delite-stats.plt"
  private var writer: FileWriter = null
  private var active = false

  if (visualization) {
    Animation.init("DELITE statistics")
    Animation.setXscale(-frameSide, frameSide)
    Animation.setYscale(-frameSide, frameSide)
    Animation.clear(Animation.BLACK)
    Animation.show()
  }

  override def start() {
    if (researchMode) {
      active = true
      // open statistics dump file:
      writer = new FileWriter(statsFileName)
      gnuplotInit()
      //ETLOGGER info "digraph {"
      //ETLOGGER info "compound=true;"
      //ETLOGGER info "node [fontname=\"Lucida Sans Typewriter\", fontsize=10];"

    }
    if (SAMPLING_STATISTICS) super.start()
  }

  override def run() {
    if (SAMPLING_STATISTICS)
    while (active) {
      sampling()
      if (visualization) {
        display()
        decorations()
        Animation.show(frameTail)
      }
    }
  }

  @deprecated
  def shutdown() {
    if (researchMode) {
      //ETLOGGER info "}"
      writer.close()
    }
    if (SAMPLING_STATISTICS) super.stop()
  }

  // optimizer statistics
  var optSnapShot: List[Int] = List(0)

  // executor statistics
  // TODO: ask executor how many threads there are in the pool.
  val number_of_threads = Runtime.getRuntime.availableProcessors + 1
  private val exeTaskTimeMin: Array[Long] = Array.fromFunction(_ => ML)(number_of_threads)
  private val exeIdleTimeMin: Array[Long] = Array.fromFunction(_ => ML)(number_of_threads)
  private val exeTaskTimeMax = new Array[Long](number_of_threads)
  private val exeIdleTimeMax = new Array[Long](number_of_threads)
  private val exeTaskTimeSum = new Array[Long](number_of_threads)
  private val exeIdleTimeSum = new Array[Long](number_of_threads)
  private val exeTaskCounter = new Array[Int](number_of_threads)
  private val exeTimeStamp: Array[Long] = Array.fromFunction(_ => System.currentTimeMillis)(number_of_threads)
  private var exeTaskTimeAbsMin = ML
  private var exeTaskTimeAbsMax = OL
  private var exeTaskTime: Array[List[Long]] = Array.fromFunction(_ => List[Long]())(number_of_threads)

  private val threadQueue = new Array[Int](number_of_threads)
  private var exeSnapShot: List[Int] = List(0)

  // executor notifies DeliteStats of number of parallel tasks
  // it can have running at discrete times, so we can count
  // avergae number of parallel tasks being executed
  private var exeParallelTasksMax = 0
  private var exeParallelTasksSum = 0
  private var exeParallelTasksEventCounter = 0

  // OP statistics
  private var opExecuted = 0
  private var opCost = 0
  private var opTaskSum = 0
  private var opTaskMax = 0
  private var opTaskMin = Integer.MAX_VALUE
  private var opTaskNum: List[Long] = List()
  private var opParallelism: List[Long] = List()

  // sampling
  private var sample = 0
  private var readyOPs = ""
  private var cacheOPs = ""
  private var opops = 0

  def sampling() {
    sample += 1
    // TODO: stats are currently meaningless for trace generator    
    val (pops, rops, cops) = (0,0,0)//OPsExecutor.getStats()
    if (opops != pops) {
      opops = pops
      writer.write(pops + "\n")
      readyOPs += rops + "\n"
      cacheOPs += cops + "\n"
    }
//    exeSnapShot ::= pops
//    optSnapShot ::= rops
    if (SAMPLING_STATISTICS) Thread.sleep(1)
  }

  // record time it took thread id to complete a task
  def recTaskTime() {
    if (researchMode) {
      val id = 0 // ThreadID.get
      val period = System.currentTimeMillis - exeTimeStamp(id)
      if (period < exeTaskTimeMin(id)) exeTaskTimeMin(id) = period
      if (period > exeTaskTimeMax(id)) exeTaskTimeMax(id) = period
      exeTaskTime(id) ::= period
      exeTaskTimeSum(id) += period
      exeTaskCounter(id) += 1
      exeTimeStamp(id) = System.currentTimeMillis
    }
  }

  // record time thread id has been idle
  def recIdleTime() {
    if (researchMode) {
      val id = 0 //ThreadID.get
      val period = System.currentTimeMillis - exeTimeStamp(id)
      if (period < exeIdleTimeMin(id)) exeIdleTimeMin(id) = period
      if (period > exeIdleTimeMax(id)) exeIdleTimeMax(id) = period
      exeIdleTimeSum(id) += period
      exeTimeStamp(id) = System.currentTimeMillis
    }
  }

  def recParallelTasks(num: Int) {
    if (researchMode) {
      exeParallelTasksSum += num
      exeParallelTasksEventCounter += 1
      if (num > exeParallelTasksMax) exeParallelTasksMax = num
    }
  }

  def recTaskPerOP(num: Int) {
    if (researchMode) {
      opExecuted += 1
      opTaskSum += num
      opTaskNum ::= num
      if (num > opTaskMax) opTaskMax = num
      if (num < opTaskMin) opTaskMin = num
    }
  }

  def recLevels(lst: List[Long]) {
    opParallelism = lst
    writer.write("# " + opParallelism.length + " :")
    opParallelism.foreach(it => writer.write(" " + it))
    writer.write("\n")
  }

  def displayQueue(ix: Int, len: Int) {
    val x: Array[Double] = Array()
    val y = new Array[Double](4)
    Animation.setPenColor(Animation.RED)
    Animation.setPenRadius(0.005)
    Animation.line(-100, -80 + 2*ix, -100 + 2*len, -80 + 2*ix)
  }

  def displayThreadQueue(index: Int) {
    val change = 0 //random.nextInt(6) - 3
    val newlen = threadQueue(index) + change
    threadQueue(index) = if (newlen < 0) 0 else newlen
    displayQueue(index, threadQueue(index))
  }

  def displayThreadQueues() {
    Animation.setPenColor(Animation.YELLOW)
    Animation.setPenRadius()
    Animation.text(-80, -80 + 2 * number_of_threads+ 4, "Thread Queues")
    for (ix <- 1 until number_of_threads) displayThreadQueue(ix)
  }

  def displayGrid(x: Double, y: Double, w: Double, h: Double, step: Double) {
    var vline = x
    var hline = y
    Animation.setPenRadius()
    Animation.setPenColor(Animation.DARK_GRAY)
    while (vline <= x + w) {
      Animation.line(vline, y, vline, y + h)
      vline += step
    }
    while (hline <= y + h) {
      Animation.line(x, hline, x + w, hline)
      hline += step
    }
  }

  def displayGraph(x: Double, y: Double, data: List[Int]) {
    val side = (0.8 * frameSide).asInstanceOf[Int]
    val step = 5
    displayGrid(x, y, side, side, step)
    Animation.setPenColor(Animation.GREEN)
    var px = x + side
    var py = y + data.head
    var vt = data.tail
    while (px > x && !vt.isEmpty) {
      val nx = px - step
      val ny = y + vt.head
      Animation.line(nx, ny, px, py)
      px = nx
      py = ny
      vt = vt.tail
    }
  }

  def displayExecutor() {
    Animation.setPenColor(Animation.YELLOW)
    Animation.setPenRadius()
    Animation.text(30, 0, "Parallel Tasks")
    displayGraph(10,10, exeSnapShot)
  }

  def displayBuffer() {
    Animation.setPenColor(Animation.YELLOW)
    Animation.setPenRadius()
    Animation.text(30, -100, "Ready OPs")
    displayGraph(10, -90, optSnapShot)
  }

  private def decorations() {
    val title = "DELITE STATISTICS"
    Animation.setPenColor(Animation.GREEN)
    val x1: Int = (1.3 * title.length - frameSide).asInstanceOf[Int]
    val y1: Int = (1.05 * frameSide).asInstanceOf[Int]
    Animation.text(x1, y1, title)
    val hint = "ESC to exit"
    val x2: Int = hint.length - frameSide
    val y2: Int = (-1.06 * frameSide).asInstanceOf[Int]
    Animation.setPenColor(Animation.WHITE)
    Animation.text(x2, y2, hint)
  }

  // visualization
  def display() {
    Animation.clear(Animation.BLACK)
    displayThreadQueues()
    displayExecutor()
    displayBuffer()
  }

  /*
  def dumpProxyDAGfile[T <: Forcible[T] with DeliteDSLType[T]](root: DeliteProxy[T]) {
    ETLOGGER info "subgraph cluster" + System.nanoTime + " {"
    ETLOGGER info "fontname=\"Lucida Sans Typewriter\";\nfontsize=10;"
    Tracer.dag2dot(root)
    ETLOGGER info "label = \"" + root + "\"\n}"
  }
  */
  def dumpHistogramData(data: List[Long], min: Long, max: Long, bars: Int) {
    val range: Int = (max - min + 1).asInstanceOf[Int]
    val (tmp, nbuckets) = if (range > bars && bars > 0)
                             (range / bars, bars) else (1, range)
    val interval = if (tmp > 0) tmp else 1
    val histogram = new Array[Long](nbuckets)
    for (it <- data) {
      val index: Int = if (it >= max) nbuckets - 1
                       else (it - min).asInstanceOf[Int] / interval
      try {histogram(index) += 1}
      catch { case ex => println("index " + index + " nbuckets " + nbuckets +
                                 " interval " + interval + " item " + it + " min " + min) }
    }
    var key = min
    for (it <- histogram) {
      writer.write(key.toString)
      key += interval
      if (range > bars) {
        if (key + interval / 2 > max) key = max + 1
        writer.write("-" + (key-1))
      }
      writer.write(" " + it + "\n")
    }
  }

  private def gnuplotInit() {
    writer.write("# optimizations :")
    /*
    if (!config.memoization &&
        !config.optimizeWithDSLOpt &&
        !config.rebalanceProxyTree)
      writer.write(" none")
    else {
      if (config.memoization) writer.write(" memoization")
      if (config.optimizeWithDSLOpt) writer.write(" dslopt")
      if (config.rebalanceProxyTree) writer.write(" rebalance")
    }
    */
    writer.write("\n")
    writer.write("set terminal png small size 800,800\n")
    writer.write("set output 'delite-stats.png'\n")
    writer.write("set size 1,1\nset origin 0,0\n")
    writer.write("set multiplot\n")
    writer.write("set style fill solid noborder\n")
    writer.write("set style data histograms\n")
    writer.write("set style histogram rowstacked\n")

    writer.write("set size 1,0.5\nset origin 0,0.5\n")
    writer.write("set title \"OPs pending and ready over time\"\n")
    writer.write("set xlabel \"time\"\nset ylabel \"OPs\"\n")//unset xtics\n")
    writer.write("p '-' ti 'OPs pending', '-' ti 'OPs ready', '-' ti 'OPs cached'\n")

  }

  def dumpText() {
    active = false
    var toIdle: Long = 0
    var toTask: Long = 0
    var numTask = 0
    var threads = 0
    var times: List[Long] = List()
    for (ix <- 1 until number_of_threads) {
      val num = exeTaskCounter(ix)
      if (num > 0) {
        val it = exeIdleTimeSum(ix)
        val tt = exeTaskTimeSum(ix)
        numTask += num
        threads += 1
        toIdle += it
        toTask += tt
        println(ix + " av idle time " + (it / num) + " (" + exeIdleTimeMin(ix) + " to " + exeIdleTimeMax(ix)
                + ") av task time " + (tt / num) + " (" + exeTaskTimeMin(ix) + " to " + exeTaskTimeMax(ix)
                + ") for " + num + " tasks")
      }
      if (exeTaskTimeMin(ix) < exeTaskTimeAbsMin) exeTaskTimeAbsMin = exeTaskTimeMin(ix)
      if (exeTaskTimeMax(ix) > exeTaskTimeAbsMax) exeTaskTimeAbsMax = exeTaskTimeMax(ix)
      times = exeTaskTime(ix) ::: times
    }
    if (threads > 0) {
      println("total tasks " + numTask)
      println("total idle time " + toIdle + " (" + (toIdle / (1000.0 * number_of_threads)) + "s)")
      println("total task time " + toTask + " (" + (toTask / (1000.0 * number_of_threads)) + "s)")
      writer.write("e\n" + readyOPs + "e\n")
      writer.write(cacheOPs  + "e\n")
      writer.write("set style histogram clustered\n")
      writer.write("set size 0.5,0.5\nset origin 0,0\n")
      writer.write("set title \"Tasks longevity\"\n")
      //writer.write("set key invert reverse Left outside\n")
      writer.write("set xlabel \"milliseconds\"\nset ylabel \"tasks\"\nset xtics rotate by -45\n")
      writer.write("p '-' using 2:xticlabels(1) notitle\n")
      dumpHistogramData(times, exeTaskTimeAbsMin, exeTaskTimeAbsMax, 4)
      writer.write("e\n")
      if (opExecuted > 0) {
        writer.write("set size 0.5,0.5\nset origin 0.5,0\n")
        writer.write("set title \"OPs to tasks distribution\"\n")
        //writer.write("set key invert reverse Left outside\n")
        writer.write("set xlabel \"tasks\"\nset ylabel \"OPs\"\nset xtics norotate\n")
        writer.write("p '-' using 2:xticlabels(1) notitle\n")
        dumpHistogramData(opTaskNum, opTaskMin, opTaskMax, opTaskMax)
        writer.write("e\n")
        writer.write("unset multiplot\nreset\n")
        println("max task/op " + opTaskMax + " ave task/op " + (opTaskSum / opExecuted))
      }
    }
    println("Stats samples " + sample)
  }

} // object DeliteStats
