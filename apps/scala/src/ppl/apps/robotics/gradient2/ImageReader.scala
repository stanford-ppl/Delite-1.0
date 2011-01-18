package ppl.apps.robotics.gradient2

import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.appinclude._
import java.io.{BufferedReader, FileReader}

object ImageReader {
  def readGrayscaleImage(filename: String): GrayscaleImage = {
    val xfs = new BufferedReader(new FileReader(filename))
    var line = xfs.readLine()
    line = line.trim()
    var ints = line.split("\\s+")
    val x = Matrix[Int](0, ints.length)

    while (line != null) {
      val v = Vector[Int](ints.length)
      var i = 0
      while (i < ints.length) {
        v(i) = Integer.parseInt(ints(i))
        i += 1
      }
      x += v

      line = xfs.readLine()
      if (line != null) {
        line = line.trim()
        ints = line.split("\\s+")
      }
    }
    xfs.close()

    new GrayscaleImage(x)
  }
}