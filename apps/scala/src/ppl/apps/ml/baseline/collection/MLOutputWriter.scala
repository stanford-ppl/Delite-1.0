package ppl.apps.ml.baseline.collection

import java.io.{FileWriter, BufferedWriter}

// file format is m lines with n floats per line, each float separated by whitespaces
// (same as matlab .dat)
object MLOutputWriter{

  /*
  def writeMatrix[T](m: Matrix[T], filename: String)(implicit conv: T => Double) : Boolean = {
    val xfs = new BufferedWriter(new FileWriter(filename))

    m.foreachRow( vec =>
      {
        vec.foreach( e =>
          {
            xfs.write(java.lang.Double.toString(conv(e)))
            xfs.write("  ")
          })
        xfs.write("\n")
      }
    )
    xfs.close()

    return true
  }
    */
   def writeDoubleVector[T](v: DoubleVector, filename: String): Boolean = {
     val xfs = new BufferedWriter(new FileWriter(filename))

     var i = 0
     while( i < v.length) {
       xfs.write(java.lang.Double.toString(v(i)) + "\n")
       i += 1
     }
     xfs.close()

     return true
   }
}