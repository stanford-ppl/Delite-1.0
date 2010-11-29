package ppl.delite.dsl.optiml.io

import java.io.{BufferedWriter, FileWriter}
import ppl.delite.dsl.optiml.{Vector, Matrix}

// file format is m lines with n floats per line, each float separated by whitespaces
// (same as matlab .dat)
object MLOutputWriter{

  def write[T](m: Matrix[T], filename: String)(implicit conv: T => Double) : Boolean = {
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

   def writeVector[T](v: Vector[T], filename: String)(implicit conv: T => Double) : Boolean = {
     val xfs = new BufferedWriter(new FileWriter(filename))

     v.foreach( e =>
       {
         xfs.write(java.lang.Double.toString(conv(e)) + "\n")
       }
     )
     xfs.close()

     return true
   }
}