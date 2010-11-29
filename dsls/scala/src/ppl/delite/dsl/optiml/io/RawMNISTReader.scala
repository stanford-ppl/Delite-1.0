package ppl.delite.dsl.optiml.io

import ppl.delite.dsl.optiml.{Vector, Matrix}
import java.io.{FileInputStream, DataInputStream}

// see http://yann.lecun.com/exdb/mnist/ for data format description
object RawMNISTReader {
  def readLabels(filename: String): Vector[Int] = {
    val x = Vector[Int](0)

    val in = new DataInputStream(new FileInputStream(filename))

    val magic = in.readInt()    // 0000 32 bit integer 0x00000801(2049) magic number(MSB first)
    val numItems = in.readInt() // 0004 32 bit integer 60000 number of items
                 
    for (i <- 0 until numItems) {
      x += in.readUnsignedByte()
    }
    in.close()

    x.pprint
    x
  }

  def readImages(filename: String): Vector[Matrix[Int]] = {
    val images = Vector[Matrix[Int]](0)

    val in = new DataInputStream(new FileInputStream(filename))

    val magic = in.readInt()     // 0000 32 bit integer  0x00000803(2051) magic number
    val numImages = in.readInt() // 0004 32 bit integer 60000            number of images
    val numRows = in.readInt()   // 0008 32 bit integer  28               number of rows
    val numCols = in.readInt()   // 0012 32 bit integer  28               number of columns

    for (i <- 0 until numImages) {
      val image = Matrix[Int](0, 0)
      for (row <- 0 until numRows) {
        val v = Vector[Int](numCols)
        for (col <- 0 until numCols) {
          v(i) = in.readUnsignedByte()
        }
        image += v
      }
      images += image
    }
    in.close()

    images
  }
}