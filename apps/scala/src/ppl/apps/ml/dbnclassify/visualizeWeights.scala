package ppl.apps.ml.dbnclassify

import ppl.delite.core.appinclude._
//import ppl.apps.ml.io.MLInputReader
import ppl.delite.dsl.optiml.io.MLInputReader
import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.Precursors._
import ppl.delite.metrics._
import ppl.delite.dsl.primitive.DeliteDouble
import ppl.delite.core.{Delite, DeliteApplication}
import java.io.{BufferedWriter, File, PrintWriter, FileWriter, BufferedReader, FileReader}
import java.awt.image.BufferedImage
import java.awt.Image
import javax.imageio.ImageIO

/**
 * Created by IntelliJ IDEA.
 * User: joe
 * Date: Aug 10, 2010
 * Time: 3:53:10 PM
 * To change this template use File | Settings | File Templates.
 */

object visualizeWeights extends DeliteApplication {

  val saveDir = "rfDBNWeights/weightVis"
  val weightPath = "rfDBNWeights/vis_hidW"

  def run(args: Array[String]) = {

    Delite.init = true
    print("loading data............... ")

    //initialize everything properly - must set this!
    //and the images don't really make sense unless the input to the weights was actually a imgX*imgY image
    //i.e. the weights are vis_hidW
    val weights = MLInputReader.read(weightPath)
    val imgX = 28
    val imgY = 28
    
    println("done")
    println("weights rows and cols: " + weights.numRows + "  " + weights.numCols)
    Delite.init = false

    for(w <- 0 until weights.numCols){
      val im = new BufferedImage(imgY, imgX, BufferedImage.TYPE_BYTE_GRAY)
      var barray : Array[Byte] = new Array[Byte](imgX*imgY)

      for(x <- 0 until imgX){
        for(y <- 0 until imgY){
          val intens:Byte = (calcIntens(weights((x*imgY + y), w)) * 2).asInstanceOf[Byte]

          barray(x*imgY + y) = intens
//          println(intens.asInstanceOf[Short])

        }
      }

      var wr = im.getRaster()
      wr.setDataElements(0,0, imgY, imgX, barray)

      try {
        var file = new File(saveDir + "/" + w)
        if(file.exists()) {
          println("this file exists! ..deleting file")
          file.delete()
        }

        ImageIO.write(im, "PNG", file)
      }
      catch {
        case e: Exception => println("Unable to save weight " + w + "!")
      }

    }

  }

  def calcIntens(value : Double) : Byte = {
    if(value < -1) return 0
    else if(value > 1) return 127
    else{
      return ((value + 1)*64).toByte
    }
  }
}