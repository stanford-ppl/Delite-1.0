package ppl.apps.robotics.gradient

import java.io.{BufferedReader, FileReader}
import collection.mutable.ArrayBuffer
import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.appinclude._


object ModelReader {
  def loadModels(filename: String): (String, Vector[BinarizedGradientTemplate]) = {
    val templates = Vector[BinarizedGradientTemplate]()

    val file = new BufferedReader(new FileReader(filename))

    if (file.readLine() != "bigg_object:") throw new RuntimeException("Illegal data format")
    file.readLine() //"============"
    val params = file.readLine().trim.split(" ")
    if (params(0) != "obj_name/obj_num/num_objs:") throw new RuntimeException("Illegal data format")
    val objName = params(1)
    val objId = params(2)
    val numObjs = Integer.parseInt(params(3))
    for (i <- 0 until numObjs) {
      templates += loadModel(file)
    }
    (objName, templates)
  }

  private def loadModel(file: BufferedReader): BinarizedGradientTemplate = {
    val template = new BinarizedGradientTemplate()

    if (file.readLine().trim != "====OneBiGG====:") throw new RuntimeException("Illegal data format")
    var temp = file.readLine().trim.split(" ")
    if (temp(0) != "view/radius/reduction:") throw new RuntimeException("Illegal data format")
    val view = Integer.parseInt(temp(1))
    val radius = Integer.parseInt(temp(2))
    val reductionFactor = Integer.parseInt(temp(3))

    temp = file.readLine().trim.split(" ")
    if (temp(0) != "Gradients:") throw new RuntimeException("Illegal data format")
    val gradientsSize = Integer.parseInt(temp(1))
    val gradients = Array.fill(gradientsSize)(0)
    val gradientsString = file.readLine().trim.split(" ")
    for (i <- 0 until gradientsSize) {
      gradients(i) = Integer.parseInt(gradientsString(i))
    }

    temp = file.readLine().trim.split(" ")
    if (temp(0) != "Match_list:") throw new RuntimeException("Illegal data format")
    val matchListSize = Integer.parseInt(temp(1))
    val matchList = ArrayBuffer[Int]()
    matchList.sizeHint(matchListSize)
    val matchListString = file.readLine().trim.split(" ")
    for (i <- 0 until matchListSize) {
      matchList += Integer.parseInt(matchListString(i))
    }

    temp = file.readLine().trim.split(" ")
    if (temp(0) != "Occlusions:") throw new RuntimeException("Illegal data format")
    val occlusionsSize = Integer.parseInt(temp(1))
    val occlusions = ArrayBuffer[ArrayBuffer[Int]]()
    val occlusionsString = file.readLine().trim.split(" ")
    if (occlusionsSize != 0) throw new RuntimeException("Occlusions not supported.")

    if (file.readLine().trim != "BoundingBox:") throw new RuntimeException("Illegal data format")
    val bbString = file.readLine().trim.split(" ")
    val x = Integer.parseInt(bbString(0))
    val y = Integer.parseInt(bbString(1))
    val width = Integer.parseInt(bbString(2))
    val height = Integer.parseInt(bbString(3))
    val bb = new Rect(x, y, width, height)

    template.radius = radius
    template.binary_gradients = gradients
    template.match_list = matchList
    template.occlusions = occlusions
    template.rect = bb

    template
  }
}