package ppl.apps.robotics.gradient2

import java.io.{BufferedReader, FileReader}
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
    var i = 0
    while (i < numObjs) {
      templates += loadModel(file)
      i += 1
    }
    (objName, templates)
  }

  private def loadModel(file: BufferedReader): BinarizedGradientTemplate = {
    if (file.readLine().trim != "====OneBiGG====:") throw new RuntimeException("Illegal data format")
    var temp = file.readLine().trim.split(" ")
    if (temp(0) != "view/radius/reduction:") throw new RuntimeException("Illegal data format")
    val view = Integer.parseInt(temp(1))
    val radius = Integer.parseInt(temp(2))
    val reductionFactor = Integer.parseInt(temp(3))

    temp = file.readLine().trim.split(" ")
    if (temp(0) != "Gradients:") throw new RuntimeException("Illegal data format")
    val gradientsSize = Integer.parseInt(temp(1))
    val gradients = Vector[Int](gradientsSize)
    val gradientsString = file.readLine().trim.split(" ")
    var i = 0
    while (i < gradientsSize) {
      gradients(i) = Integer.parseInt(gradientsString(i))
      i += 1
    }

    temp = file.readLine().trim.split(" ")
    if (temp(0) != "Match_list:") throw new RuntimeException("Illegal data format")
    val matchListSize = Integer.parseInt(temp(1))
    val matchList = Vector[Int]()
    val matchListString = file.readLine().trim.split(" ")
    i = 0
    while (i < matchListSize) {
      matchList += Integer.parseInt(matchListString(i))
      i += 1
    }

    temp = file.readLine().trim.split(" ")
    if (temp(0) != "Occlusions:") throw new RuntimeException("Illegal data format")
    val occlusionsSize = Integer.parseInt(temp(1))
    val occlusions = Vector[Vector[Int]]()
    val occlusionsString = file.readLine().trim.split(" ")
    if (occlusionsSize != 0) throw new RuntimeException("Occlusions not supported.")

    if (file.readLine().trim != "BoundingBox:") throw new RuntimeException("Illegal data format")
    val bbString = file.readLine().trim.split(" ")
    val x = Integer.parseInt(bbString(0))
    val y = Integer.parseInt(bbString(1))
    val width = Integer.parseInt(bbString(2))
    val height = Integer.parseInt(bbString(3))
    val bb = new Rect(x, y, width, height)

    new BinarizedGradientTemplate(radius, bb, null, 0, gradients, matchList, occlusions, null, null)
  }
}