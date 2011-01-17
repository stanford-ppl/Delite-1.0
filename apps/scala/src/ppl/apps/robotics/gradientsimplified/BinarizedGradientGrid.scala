package ppl.apps.robotics.gradientsimplified

import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.dsl.optiml.Precursors._
import ppl.delite.metrics._
import ppl.delite.core.appinclude._

class BinarizedGradientGrid(modelFilenames: Vector[String]) {

  // The radius of the template
  val template_radius_ = 15

  // Threshold for accepting a template match (1 is perfect)
  val accept_threshold_ = 0.82f

  // Ignore gradients with lower magnitude
  val magnitude_threshold_ = 200

  // Fraction overlap between two detections above which one of them is suppressed
  val fraction_overlap_ = 0.6f

  val all_templates = Vector[Vector[BinarizedGradientTemplate]]()
  val names = Vector[String]()

  readModels()

  // Loads pre-trained models
  def readModels() = {
    for(i <- 0 until modelFilenames.length) {
      println("Loading model: " + modelFilenames(i))
      val (name, templates) = ModelReader.loadModels(modelFilenames(i))
      all_templates += templates
      names += name
    }
  }

  // Runs the object detection of the current image.
  def detectAllObjects(image: GrayscaleImage) = {
    val img_gray = image // assuming image is single-channel. Needs to be made such if not.

    val (mag, phase) = computeGradients(img_gray)
    val binGrad = binarizeGradients(mag, phase)
    val cleanGrad = gradMorphology(binGrad)

    val pyr = new BinarizedGradientPyramid(cleanGrad)

    val all_detections = Vector[BiGGDetection]()
    var index = 0
    all_templates.foreach {
      root_templates =>
        println(names(index) + ": Using " + root_templates.length + " templates")
        val detections = detectSingleObject(pyr.getIndex(pyr.fixedLevelIndex), root_templates, template_radius_, pyr.fixedLevelIndex, accept_threshold_)
        detections.force // Force the result for timing purposes
        println(names(index) + ": Detections: " + detections.length)

        detections.foreach {
          detection =>
            detection.name = names(index)
        }
        all_detections ++= detections
        index += 1
    }
    all_detections.force // Force the result for timing purposes
    val filteredDetections = nonMaxSuppress(all_detections, fraction_overlap_)
    filteredDetections.force // Force the result for timing purposes
    println("Total detections: " + filteredDetections.length)
  }

  //Run detection for this object class.
  def detectSingleObject(gradSummary: GrayscaleImage, templates: Vector[BinarizedGradientTemplate], template_radius: Int, level: Int, accept_threshold: Float): Vector[BiGGDetection] = {
    aggregate(5, gradSummary.rows - 5, 5, gradSummary.cols - 5) {
      (x, y) => searchTemplates(gradSummary, x, y, template_radius, level, accept_threshold, templates)
    }
  }

  def searchTemplates(gradSummary: GrayscaleImage, x: Int, y: Int, template_radius: Int, level: Int, accept_threshold: Float, templates: Vector[BinarizedGradientTemplate]): Vector[BiGGDetection] = {
    val reduction_factor = (1 << level)
    val crt_template = fillTemplateFromGradientImage(gradSummary, x, y, template_radius, level)
    aggregate(0, templates.length) { j =>
      val res = templates(j).score(crt_template, accept_threshold)
      if (res > accept_threshold) {
        val detection = new BiGGDetection()
        val bbox = templates(j).rect

        detection.roi = new Rect((reduction_factor * x - bbox.width / 2).asInstanceOf[Int], (reduction_factor * y - bbox.height / 2).asInstanceOf[Int], bbox.width, bbox.height)
        detection.score = res
        detection.index = j
        detection.x = x
        detection.y = y
        detection.tpl = templates(j)
        detection.crt_tpl = crt_template

        Vector[BiGGDetection](detection)
      }
      else {
        Vector[BiGGDetection]()
      }
    }
  }

  // Construct a template from a region of a gradient summary image.
  def fillTemplateFromGradientImage(gradSummary: GrayscaleImage, xc: Int, yc: Int, r: Int, level: Int): BinarizedGradientTemplate = {
    val span = 2 * r
    val tpl = new BinarizedGradientTemplate()
    tpl.radius = r
    tpl.level = level
    tpl.binary_gradients = Vector[Int](span * span) //Create the template
    tpl.match_list = Vector[Int]()

    //Bear with me, we have to worry a bit about stepping off the image boundaries:
    val rows = gradSummary.rows
    val cols = gradSummary.cols
    //y
    var ystart = yc - r
    var yoffset = 0 //offset before you reach the playing field
    if (ystart < 0) {
      yoffset = -ystart
      ystart = 0
    }
    var yend = yc + r
    if (yend > rows) {
      yend = rows
    }

    //x
    var xstart = xc - r
    var xoffset = 0 //offset before you reach the playing field
    if (xstart < 0) {
      xoffset = -xstart
      xstart = 0
    }
    var xend = xc + r
    if (xend > cols) {
      xend = cols
    }

    tpl.hist = Vector[Float](8)
    var cnt = 0

    //Fill the binary _gradients
    for (y <- ystart until yend) {
      val imageRow = gradSummary.data(y)
      for (x <- xstart until xend) {
        var index = (yoffset + y - ystart) * span + (xoffset + x - xstart) //If this were an image patch, this is the offset to it
        tpl.binary_gradients(index) = imageRow(x)
        if (imageRow(x) > 0) {
          //Record where gradients are
          tpl.match_list += index
        }

        for (i <- 0 until 8) {
          if ((imageRow(x) & (1 << i)) > 0) {
            tpl.hist(i) += 1
            cnt += 1
          }
        }
      }
    }

    for (i <- 0 until 8) {
      tpl.hist(i) /= cnt;
    }
    tpl
  }

  // Compute magnitude and phase in degrees from single channel image
  def computeGradients(img: GrayscaleImage): (ImageF, ImageF) = {
    //Find X and Y gradients
    val (x, y) = img.scharr
    cartToPolar(x, y)
  }

  def cartToPolar(x: GrayscaleImage, y: GrayscaleImage): (ImageF, ImageF) = {
    val mag = new ImageF(x.data.zipWith(y.data, (a, b) => math.sqrt(a*a + b*b).asInstanceOf[Float]))
    val phase = new ImageF(x.data.zipWith(y.data, (a, b) => (math.atan2(b, a)*180/math.Pi).asInstanceOf[Float]).mmap(a => if (a < 0) a + 360 else a))
    (mag, phase)
  }

  //Turn mag and phase into a binary representation of 8 gradient directions.
  def binarizeGradients(mag: ImageF, phase: ImageF): GrayscaleImage = {
    new GrayscaleImage(mag.data.zipWith(phase.data, (a, b) => {
      if (a >= magnitude_threshold_) {
          var angle = b
          if (angle >= 180) {
            angle -= 180 //Ignore polarity of the angle
          }
          (1 << (angle / (180.0 / 8)).asInstanceOf[Int])
        }
      else 0
    }))
  }

  // Filter out noisy gradients via non-max suppression in a 3x3 area.
  def gradMorphology(binaryGradient: GrayscaleImage): GrayscaleImage = {
    val rows = binaryGradient.rows
    val cols = binaryGradient.cols
    //Zero the borders -- they are unreliable
    for (x <- 0 until cols) {
      binaryGradient.data(0, x) = 0
      binaryGradient.data(rows - 1, x) = 0
    }
    for (y <- 1 until rows - 1) {
      binaryGradient.data(y, 0) = 0
      binaryGradient.data(y, cols - 1) = 0
    }
    //Now do non-max suppression. NOTE. Each pixel location contains just one orientation at this point
    val cleanedGradient = new GrayscaleImage(rows, cols)
    val counts = Vector[Int](9) //9 places since we must also count zeros ... and ignore them.  That means that bit 1 falls into counts[1], 10 into counts[2] etc
    val index = Vector[Int](255) //This is just a table to translate 0001->1, 0010->2, 0100->3, 1000->4 and so on
    index(0) = 0 //Fill out this table Index to increments counts, counts how many times a 1 has been shifted
    index(1) = 1
    index(2) = 2
    index(4) = 3
    index(8) = 4
    index(16) = 5
    index(32) = 6
    index(64) = 7
    index(128) = 8
    val sft = Vector[Int](9) //OK, bear with me now. This table will translate from offset back to shift: 1->0001, 2->0010, 3->0100 and so on.
    var mask = 1
    for (i <- 1 until 9) {   // sft(n) = 2^(n-1)
      sft(i) = mask
      mask = mask << 1
    }
    for (y <- 0 until rows - 2) {
      for (i <- 0 until 9) //We sweep counts across and use it to determine the orientations present in a local neighborhood
        counts(i) = 0
      val b0 = binaryGradient.getRow(y) //We're just going to step across 9 positions at a time, peeling off the back pointers
      val b1 = binaryGradient.getRow(y + 1)
      val b2 = binaryGradient.getRow(y + 2)

      //init the back cols of the count ... remember that index translates the (single) bit position into a count of its shift from right + 1
      counts(index(b0(0))) += 1
      counts(index(b0(1))) += 1
      counts(index(b1(0))) += 1
      counts(index(b1(1))) += 1
      counts(index(b2(0))) += 1
      counts(index(b2(1))) += 1
      for (x <- 0 until cols - 2) {
        counts(index(b0(x + 2))) += 1 //add the leading edge of counts
        counts(index(b1(x + 2))) += 1
        counts(index(b2(x + 2))) += 1
        // if(1)//*(b1+1)) //Don't fill in where we found no gradients
        // {

        //find the max in count
        var maxindx = 1 //Find the maximum count of real orientation (skip bin zero which means no orientation)
        var maxcnt = counts(1)
        for (j <- 2 until 9) {
          if (counts(j) > maxcnt) {
            maxindx = j
            maxcnt = counts(j)
          }
        }
        //see if we have a valid maximum
        if (maxcnt > 1) { //Only record the gradient if it's not a singleton (shot noise)
          cleanedGradient.data(y + 1, x + 1) = sft(maxindx) // the center pixel of the "clean" image
        }
        // }
        //Peel off the back pointers
        counts(index(b0(x))) -= 1
        counts(index(b1(x))) -= 1
        counts(index(b2(x))) -= 1
      }
    }
    cleanedGradient
  }

  // Determines if two rectangles intersect (true = intersects)
  def intersect(a: Rect, b: Rect): Boolean = {
    ((a.x < (b.x + b.width)) && ((a.x + a.width) > b.x) && ((a.y + a.height) > b.y) && (a.y < (b.y + b.height)))
  }

  // Computes the fraction of the intersection of two rectangles with respect to the total area of the rectangles.
  def rectFractOverlap(a: Rect, b: Rect): Float = {
    if (intersect(a, b)) {
      val total_area: Float = b.height * b.width + a.width * a.height
      val left = if (a.x > b.x) a.x else b.x
      val top = if (a.y > b.y) a.y else b.y
      val right = if (a.x + a.width < b.x + b.width) a.x + a.width else b.x + b.width
      val width = right - left
      val bottom = if (a.y + a.height < b.y + b.height) a.y + a.height else b.y + b.height
      val height = bottom - top
      2.0f * height * width / (total_area + 0.000001f) //Return the fraction of intersection
    } else {
      0.0f
    }
  }

  /**
   * Suppress overlapping rectangle to be the rectangle with the highest score
   * detections: vector of detections to work with
   * frac_overlap: what fraction of overlap between 2 rectangles constitutes overlap
   */
  def nonMaxSuppress(detections: Vector[BiGGDetection], frac_overlap: Float): Vector[BiGGDetection] = {
    var len = detections.length

    var i = 0
    while (i < len - 1) {
      var j = i + 1
      var iMoved = false
      while (j < len && iMoved == false) {
        val measured_frac_overlap = rectFractOverlap(detections(i).roi, detections(j).roi)
        if (measured_frac_overlap > frac_overlap) {
          if (detections(i).score >= detections(j).score) {
            val temp = detections(len - 1)
            detections(len - 1) = detections(j)
            detections(j) = temp
            len -= 1
            j -= 1
          }
          else {
            val temp = detections(len - 1)
            detections(len - 1) = detections(i)
            detections(i) = temp
            len -= 1
            i -= 1
            iMoved = true
          }
        }
        j += 1
      }
      i += 1
    }
    detections.take(len)
  }
}