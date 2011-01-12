package ppl.apps.robotics.gradient

import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.dsl.optiml.Precursors._
import ppl.delite.metrics._
import ppl.delite.core.appinclude._


/**
 * Created by IntelliJ IDEA.
 * User: Anand Atreya
 * Date: Oct 17, 2010
 * Time: 10:32:10 PM
 * To change this template use File | Settings | File Templates.
 */

class BinarizedGradientGrid(modelFilenames: Vector[String]) {
  val match_table = Matrix[Int](256, 256)

  //A somewhat inelegant way to create a unsigned char 256x256 look up table for cosine matching
  // In the binary vector, 4 positions away either direction are orthogonal (cos = 0)
  // Each adjacent position, whether right or left is a 22.5 degree mismatch since I don't consider
  // contrast (gradients go only from 0 to 180 degrees) and I binarize into bytes (180/8 = 22.5)
  // This table will be used if match_type = 1, else we'll use anding (and just disregard this table)
  (0 :: 256).map { d1 =>
    (0 :: 256).map { d2 =>
      if ((d1 == 0) || (d2 == 0)) {
        match_table(d1, d2) = 0
      }
      else {
        if ((d1 & d1) > 0) {
          match_table(d1, d2) = 100; //100 => 1.0 but in binary so in the end, divide by 100
        }
        else {
          if (((_rotl(d1, 1) & d2) > 0) || ((_rotr(d1, 1) & d2) > 0)) {
            match_table(d1, d2) = 92; //100xCos match of 22.5 degree difference
          }
          else {
            if (((_rotl(d1, 2) & d2) > 0) || ((_rotr(d1, 2) & d2) > 0)) {
              match_table(d1, d2) = 71; //100xCos match of 2*22.5 degree difference
            }
            else {
              if (((_rotl(d1, 3) & d2) > 0) || ((_rotr(d1, 3) & d2) > 0)) {
                match_table(d1, d2) = 38; //100xCos match of 3*22.5 degree difference
              }
              else {
                match_table(d1, d2) = 0; //100xCos match of 90 degree difference
              }
            }
          }
        }
      }
    }
  }
  match_table(0, 0) = 100 //Call matching 0 against 0 a perfect match (though indices are not taken there
  // match_table NOT DEBUGGED - never used.


  // Helper circular shift functions for unsigned 8-bit ints
  def _rotl(value: Int, shift: Int) = {
    val shift2 = shift & 7
    ((value << shift2) | (value >> (8 - shift2))) % 256
  }

  def _rotr(value: Int, shift: Int) = {
    val shift2 = shift & 7
    ((value >> shift2) | (value << (8 - shift2))) % 256
  }

  val all_templates = Vector[Vector[BinarizedGradientTemplate]]()
  val names = Vector[String]()

  readModels()


  /**
     * Loads pre-trained models
     */
  def readModels() = {
//    println("Loading models")
    for(i <- 0 until modelFilenames.length) {
      println("Loading model: " + modelFilenames(i))
      val (name, templates) = ModelReader.loadModels(modelFilenames(i))
      all_templates += templates
      names += name
    }
  }

  /**
   * Construct a template from a region of a gradient summary image.
   * @param tpl The BinarizedGradientTemplate structure.
   * @param gradSummary The single channel uchar binary gradient summary image
   * @param xc The center of a template in the gradSummary (in reduced scale), x coordinate
   * @param yc The center of a template in the gradSummary (in reduced scale), ycoordinate
   * @param r The radius of the template (in reduced scale)
   * @param reduct_factor
   */
  def fillTemplateFromGradientImage(gradSummary: Image, xc: Int, yc: Int, r: Int, level: Int): BinarizedGradientTemplate = {
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

  def fillTemplateFromGradientImage(gradSummary: Image, roi: Rect, mask: Image, img_mask: Image, r: Int, level: Int): BinarizedGradientTemplate = {
    val span = 2 * r
    val tpl = new BinarizedGradientTemplate()
    tpl.radius = r
    tpl.level = level
    tpl.binary_gradients = Vector[Int](span * span) //Create the template
    tpl.match_list = Vector[Int]()

    tpl.rect = new Rect(0, 0, roi.width, roi.height)

    // set mask
    tpl.mask_list = Vector[Int](mask.rows * mask.cols)
    var idx = 0
    for (y <- 0 until mask.rows) {
      for (x <- 0 until mask.cols) {
        if (mask.data(y, x) > 0) {
          tpl.mask_list(idx) = y * mask.cols + x
          idx += 1
        }
      }
    }

    val reduction_factor = 1 << level

    val xc = (roi.x + roi.width / 2) / reduction_factor
    val yc = (roi.y + roi.height / 2) / reduction_factor
    var xr = roi.width / 2 / reduction_factor
    var yr = roi.height / 2 / reduction_factor

    if (xr > r) {
      xr = r
    }
    if (yr > r) {
      yr = r
    }

    val rows = gradSummary.rows
    val cols = gradSummary.cols
    //y
    var ystart = yc - yr
    var yoffset = r - yr //offset before you reach the playing field
    if (ystart < 0) {
      yoffset = -ystart
      ystart = 0
    }
    var yend = yc + yr
    if (yend > rows) {
      yend = rows
    }

    //x
    var xstart = xc - xr
    var xoffset = r - xr //offset before you reach the playing field
    if (xstart < 0) {
      xoffset = -xstart
      xstart = 0
    }
    var xend = xc + xr
    if (xend > cols) {
      xend = cols
    }

    tpl.hist = Vector[Float](8)

    //Fill the binary _gradients
    for (y <- ystart until yend) {
      val imageRow = gradSummary.data(y)
      val maskRow = img_mask.data(y)
      for (x <- xstart until xend) {
        val index = (yoffset + y - ystart) * span + (xoffset + x - xstart) //If this were an image patch, this is the offset to it
        tpl.binary_gradients(index) = imageRow(x)
        if (imageRow(x) > 0) {
          //Record where gradients are
          tpl.match_list += index
        }
      }
    }

    tpl
  }

  def detect2(img: Image, level: Int, pyr: BinarizedGradientPyramid, locations: Vector[Point2i], templates: Vector[BinarizedGradientTemplate], detections: Vector[BiGGDetection], template_radius: Int, accept_threshold: Float, accept_threshold_decay: Float): Unit = {
//    println("Detect on level: " + level)
    val reduction_factor = (1 << level)
    val newRadius = template_radius // = template_radius / reduction_factor
    // NOTE: Likely due to lack of full-scale database

    PerformanceTimerAggregate.start("detect3")
    val crt_detections = detect3(pyr.getIndex(level), locations, templates, newRadius, level, accept_threshold)
    crt_detections.force  // Force the result for timing purposes
    PerformanceTimerAggregate.stop("detect3")

    //	printf("Before nms, detections: %d\n", crt_detections.size());
    println("Detections: " + crt_detections.length)

    if (level > pyr.start_level) {
      println("Transitioning to next level of pyramid")
      for (i <- 0 until crt_detections.length) {
        val crt_locations = Vector[Point2i]()

        val dir = Matrix[Int](Vector[Int](-1, -1, -1, 0, 0, 0, 1, 1, 1), Vector[Int](-1, 0, 1, -1, 0, 1, -1, 0, 1))

        val x = crt_detections(i).x
        val y = crt_detections(i).y
        for (k <- 0 until 9) {
          crt_locations += new Point2i(2 * x + dir(0)(k), 2 * y + dir(1)(k))
        }
        detect2(img, level - 1, pyr, crt_locations, templates(crt_detections(i).index).templates, detections, template_radius, accept_threshold * (1 - accept_threshold_decay), accept_threshold_decay)
      }
    }
    else {
      detections ++= crt_detections
    }
  }

  def dist(a: Vector[Float], b: Vector[Float]): Float = {
    var result: Float = 0
    for (i <- 0 until a.length) {
      result += (a(i) - b(i)) * (a(i) - b(i))
    }
    result
  }

  /**
   * Run detection for this object class.
   * @param gradientSummary
   * @param detections
   */
  def detect3(gradSummary: Image, locations: Vector[Point2i], templates: Vector[BinarizedGradientTemplate], template_radius: Int, level: Int, accept_threshold: Float): Vector[BiGGDetection] = {
    if (locations.length == 0) {
      aggregate(5, gradSummary.rows - 5, 5, gradSummary.cols - 5) {
        (x, y) => searchTemplates(gradSummary, x, y, template_radius, level, accept_threshold, templates)
      }
    }
    else {
      println("Using locations vector")
      locations.flatMap{loc => searchTemplates(gradSummary, loc.x, loc.y, template_radius, level, accept_threshold, templates)}
    }
  }

//  var printcount = 0

  def searchTemplates(gradSummary: Image, x: Int, y: Int, template_radius: Int, level: Int, accept_threshold: Float, templates: Vector[BinarizedGradientTemplate]): Vector[BiGGDetection] = {
    val reduction_factor = (1 << level)
//    PerformanceTimerAggregate.start("fill")
    val crt_template = fillTemplateFromGradientImage(gradSummary, x, y, template_radius, level)
//    PerformanceTimerAggregate.stop("fill")
    aggregate(0, templates.length) { j =>
      val res = templates(j).score(crt_template, accept_threshold, match_table, match_method_)
      if (res > accept_threshold) {
//          println("Level: " + level + ", score: " + res)
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

  var detectionMsg = Vector[BiGGDetection]()

  /**
   * Runs the object detection of the current image.
   */
  def detectMain(image: Image) = {
    // forget about previous detections
    detectionMsg = Vector[BiGGDetection]()

    val img_gray = image // assuming image is single-channel. Needs to be made such if not.

    PerformanceTimerAggregate.start("computeGradients")
    val (mag, phase) = computeGradients(img_gray)
    PerformanceTimerAggregate.stop("computeGradients")
    val binGrad = binarizeGradients(mag, phase)
    val cleanGrad = gradMorphology(binGrad)
//    cleanGrad.data.pprint

    PerformanceTimerAggregate.start("pyramid")
    val pyr = new BinarizedGradientPyramid(cleanGrad, start_level_, levels_) // build two levels starting with level 2
    PerformanceTimerAggregate.stop("pyramid")

    println("Doing detection")

    val all_detections = Vector[BiGGDetection]()
    var index = 0
    PerformanceTimerAggregate.start("detectLoop")
    all_templates.foreach {
      root_templates =>
        val detections = Vector[BiGGDetection]()
        val everywhere = Vector[Point2i]()
        println("Using " + root_templates.length + " templates")
        PerformanceTimerAggregate.start("detect2")
        detect2(image, pyr.start_level + pyr.levels - 1, pyr, everywhere, root_templates, detections, template_radius_, accept_threshold_, accept_threshold_decay_)
        detections.force // Force the result for timing purposes
        PerformanceTimerAggregate.stop("detect2")

        detections.foreach {
          detection =>
            detection.name = names(index)
        }
        all_detections ++= detections
        index += 1
    }
    all_detections.force // Force the result for timing purposes
    PerformanceTimerAggregate.stop("detectLoop")
    PerformanceTimerAggregate.start("nonMaxSuppress")
    val filteredDetections = helper.nonMaxSuppress(all_detections, fraction_overlap_)
    filteredDetections.force // Force the result for timing purposes
    PerformanceTimerAggregate.stop("nonMaxSuppress")
    println("Total detections: " + filteredDetections.length)

//    detectionMsg.sizeHint(all_detections.length)
//    for (i <- 0 until all_detections.length) {
//      val tpl = all_detections(i).tpl
//      val mask = new Image(tpl.rect.height, tpl.rect.width)
//      for (j <- 0 until tpl.mask_list.length) {
//        val x = tpl.mask_list(j) % tpl.rect.width
//        val y = tpl.mask_list(j) / tpl.rect.width
//        mask.data(y, x) = 255
//      }
//      all_detections(i).mask = mask
//      //convert BiGGDetection struct  to Detection message
//      detectionMsg += all_detections(i)
//
//      val d = all_detections(i)
//      // fill in indices vector (used for point cloud segmentation)
//      for (j <- 0 until tpl.mask_list.length) {
//        val x = tpl.mask_list(j) % tpl.rect.width
//        val y = tpl.mask_list(j) / tpl.rect.width
//        val index = (d.roi.y + y) * image.cols + (d.roi.x + x)
//        detectionMsg(i).mask.indices.indices.push_back(index)
//      }
//    }
  }

/**  ////// OMITTED TRAINING STUFF

var root_templates_: Vector[BinarizedGradientTemplate]
var crt_object_: Int

/**
 * Starts training for a new object category model. It may allocate/initialize
 * data structures needed for training a new category.
 * @param name
 */
def startTraining(name: String) = {
	crt_object_ = -1;
	for (size_t i=0;i<names.size();++i) {
		if (names[i]==name) {
			crt_object_ = i;
			break;
		}
	}
	if (crt_object_==-1) {
		crt_object_ = names.size();
		names.push_back(name);
	}
}

def trainInstance(img: Image, level: Int, pyr: BinarizedGradientPyramid, mask_pyr: BinarizedGradientPyramid, templates: Vector[BinarizedGradientTemplate], roi: Rect, mask: Image, template_radius: Float, accept_threshold: Float) = {
	BinarizedGradientTemplate bgt;
	std::vector<BiGGDetection> detections;
	float reduction_factor = float(1<<level);
	std::vector<cv::Point2i> locations;
	locations.resize(9);
    int xc = (roi.x + roi.width/2) / reduction_factor;
	int yc = (roi.y + roi.height/2) / reduction_factor;
	int dir[2][9] = {
			{ -1,-1,-1,0,0,0,1,1,1},
			{ -1,0,1,-1,0,1,-1,0,1}
	};
	for (int k=0;k<9;++k) {
		locations[k] = cv::Point2i(xc+dir[0][k],yc+dir[1][k]);
	}
	detect(pyr[level], locations, templates, detections, template_radius/reduction_factor, level, accept_threshold);


	int template_id = -1;
	if (detections.size()==0) {
		printf("No detections, adding template: %d on level %d\n", (int)templates.size(), level);
		val bgt = fillTemplateFromGradientImage(pyr[level], roi, mask, mask_pyr[level], template_radius/reduction_factor, level);

//		show_template(bgt, "template");
//		cv::waitKey(0);
		templates.push_back(bgt);
		template_id = templates.size()-1;
	}
	else {
		int max_id = 0;
		float max_score = detections[0].score;
		for (size_t i=1;i<detections.size();++i) {
			if (detections[i].score>max_score) {
				max_score = detections[i].score;
				max_id = i;
			}
		}

		printf("Template detected on level: %d, score: %g\n", level, detections[max_id].score);
		template_id = detections[max_id].index;
	}

	if (level>pyr.start_level) {
		trainInstance(img, level-1, pyr, mask_pyr, templates[template_id].templates, roi, mask, template_radius, accept_threshold);
	}
}

/**
 * Trains the model on a new data instance.
 * @param name The name of the model
 * @param data Training data instance
 */
def trainInstance(name: String, data: TrainingData) = {
	printf("BiGG: training instance: %s\n", name.c_str());

	cv::Mat img_gray;
	// compute the gradient summary image
	cv::Mat img = data.image;
    if(img.channels() != 1) {
    	cv::cvtColor(img, img_gray, CV_BGR2GRAY);
    }
    else {
    	img_gray = img;
    }

	static cv::Mat mag;
	static cv::Mat phase;
	computeGradients(img_gray, mag, phase);

    static cv::Mat binGrad;
    static cv::Mat cleanGrad;
    binarizeGradients(mag, phase, binGrad);
    gradMorphology(binGrad, cleanGrad);

	BinarizedGradientPyramid pyr(cleanGrad,start_level_,levels_);

//	show_mask(data.image, data.roi, data.mask);

	cv::Mat mask = cv::Mat(img.rows, img.cols, CV_8UC1);
	mask = cv::Scalar::all(0);
	cv::Mat mask_roi = mask(data.roi);
	data.mask.copyTo(mask_roi);

	BinarizedGradientPyramid mask_pyr(mask,start_level_,levels_);

	trainInstance(img, pyr.start_level+pyr.levels-1, pyr, mask_pyr, root_templates_, data.roi, data.mask, template_radius_, accept_threshold_);
}


/**
 * Saves a trained model.
 * @param name model name
 */
def endTraining(name: String) = {
	model_storage_->save(name, getName(), root_templates_);
}

*/ //////// END OMITTED TRAINING STUFF

  /**
   * Compute magnitude and phase in degrees from single channel image
   * @param img input image
   * @param mag gradient magnitude, returned
   * @param phase gradient phase, returned
   *
   * \pre img.channels()==1 && img.rows>0 && img.cols>0
   */
  def computeGradients(img: Image): (ImageF, ImageF) = {
    //Find X and Y gradients
    val (x, y) = img.scharr
    cartToPolar(x, y)
  }

  def cartToPolar(x: Image, y: Image): (ImageF, ImageF) = {
    val mag = new ImageF(x.data.zipWith(y.data, (a, b) => math.sqrt(a*a + b*b).asInstanceOf[Float]))
    val phase = new ImageF(x.data.zipWith(y.data, (a, b) => (math.atan2(b, a)*180/math.Pi).asInstanceOf[Float]).mmap(a => if (a < 0) a + 360 else a))
    (mag, phase)
  }

  /**
   * Turn mag and phase into a binary representation of 8 gradient directions.
   * @param mag Floating point gradient magnitudes
   * @param phase Floating point angles
   * @param binryGradient binarized gradients, returned value. This will be allocated if it is empty or is the wrong size.
   * @return
   *
   * \pre mag.rows==phase.rows && mag.cols==phase.cols
   * \pre mag.rows>0 && mag.cols>0
   */
  def binarizeGradients(mag: ImageF, phase: ImageF): Image = {
    val binaryGradient = new Image(mag.rows, mag.cols)
    for (i <- 0 until mag.rows) {
      for (j <- 0 until mag.cols) {
        if (mag.data(i, j) >= magnitude_threshold_) {
          var angle = phase.data(i, j)
          if (angle >= 180) {
            angle -= 180 //Ignore polarity of the angle
          }
//          println((angle / (180.0 / 8)).asInstanceOf[Int])
          binaryGradient.data(i, j) = 1 << (angle / (180.0 / 8)).asInstanceOf[Int]
        }
      }
    }
    binaryGradient
  }

  /**
   * Filter out noisy gradients via non-max suppression in a 3x3 area.
   * @param binaryGradient input binarized gradient
   * @param cleaned gradient, will be allocated if not already
   */
  def gradMorphology(binaryGradient: Image): Image = {
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
    val cleanedGradient = new Image(rows, cols)
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
    for (i <- 1 until 9) {
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

  val name_ = "BiGG"
  // PARAMETERS

  // The radius of the template
  val template_radius_ = 15

  // Threshold for accepting a template match (1 is perfect)
  val accept_threshold_ = 0.82f

  // How much the accept threshold decays on each level of the pyramid
  val accept_threshold_decay_ = 0.1f

  // Ignore gradients with lower magnitude
  val magnitude_threshold_ = 200

  // Start level in the pyramid
//  val start_level_ = 2
  val start_level_ = 3

  // Levels in the pyramid
//  val levels_ = 2
  val levels_ = 1

  // Fraction overlap between two detections above which one of them is suppressed
  val fraction_overlap_ = 0.6f

  // If 1 use cosine matching; if 0 use AND of bytes
  val match_method_ = 0
}