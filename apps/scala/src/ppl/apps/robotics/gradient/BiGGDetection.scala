package ppl.apps.robotics.gradient

/**
 * Created by IntelliJ IDEA.
 * User: Anand Atreya
 * Date: Oct 17, 2010
 * Time: 10:28:35 PM
 * To change this template use File | Settings | File Templates.
 */

class BiGGDetection {
	var name: String = null
  var score: Float = 0.0f
	var roi: Rect = null
	var mask: Image = null
	var index: Int = 0
	var x: Int = 0
  var y: Int = 0
  var tpl: BinarizedGradientTemplate = null
  var crt_tpl: BinarizedGradientTemplate = null
}