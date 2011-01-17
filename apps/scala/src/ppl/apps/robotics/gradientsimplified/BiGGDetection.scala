package ppl.apps.robotics.gradientsimplified

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