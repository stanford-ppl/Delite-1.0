package ppl.apps.robotics.gradient2

class BiGGDetection(
                     val name: String,
                     val score: Float,
                     val roi: Rect,
                     val mask: GrayscaleImage,
                     val index: Int,
                     val x: Int,
                     val y: Int,
                     val tpl: BinarizedGradientTemplate,
                     val crt_tpl: BinarizedGradientTemplate)
{

}