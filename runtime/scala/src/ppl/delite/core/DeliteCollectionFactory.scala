package ppl.delite.core

// TODO: merge this with DeliteProxy factory to create a single, expressive interface for instantiating DeliteDSLTypes
// TODO: in the merged version, move DeliteCollection.shape to DeliteDSLType.initParams, or something like that

@deprecated
trait DeliteCollectionFactory[C[X] <: DeliteCollection[X]] {
  //def newInstance[A](size: Int, shape: Option[Tuple2[Int,Int]])(implicit m: ClassManifest[A]): C[A]
  def newInstance[A](size: Int, shape: Option[Any])(implicit m: ClassManifest[A]): C[A]
}