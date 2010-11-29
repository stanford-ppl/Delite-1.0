package ppl.apps.ml.gda

import ppl.delite.dsl.optiml.Matrix
import ppl.delite.core.Config

/*
object blah {

  //////////////////////////////////////
  // new optiml operators
  //
  // expose parallel constructs in domain-specific ways
  // restrict semantics to improve locality and avoid dependencies



  /************ SUM *****************/

  // old GDA main loop
  val end = if(Config.CPUThreadNumOverride) Config.CPUThreadNum else Runtime.getRuntime.availableProcessors
    val coll = Vector.range(0,end).map(j => {
      var sigma = Matrix[Double](n,n)
      for (i <- m*j/end until m*(j+1)/end) {
        if (y(i) == false) {
          sigma += ((x(i) - mu0).trans).outer(x(i) - mu0)
        }
        else {
          sigma += ((x(i) - mu1).trans).outer(x(i) - mu1)
        }
      }
      sigma
    })

    val sigma = coll.sum[Matrix[Double]]



  // new GDA main loop
  val sigma = sumimpl(0, m) { i =>
    if (y(i) == false){
      ((x(i)-mu0).trans).outer(x(i)-mu0)
    }
    else{
      ((x(i)-mu1).trans).outer(x(i)-mu1)
    }
  }



  // eventual GDA main loop (with compiler plugin)
  val sigma = sum(0, m) {
    if (y($) == false){
      ((x($)-mu0).trans).outer(x($)-mu0)
    }
    else{
      ((x($)-mu1).trans).outer(x($)-mu1)
    }
  }



  /*************** VECTOR CONSTRUCTION **************/

  // old NB operation
  val phi_y0 = Vector.range(0, numTokens).map( j => {
    var nonspamwordcount = 0.0
    for (i <- 0 until numTrainDocs){
      if (ts.classifications(i) == 0){
        nonspamwordcount = nonspamwordcount + ts.features(j, i)
      }
    }
    (nonspamwordcount + 1) / (weightednonspamcount + numTokens)
  })



  // new NB operation: uses domain-specific index vector
  val phi_y0 = (0::numTokens).applyimpl{ j =>
    var nonspamwordcount = 0.0
    for (i <- 0 until numTrainDocs){
      if (ts.classifications(i) == 0){
        nonspamwordcount = nonspamwordcount + ts.features(j, i)
       }
    }
    (nonspamwordcount + 1) / (weightednonspamcount + numTokens)
  }


  // eventual NB operation, using compiler plugin

  val phi_y0 = (0::numTokens) {
    var nonspamwordcount = 0.0
    for (i <- 0 until numTrainDocs){
      if (ts.classifications(i) == 0){
        nonspamwordcount = nonspamwordcount + ts.features($, i)
      }
    }
    (nonspamwordcount + 1) / (weightednonspamcount + numTokens)
  }



  /************** until converged ***********************/

  // relaxed dependency dynamic optimization embedded

  // see SVM


}
*/