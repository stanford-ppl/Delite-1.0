package ppl.apps.ml.baseline.lbp

import ppl.apps.ml.baseline.lbp.Graph.{Scope, Consistency}
import ppl.apps.ml.lbp._

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 12/06/2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class Edge(var message: UnaryFactor, var old_message: UnaryFactor) {
  def copy(): Edge = {
    new Edge(message.copy(), old_message.copy())
  }
}
class Vertex(var potential: UnaryFactor, var belief: UnaryFactor)

// Edge that can pass message back and forth between two vertices
class MessageEdge(val v1: Vertex, val inEdgeV1: Edge, val v2: Vertex, val inEdgeV2: Edge) {
  // In edge
  def in(v: Vertex): Edge = {
    assert(v == v1 || v == v2)

    if (v == v1) inEdgeV1 else inEdgeV2
  }

  // Out edge
  def out(v: Vertex): Edge = {
    assert(v == v1 || v == v2)

    if (v == v1) inEdgeV2 else inEdgeV1
  }

  def notMe(v: Vertex): Vertex = {
    assert(v == v1 || v == v2)

    if (v == v1) v2 else v1
  }
}

object GraphLBP {
  var colors = 5
  var damping = 0.1
  var bound = 1E-15
  var rows = 50
  var cols = 50
  var sigma = 2
  var lambda = 10
  var smoothing = "laplace"
  var pred_type = "map";

  var count = 1

  val edgePotential = new BinaryFactor(0, colors, 0, colors)

  /* Residual printing
  var out_file = new java.io.FileOutputStream("residuals_scala.txt") 
  var out_stream = new java.io.PrintStream(out_file)
*/

  def main(args: Array[String]) = {
    // Generate image
    val img = new LBPImage(rows, cols)
    img.paintSunset(colors)
    img.save("src_img.pgm")
    img.corrupt(sigma)
    img.save("noise_img.pgm")

    /*val img = LBPImage.load("noisy_image.raw")
    img.save("check_img.pgm")*/

    // Construct graph from image
    val g = constructGraph(img, colors, sigma)

    if (smoothing == "laplace") {
      edgePotential.setLaplace(lambda)
    }
    else if (smoothing == "square") {
      edgePotential.setAgreement(lambda)
    }

    println(edgePotential)

    Graph.runUpdateFunction(g, Consistency.Edge)(bpUpdate)

    // Predict the image! Well as of now we don't even get to this point, so fuck
    if (pred_type == "map") {
      for (v <- g.vertexSet) {
        img.data(v.belief.v) = v.belief.max_asg();
      }
    }
    else if (pred_type == "exp") {
      for (v <- g.vertexSet) {
        img.data(v.belief.v) = v.belief.max_asg();
      }
    }

    // Save the predicted image
    img.save("pred_img.pgm")

    /*out_stream.println(count)
    out_stream.close*/
  }

  def bpUpdate(v: Vertex, scope: Scope[Vertex, MessageEdge]) {
    // Flip messages on in edges
    for (me <- scope.edgeData) {
      me.in(v).old_message = me.in(v).message
    }

    // count += 1
    
    v.belief = v.potential.copy()

    // Multiply belief by messages
    for (me <- scope.edgeData) {
      v.belief.times(me.in(v).old_message)
    }

    // Normalize the belief
    v.belief.normalize()

    // Send outbound messages
    for (me <- scope.edgeData) {
      // Compute the cavity
      val cavity = v.belief.copy()
      cavity.divide(me.in(v).old_message)
      cavity.normalize()

      // Convolve the cavity with the edge factor
      val outEdge = me.out(v)
      val outMsg = new UnaryFactor(outEdge.message.v, outEdge.message.arity)
      outMsg.convolve(edgePotential, cavity)
      outMsg.normalize()

      // Damp the message
      outMsg.damp(outEdge.message, damping)

      outEdge.message = outMsg

      // Compute message residual
      val residual = outMsg.residual(outEdge.old_message)

      /*
      if(count < 5000) {
        out_stream.println("in: " + outMsg.v + " out: " + me.in(v).message.v + " " + residual)
      } */

      // Enqueue update function on target vertex if residual is greater than bound
      if (residual > bound) {
        scope.enqueueUpdateFunctionVertex(Consistency.Edge, me.notMe(v))(bpUpdate)
      }
    }
  }

  def constructGraph(img: LBPImage, numRings: Int, sigma: Double): UndirectedGraph[Vertex, MessageEdge] = {
    val g = new UndirectedGraphImpl[Vertex, MessageEdge]

    // Same belief for everyone
    val belief = new UnaryFactor(0, numRings)
    belief.uniform()
    belief.normalize()

    val sigmaSq = sigma * sigma

    val vertices = Array.ofDim[Vertex](img.rows, img.cols)

    // Set vertex potential based on image
    for (i <- 0 until img.rows) {
      for (j <- 0 until img.cols) {
        val pixelId = img.vertid(i, j)
        val potential = new UnaryFactor(pixelId, numRings)

        val obs = img.data(pixelId)

        for (pred <- 0 until numRings) {
          potential.logP(pred) = -(obs - pred) * (obs - pred) / (2.0 * sigmaSq)
        }

        potential.normalize()

        val vertex = new Vertex(potential, belief.copy(pixelId))

        vertices(i)(j) = vertex
        g.addVertex(vertex)
      }
    }

    val message = new UnaryFactor(0, numRings)
    message.uniform()
    message.normalize()

    val oldMessage = message.copy()

    val message2 = message.copy()
    val oldMessage2 = message.copy()

    val templateEdge = new Edge(message, oldMessage)
    val baseEdge = new Edge(message2, oldMessage2)

    // Add bidirectional edges between neighboring pixels
    for (i <- 0 until img.rows - 1) {
      for (j <- 0 until img.cols - 1) {
        message.v = img.vertid(i, j + 1)
        oldMessage.v = img.vertid(i, j + 1)

        message2.v = img.vertid(i, j)
        oldMessage2.v = img.vertid(i, j)

        val edgeRight = templateEdge.copy()

        g.addEdge(new MessageEdge(vertices(i)(j), edgeRight, vertices(i)(j + 1), baseEdge.copy()), vertices(i)(j), vertices(i)(j + 1))

        message.v = img.vertid(i + 1, j)
        oldMessage.v = img.vertid(i + 1, j)

        val edgeDown = templateEdge.copy()

        g.addEdge(new MessageEdge(vertices(i)(j), edgeDown, vertices(i + 1)(j), baseEdge.copy()), vertices(i)(j), vertices(i + 1)(j))
      }
    }

    g
  }
}