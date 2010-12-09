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

class Edge(var message: UnaryFactor, var old_message: UnaryFactor)
class Vertex(var potential: UnaryFactor, var belief: UnaryFactor)

// Edge that can pass message back and forth between two vertices
class MessageEdge(val v1: Vertex, val inEdgeV1 : Edge, val v2: Vertex, val inEdgeV2 : Edge) {
  // In edge
  def in(v: Vertex) : Edge = {
    assert(v == v1 || v == v2)

    if(v == v1) inEdgeV1 else inEdgeV2
  }

  // Out edge
  def out(v: Vertex) : Edge = {
    assert(v == v1 || v == v2)

    if(v == v1) inEdgeV2 else inEdgeV1
  }
}

object GraphLBP {
  var colors = 5
  var damping = 0.1
  var bound = 1E-15
  var rows = 200
  var cols = 200
  var sigma = 2
  var lambda = 10
  var smoothing = "laplace"

  val edgePotential = new BinaryFactor(0, colors, 0, colors)

  def main(args: Array[String]) = {
    // Generate image
    val img = new LBPImage(rows, cols)
    img.paintSunset(colors)
    img.corrupt(sigma)

    // Construct graph from image
    val g = constructGraph(img, colors, sigma)

    if(smoothing == "laplace") {
      edgePotential.setLaplace(lambda)
    }
    else if(smoothing == "square") {
      edgePotential.setAgreement(lambda)
    }

    Graph.runUpdateFunction(g, Consistency.Edge)(bpUpdate)
  }

  def bpUpdate(v: Vertex, scope: Scope[Vertex, MessageEdge]) {
    // Flip messages on in edges
    for(me <- scope.edgeData) {
      me.in(v).old_message = me.in(v).message
    }

    v.belief = v.potential

    // Multiply belief by messages
    for(me <- scope.edgeData) {
      v.belief.times(me.in(v).old_message)
    }

    // Normalize the belief
    v.belief.normalize()

    // Send outbound messages
    for(me <- scope.edgeData) {
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
      if(residual > bound) {
        scope.enqueueUpdateFunctionVertex(Consistency.Edge, v)(bpUpdate)
      }
    }
  }

  def constructGraph(img: LBPImage, numRings: Int, sigma: Double) : UndirectedGraph[Vertex, MessageEdge] = {
    val g = new UndirectedGraphImpl[Vertex, MessageEdge]

    // Same belief for everyone
    val belief = new UnaryFactor(0, numRings)
    belief.uniform()
    belief.normalize()

    val sigmaSq = sigma*sigma

    val vertices = Array.ofDim[Vertex](img.rows, img.cols)

    // Set vertex potential based on image
    for(i <- 0 until img.rows) {
      for(j <- 0 until img.cols) {
        val pixelId = img.vertid(i, j)
        val potential = new UnaryFactor(pixelId, numRings)

        val obs = img.data(pixelId)

        for(pred <- 0 until numRings) {
          potential.logP(pred) = -(obs - pred)*(obs - pred) / (2.0 * sigmaSq)
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

    // Add bidirectional edges between neighboring pixels
    for(i <- 0 until img.rows - 1) {
      for(j <- 0 until img.cols - 1) {
        val edgeRight = new Edge(message.copy(img.vertid(i, j+1)), message.copy(img.vertid(i, j+1)))
        val edgeRightBack = new Edge(message.copy(img.vertid(i, j)), message.copy(img.vertid(i, j)))

        g.addEdge(new MessageEdge(vertices(i)(j), edgeRight, vertices(i)(j+1), edgeRightBack), vertices(i)(j), vertices(i)(j+1))

        val edgeDown = new Edge(message.copy(img.vertid(i+1, j)), message.copy(img.vertid(i+1, j)))
        val edgeDownBack = new Edge(message.copy(img.vertid(i, j)), message.copy(img.vertid(i, j)))

        g.addEdge(new MessageEdge(vertices(i)(j), edgeDown, vertices(i+1)(j), edgeDownBack), vertices(i)(j), vertices(i+1)(j))
      }
    }

    g
  }
}