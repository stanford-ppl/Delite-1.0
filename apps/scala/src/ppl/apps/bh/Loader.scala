package ppl.apps.bh

import java.awt.Color
import java.io.File
import java.util.Scanner

object Loader {

  private var input: Scanner = _
  private var nbody: Int = _
  private var usize: Double = _
  private var loaded = 0

  def open(filename: String) {
    input = new Scanner(new File(filename))
    nbody = input.nextInt()
    usize = input.nextDouble()
  }

  def getNumBodies() = nbody

  def getSize() = usize

  def body() = {
    loaded += 1
    val px = input.nextDouble()
    val py = input.nextDouble()
    val pz = 0.0
    val vx = input.nextDouble()
    val vy = input.nextDouble()
    val vz = 0.0
    val mass = input.nextDouble()
    val r = input.nextInt()
    val g = input.nextInt()
    val b = input.nextInt()
    new Body(loaded, px, py, pz, vx, vy, vz, mass, new Color(r, g, b))
  }

  def close() {
    input.close()
  }

} // object Loader
