package ppl.apps.bh

import java.awt.Color
import ppl.delite.statistics.Animation

class Body(val index: Int,
           var rx: Double, var ry: Double, var rz: Double, // position
           var vx: Double, var vy: Double, var vz: Double, // velocity
           val mass: Double, val color: Color) {

  if (Body.minmass > mass || Body.minmass < 0.0) Body.minmass = mass
  if (Body.maxmass < mass)                       Body.maxmass = mass
  lazy val radius = 2 * Math.log(mass) / Animation.factorX(Math.log(Body.minmass))

  var fx, fy, fz = 0.0 // force

  def in(qd: Quad) = qd.contains(rx, ry, rz)

  // update the velocity and position of the invoking Body
  def update(dt: Double) {
    vx += dt * fx / mass
    vy += dt * fy / mass
    vz += dt * fz / mass
    rx += dt * vx
    ry += dt * vy
    rz += dt * vz
  }

  // return the Euclidean distance bewteen the invoking Body and b
  def distanceTo(b: Body) = {
    val (dx, dy, dz) = (rx - b.rx, ry - b.ry, rz - b.rz)
    Math.sqrt(dx * dx + dy * dy + dz * dz)
  }

  // reset the force of the invoking Body to 0
  def resetForce() { fx = 0.0; fy = 0.0; fz = 0.0 }

  // compute the net force acting between the invoking body a and b,
  // and add to the net force acting on the invoking Body
  def addForce(b: Body) {
    val (dx, dy, dz) = (b.rx - rx, b.ry - ry, b.rz - rz)
    val disq = dx * dx + dy * dy + dz * dz
    val dist = Math.sqrt(disq)
    val F = (Body.G * mass * b.mass) / (disq + Body.EPS * Body.EPS)
    fx += F * dx / dist
    fy += F * dy / dist
    fz += F * dz / dist
  }

  // make a body which is center of mass of this and bo bodies.
  def +(bo: Body) = {
    val ix = index + bo.index
    val m = mass + bo.mass
    val x = (rx * mass + bo.rx * bo.mass) / m
    val y = (ry * mass + bo.ry * bo.mass) / m
    val z = (rz * mass + bo.rz * bo.mass) / m
    new Body(ix, x, y, z, 0.0, 0.0, 0.0, m, Animation.BLACK )
  }

  // draw the invoking Body in Turtle graphics
  def draw() {
    Animation.setPenColor(color)
    Animation.filledCircle(rx, ry, radius)
//    Animation.pixel(rx, ry)
  }

  private def dfmt(v: Double) = (v formatted ("%8.1E"))

  // convert to string representation formatted nicely
  override def toString() = (index + ": r[" +
          dfmt(rx) + " " + dfmt(ry) + " " + dfmt(rz) + "] v'" +
          dfmt(vx) + " " + dfmt(vy) + " " + dfmt(vz) + "' " +
          dfmt(mass) + "g " + ((color.getRGB()) formatted ("h%08X")))

} // class Body

object Body {
  val G = 6.67e-11 // gravitational constant
  val EPS = 3E4    // softening parameter
  var minmass = -1.0
  var maxmass = -1.0
} // object Body
