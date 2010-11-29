package ppl.delite.statistics

import java.io._
import java.net._
import java.awt._
import java.awt.geom._
import java.awt.event._
import java.awt.image._
import java.lang.Thread.sleep
import javax.swing._
import javax.imageio.ImageIO

object Animation extends ActionListener {

  // pre-defined colors
  val BLACK      = Color.BLACK
  val BLUE       = Color.BLUE
  val CYAN       = Color.CYAN
  val DARK_GRAY  = Color.DARK_GRAY
  val GRAY       = Color.GRAY
  val GREEN      = Color.GREEN
  val LIGHT_GRAY = Color.LIGHT_GRAY
  val MAGENTA    = Color.MAGENTA
  val ORANGE     = Color.ORANGE
  val PINK       = Color.PINK
  val RED        = Color.RED
  val WHITE      = Color.WHITE
  val YELLOW     = Color.YELLOW

  // default colors
  val DEFAULT_PEN_COLOR = BLACK
  val DEFAULT_CLEAR_COLOR = WHITE

  // current pen color
  private var penColor = DEFAULT_PEN_COLOR

  // default canvas size is SIZE-by-SIZE
  private val SIZE = 512
  private var width = SIZE
  private var height = SIZE

  // default pen radius
  private val DEFAULT_PEN_RADIUS = 0.002

  // current pen radius
  private var penRadius = DEFAULT_PEN_RADIUS

  // show we draw immediately or wait until next show?
  private var defer = false

  // flag to determine when to draw JFrame on screen
  private var inited = false

  // boundary of drawing canvas, 5% border
  private val BORDER = 0.05
  private val DEFAULT_XMIN = 0.0
  private val DEFAULT_XMAX = 1.0
  private val DEFAULT_YMIN = 0.0
  private val DEFAULT_YMAX = 1.0
  private var (xmin, ymin, xmax, ymax) = (DEFAULT_XMIN, DEFAULT_XMAX, DEFAULT_YMIN, DEFAULT_YMAX)

  // font
  private val DEFAULT_FONT = new Font("Serif", Font.PLAIN, 16)
  private var font = DEFAULT_FONT

  // double buffered graphics
  private var offscreenImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
  private var onscreenImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
  private var offscreen = offscreenImage.createGraphics()
  private var onscreen = onscreenImage.createGraphics()

  // the frame for drawing to the screen
  private var frame = new JFrame()

  def setCanvasSize(w: Int, h: Int, title: String) {
    width = w
    height = h
    init(title)
  }

  // init
  def init(title: String) {
    inited = false
    if (frame != null) frame.setVisible(false)
    setXscale()
    setYscale()
    offscreen.setColor(DEFAULT_CLEAR_COLOR)
    offscreen.fillRect(0, 0, width, height)
    setPenColor()
    setPenRadius()
    setFont()
    clear()

    // add antialiasing
    val hints = new RenderingHints(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    hints.put(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
    offscreen.addRenderingHints(hints)

    // frame stuff
    val icon = new ImageIcon(onscreenImage);
    frame.setContentPane(new JLabel(icon));
    frame.setResizable(false);
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE); // closes all windows
    // frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);      // closes only current window
    frame.setTitle(title)
    frame.setJMenuBar(createMenuBar())
    frame.pack()
    // frame.setVisible(true);
    inited = true;
  }

  private val SaveCommand: String = " Save...   "
  private val ExitCommand: String = " Exit"

  // create the menu bar
  def createMenuBar() = {
    val menuBar = new JMenuBar()
    val menu = new JMenu("File")
    menuBar.add(menu)

    val menuItem1 = new JMenuItem(SaveCommand)
    menuItem1.addActionListener(this)
    val keyMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()
    val keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_S, keyMask)
    menuItem1.setAccelerator(keyStroke)
    menu.add(menuItem1)

    val menuItem2 = new JMenuItem(ExitCommand)
    menuItem2.addActionListener(this)
    menuItem2.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0))
    menu.add(menuItem2)

    menuBar
  }



  // change the user coordinate system
  def setXscale(): Unit = setXscale(DEFAULT_XMIN, DEFAULT_XMAX)

  def setYscale(): Unit = setYscale(DEFAULT_YMIN, DEFAULT_YMAX)

  def setXscale(min: Double, max: Double) {
    val size = max - min
    xmin = min - BORDER * size
    xmax = max + BORDER * size
  }

  def setYscale(min: Double, max: Double) {
    val size = max - min
    ymin = min - BORDER * size
    ymax = max + BORDER * size
  }

  // helper functions that scale from user coordinates to screen coordinates
  def scaleX(x: Double) = (width * (x - xmin) / (xmax - xmin))

  def scaleY(y: Double) = (height * (ymax - y) / (ymax - ymin))

  def factorX(w: Double) = (w * width / Math.abs(xmax - xmin))

  def factorY(h: Double) = (h * height / Math.abs(ymax - ymin))

  // clear the screen with given color
  def clear(): Unit = clear(DEFAULT_CLEAR_COLOR)

  def clear(color: Color) {
    offscreen setColor color
    offscreen.fillRect(0, 0, width, height)
    offscreen setColor penColor
    show()
  }

  // set the pen size
  def setPenRadius(): Unit = setPenRadius(DEFAULT_PEN_RADIUS)

  def setPenRadius(r: Double) {
    penRadius = r * SIZE
    val stroke = new BasicStroke(penRadius.asInstanceOf[Float])
    offscreen setStroke stroke
  }

  // set the pen color
  def setPenColor(): Unit = setPenColor(DEFAULT_PEN_COLOR)

  def setPenColor(color: Color) {
    penColor = color
    offscreen setColor penColor
  }

  // write the given string in the current font
  def setFont(): Unit = setFont(DEFAULT_FONT)

  def setFont(f: Font) = font = f

  // draw a line from (x0, y0) to (x1, y1)
  def line(x0: Double, y0: Double, x1: Double, y1: Double) {
    offscreen.draw(new Line2D.Double(scaleX(x0), scaleY(y0), scaleX(x1), scaleY(y1)))
    show()
  }

  // draw one pixel at (x, y)
  def pixel(x: Double, y: Double) {
    offscreen.fillRect(Math.round(scaleX(x)).asInstanceOf[Int],
                       Math.round(scaleY(y)).asInstanceOf[Int], 1, 1)
  }

  // draw point at (x, y)
  def point(x: Double, y: Double) {
    val xs = scaleX(x)
    val ys = scaleY(y)
    val r = penRadius
    if (r <= 1) pixel(x, y)
    else offscreen.fill(new Ellipse2D.Double(xs - r / 2, ys - r / 2, r, r))
    show()
  }

  // draw circle of radius r, centered on (x, y); degenerate to pixel if small
  def circle(x: Double, y: Double, r: Double) {
    val xs = scaleX(x)
    val ys = scaleY(y)
    val ws = factorX(2 * r)
    val hs = factorY(2 * r)
    if (ws <= 1 && hs <= 1) pixel(x, y)
    else offscreen.draw(new Ellipse2D.Double(xs - ws / 2, ys - hs / 2, ws, hs))
    show()
  }


  // draw filled circle of radius r, centered on (x, y); degenerate to pixel if small
  def filledCircle(x: Double, y: Double, r: Double) {
    val xs = scaleX(x)
    val ys = scaleY(y)
    val ws = factorX(2 * r)
    val hs = factorY(2 * r)
    if (ws <= 1 && hs <= 1) pixel(x, y)
    else offscreen.fill(new Ellipse2D.Double(xs - ws / 2, ys - hs / 2, ws, hs))
    show()
  }

  // draw squared of side length 2r, centered on (x, y); degenerate to pixel if small
  def square(x: Double, y: Double, r: Double) {
    val xs = scaleX(x)
    val ys = scaleY(y)
    val ws = factorX(2 * r)
    val hs = factorY(2 * r)
    if (ws <= 1 && hs <= 1) pixel(x, y)
    else offscreen.draw(new Rectangle2D.Double(xs - ws / 2, ys - hs / 2, ws, hs))
    show()
  }

  // draw squared of side length 2r, centered on (x, y); degenerate to pixel if small
  def filledSquare(x: Double, y: Double, r: Double) {
    val xs = scaleX(x)
    val ys = scaleY(y)
    val ws = factorX(2 * r)
    val hs = factorY(2 * r)
    if (ws <= 1 && hs <= 1) pixel(x, y)
    else offscreen.fill(new Rectangle2D.Double(xs - ws / 2, ys - hs / 2, ws, hs))
    show()
  }

  // draw a polygon with the given (x[i], y[i]) coordinates
  def polygon(x: Array[Double], y: Array[Double]) {
    val N = x.length
    val path = new GeneralPath()
    val x0: Float = (scaleX(x(0))).asInstanceOf[Float]
    val y0: Float = (scaleY(y(0))).asInstanceOf[Float]
    path.moveTo(x0, y0)
    for (i <- 0 until N) {
      val xi: Float = (scaleX(x(i))).asInstanceOf[Float]
      val yi: Float = (scaleY(y(i))).asInstanceOf[Float]
      path.lineTo(xi, yi)
    }
    path.closePath()
    offscreen.draw(path)
    show()
  }

  // draw a filled polygon with the given (x[i], y[i]) coordinates
  def filledPolygon(x: Array[Double], y: Array[Double]) {
    val N = x.length
    val path = new GeneralPath()
    val x0: Float = (scaleX(x(0))).asInstanceOf[Float]
    val y0: Float = (scaleY(y(0))).asInstanceOf[Float]
    path.moveTo(x0, y0)
    for (i <- 0 until N) {
      val xi: Float = (scaleX(x(i))).asInstanceOf[Float]
      val yi: Float = (scaleY(y(i))).asInstanceOf[Float]
      path.lineTo(xi, yi)
    }
    path.closePath()
    offscreen.fill(path)
    show()
  }

  // get an image from the given filename
  private def getImage(filename: String): Image = {
    // to read from file
    var icon = new ImageIcon(filename)
    // try to read from URL
    if ((icon == null) || (icon.getImageLoadStatus() != MediaTracker.COMPLETE)) {
      try {icon = new ImageIcon(new URL(filename))}
      catch {case e: Exception => /* not a url */ }
    }
    // in case file is inside a .jar
    if ((icon == null) || (icon.getImageLoadStatus() != MediaTracker.COMPLETE)) {
      val url = this.getClass.getResource(filename)
      if (url == null) throw new RuntimeException("image " + filename + " not found")
      icon = new ImageIcon(url)
    }
    icon.getImage()
  }

  // draw picture (gif, jpg, or png) centered on (x, y)
  def picture(x: Double, y: Double, s: String) {
    val image = getImage(s)
    val xs = scaleX(x)
    val ys = scaleY(y)
    val ws = image.getWidth(null)
    val hs = image.getHeight(null)
    val X = Math.round(xs - ws / 2.0).asInstanceOf[Int]
    val Y = Math.round(ys - hs / 2.0).asInstanceOf[Int]
    offscreen.drawImage(image, X, Y, null)
    show()
  }

  // draw picture (gif, jpg, or png) centered on (x, y), rescaled to w-by-h
  def picture(x: Double, y: Double, s: String, w: Double, h: Double) {
    val image = getImage(s)
    val xs = scaleX(x)
    val ys = scaleY(y)
    val ws = factorX(w)
    val hs = factorY(h)
    if (ws <= 1 && hs <= 1) pixel(x, y)
    else {
      val X = Math.round(xs - ws / 2.0).asInstanceOf[Int]
      val Y = Math.round(ys - hs / 2.0).asInstanceOf[Int]
      val W = Math.round(ws).asInstanceOf[Int]
      val H = Math.round(hs).asInstanceOf[Int]
      offscreen.drawImage(image, X, Y, W, H, null)
    }
  }

  // write the given text string in the current font, center on (x, y)
  def text(x: Double, y: Double, s: String) {
    offscreen.setFont(font)
    val metrics = offscreen.getFontMetrics()
    val xs = scaleX(x)
    val ys = scaleY(y)
    val ws = metrics.stringWidth(s)
    val hs = metrics.getDescent()
    val X = (xs - ws / 2.0).asInstanceOf[Float]
    val Y = (ys + hs).asInstanceOf[Float]
    offscreen.drawString(s, X, Y)
    show()
  }

  // display on screen and pause for t miliseconds
  def show(t: Int) {
    defer = true
    onscreen.drawImage(offscreenImage, 0, 0, null)
    frame.repaint()
    try { /*Thread.currentThread().*/ sleep(t)}
    catch {case e: InterruptedException => println("Error sleeping")}
  }

  // view on-screen, creating new frame if necessary
  def show() {
    if (inited) frame.setVisible(true)
    if (!defer) onscreen.drawImage(offscreenImage, 0, 0, null)
    if (!defer) frame.repaint()
  }

  // save to file - suffix must be png, jpg, or gif
  def save(filename: String) {
    val file = new File(filename)
    val suffix = filename.substring(filename.lastIndexOf('.') + 1)

    // png files
    if (suffix.toLowerCase().equals("png")) {
      try {ImageIO.write(offscreenImage, suffix, file)}
      catch {case e: IOException => e.printStackTrace()}
    }

    // need to change from ARGB to RGB for jpeg
    // reference: http://archives.java.sun.com/cgi-bin/wa?A2=ind0404&L=java2d-interest&D=0&P=2727
    else if (suffix.toLowerCase().equals("jpg")) {
      val raster = offscreenImage.getRaster();
      val newRaster = raster.createWritableChild(0, 0, width, height, 0, 0, Array(0, 1, 2))
      val cm = offscreenImage.getColorModel().asInstanceOf[DirectColorModel]
      val newCM = new DirectColorModel(cm.getPixelSize(),
                                       cm.getRedMask(),
                                       cm.getGreenMask(),
                                       cm.getBlueMask())
      val rgbBuffer = new BufferedImage(newCM, newRaster, false, null)
      try {ImageIO.write(rgbBuffer, suffix, file)}
      catch {case e: IOException => e.printStackTrace()}
    }

    else println("Invalid image file type: " + suffix)
  }

  // open a save dialog when the user selects "Save As" from the menu
  def actionPerformed(e: ActionEvent) {
    e.getActionCommand() match {
      case SaveCommand => {
        val chooser = new FileDialog(frame, "Use a .png or .jpg extension", FileDialog.SAVE)
        chooser.setVisible(true)
        val filename = chooser.getFile()
        if (filename != null) save(chooser.getDirectory() + File.separator + filename)
      }
      case ExitCommand => {
        exit
      }
      case _ =>
    }
  }

} // object Animation
