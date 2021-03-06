
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops

    val grid = for {
      a <- x - radius to x + radius if a > -1 && a < src.width
      b <- y - radius to y + radius if b > -1 && b < src.height
    } yield (a, b)

    val pixelColoredSummed = grid.foldLeft((0,0,0,0)){
      case ((r, g, b, a), (pointX, pointY)) =>
        val rgba = src(pointX, pointY)
        (r + red(rgba), g + green(rgba), b + blue(rgba), a + alpha(rgba))
    }

    println("bluring: " + x + "  " + y)

    rgba(
      pixelColoredSummed._1 / grid.length,
      pixelColoredSummed._2 / grid.length,
      pixelColoredSummed._3 / grid.length,
      pixelColoredSummed._4 / grid.length)

  }

}
