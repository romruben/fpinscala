
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
  //984.8985157000001 vertical rend
    def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
      var outputColor = (0, 0, 0, 0)

      val startXPoint = clamp(x - radius, 0, src.width - 1)
      val endXPoint = clamp(radius + x, 0, src.width - 1)

      val startYPoint = clamp(y - radius, 0, src.height - 1)
      val endYPoint = clamp(radius + y, 0, src.height - 1)

      var xPixels = startXPoint
      var pixelsEvaluated = 0

      while (xPixels <= endXPoint) {
        var yPixels = startYPoint

        while (yPixels <= endYPoint) {
          val pixel = src(xPixels, yPixels)

          outputColor = (outputColor._1 + red(pixel), outputColor._2 + green(pixel),
            outputColor._3 + blue(pixel), outputColor._4 + alpha(pixel))

          yPixels += 1
          pixelsEvaluated += 1
        }
        xPixels += 1
      }

      rgba(outputColor._1 / pixelsEvaluated, outputColor._2 / pixelsEvaluated, outputColor._3 / pixelsEvaluated,
        outputColor._4 / pixelsEvaluated)
    }


  //4737.8632103 vertical rend
//  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
//
//    val pixels = for {
//      x <- x - radius to radius + x
//      y <- y - radius to radius + y
//    } yield src(clamp(x, 0, src.width - 1), clamp(y, 0, src.height - 1))
//
//    val pixelsLength = pixels.length
//    val outputColor = pixels.distinct.foldLeft((0, 0, 0, 0))((acc, pixel) =>
//      (acc._1 + red(pixel), acc._2 + green(pixel), acc._3 + blue(pixel), acc._4 + alpha(pixel)))
//
//    rgba(outputColor._1 / pixelsLength, outputColor._2 / pixelsLength, outputColor._3 / pixelsLength, outputColor._4 / pixelsLength)
//}

  //4149.7932229 ms vertical blur
//    def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
  //
  //      val pixels = for {
  //        x <- clamp(x - radius, 0, src.width - 1) to clamp(radius + x, 0, src.width - 1)
  //        y <- clamp(y - radius, 0, src.height - 1) to clamp(radius + y, 0, src.height - 1)
  //      } yield src(x, y)
  //
  //
  //      val pixelsLength = pixels.length
  //      val outputColor = pixels.distinct.foldLeft((0, 0, 0, 0))((acc, pixel) =>
  //        (acc._1 + red(pixel), acc._2 + green(pixel), acc._3 + blue(pixel), acc._4 + alpha(pixel)))
  //
  //      rgba(outputColor._1 / pixelsLength, outputColor._2 / pixelsLength, outputColor._3 / pixelsLength, outputColor._4 / pixelsLength)
  //  }


//  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
//
//    val pixels = for {
//      x <- clamp(x - radius, 0, src.width - 1) to clamp(radius + x, 0, src.width - 1)
//      y <- clamp(y - radius, 0, src.height - 1) to clamp(radius + y, 0, src.height - 1)
//    } yield src(x, y)
//
//    val pixelsLength = pixels.length
//    var it = 0
//    var outputColor = (0,0,0,0)
//    while(it < pixelsLength){
//      val pixel = pixels.indexOf(it)
//      outputColor = (outputColor._1 + red(pixel), outputColor._2 + green(pixel), outputColor._3 + blue(pixel), outputColor._4 + alpha(pixel))
//      it += 1
//    }
//
//    rgba(outputColor._1 / pixelsLength, outputColor._2 / pixelsLength, outputColor._3 / pixelsLength, outputColor._4 / pixelsLength)
//  }


  //279.8064333 ms vertical parl
//  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
//    var outputColor = (0,0,0,0)
//    var itRow = clamp(x - radius, 0, src.width - 1)
//    val rowEnd = clamp(radius + x, 0, src.width - 1)
//
//    var itCol = clamp(y - radius, 0, src.height - 1)
//    val colEnd = clamp(radius + y, 0, src.height - 1)
//    var colCount = clamp(y - radius, 0, src.height - 1)
//
//    var totalXY = 0
//
//    while(itRow <= rowEnd){
//      itCol = colCount
//      while(itCol <= colEnd){
//        val pixel = src(itRow, itCol)
//        outputColor = (outputColor._1 + red(pixel), outputColor._2 + green(pixel), outputColor._3 + blue(pixel), outputColor._4 + alpha(pixel))
//        totalXY += 1
//        colCount +=1
//      }
//      itRow +=1
//    }
//
//    rgba(outputColor._1 / totalXY, outputColor._2 / totalXY, outputColor._3 / totalXY, outputColor._4 / totalXY)
//  }
}
