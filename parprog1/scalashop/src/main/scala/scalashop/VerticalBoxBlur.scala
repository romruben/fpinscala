package scalashop

import org.scalameter._
import common._

//19237.198419
//13237.198419
//1292.1927222
//984.8985157000001
//4737.8632103 // sin whiles
object VerticalBoxBlurRuner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
    * `dst`, starting with `from` and ending with `end` (non-inclusive).
    *
    * Within each column, `blur` traverses the pixels by going from top to
    * bottom.
    */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    val coors = for {
      row <- from until end
      col <- 0 until src.height
    } yield (row, col)

    coors.foreach(pixel => dst.update(pixel._1, pixel._2, boxBlurKernel(src, pixel._1, pixel._2, radius)))
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
    *
    * Parallelization is done by stripping the source image `src` into
    * `numTasks` separate strips, where each strip is composed of some number of
    * columns.
    */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val it = if (numTasks != 0) (src.width / numTasks) max 1 else 1
    val split = 0 until src.width by it
    split.map(s => task {
      val incr = if (s + it > src.width) it - s else it + s
      blur(src, dst, s, incr, radius)
    }).foreach(_.join())
  }
}
