package scalashop

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 1
    val width = 3
    val height = 3
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 4
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
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    var i = from

    while ( i < end){
      var j = 0

      while ( j < src.height){

        dst.update(i,j,boxBlurKernel(src, i,j, radius))
        j += 1

      }

      i += 1

    }

  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method

    if(numTasks == 1){

      blur(src, dst, 0, src.width, radius)

    }else{

      if ((numTasks >= src.width)) {

        var i = 0

        while (i < src.width)
        {
          task {
            blur(src, dst, i, i + 1, radius)
          }

          i += 1
        }

      }else {

        val r1 = 0 to src.width by numTasks
        val r2 = r1.updated(r1.length - 1, src.width)
        val r3 = r2.zip(r2.tail)

        r3.foreach { case (from, end) =>
          task {
            blur(src, dst, from, end, radius)
          }
        }
      }

    }


  }

}
