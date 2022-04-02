package qualification

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object Printing3D {

  def readInputArrStr: Array[String] = StdIn.readLine().split(" ")
  def readInputArrInt: Array[Int] = readInputArrStr.map(_.toInt)
  def readInputStr: String = StdIn.readLine()
  def readInputInt: Int = StdIn.readInt()

  def runTestCase: Try[String] = Try {
    val NUM_OF_PRINTERS = 3
    val NUM_OF_COLORS = 4
    val printers = (0 until NUM_OF_PRINTERS).map(_ => readInputArrInt)

    def getMaxInkUsableForColor(idx: Int): Int = printers.map(_(idx)).min

    val res = (0 until NUM_OF_COLORS).map(getMaxInkUsableForColor)
      .zipWithIndex
      .foldLeft((math.pow(10, 6).toInt, Array.fill(NUM_OF_COLORS)(0)))({
        case (currRes@(0, currColors), _) => currRes
        case ((inkLeftToUse, currColorsToUse), (currColorInk, colorIdx)) =>
          val inkToUseFromCurrColor = math.min(inkLeftToUse, currColorInk)
          currColorsToUse(colorIdx) = inkToUseFromCurrColor
          (inkLeftToUse - inkToUseFromCurrColor, currColorsToUse)
      })

    res match {
      case (0, colors) => colors.mkString(" ")
      case _ => throw new Exception()
    }
  }

  def main(args: Array[String]): Unit = {
    val T: Int = readInputInt
    val totalRes = (1 to T).flatMap(testIdx => {

      val testCaseRes: Try[String] = runTestCase
      testCaseRes match {
        case Success(v) => List(s"Case #$testIdx: $v")
        case Failure(_) => List(s"Case #$testIdx: IMPOSSIBLE")
      }
    })

    totalRes.foreach(println)
  }

}
