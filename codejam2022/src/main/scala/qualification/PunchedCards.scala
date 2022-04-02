package qualification

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object PunchedCards {

  def readInputArrStr: Array[String] = StdIn.readLine().split(" ")

  def readInputArrInt: Array[Int] = readInputArrStr.map(_.toInt)

  def readInputStr: String = StdIn.readLine()

  def readInputInt: Int = StdIn.readInt()

  def runTestCase: Try[String] = Try {
    val Array(rows, cols) = readInputArrInt

    (0 to rows * 2).map(row => {
      (0 to cols * 2).map(col => {
        (row, col) match {
          case (r, c) if r < 2 && c < 2 => "."
          case _ => (row % 2, col % 2) match {
            case (0, 0) => "+"
            case (0, 1) => "-"
            case (1, 0) => "|"
            case (1, 1) => "."
          }
        }
      })
    }).map(_.mkString("")).mkString("\n")
  }

  def main(args: Array[String]): Unit = {
    val T: Int = readInputInt
    val totalRes = (1 to T).flatMap(testIdx => {

      val testCaseRes: Try[String] = runTestCase
      testCaseRes match {
        case Success(v) => List(s"Case #$testIdx:", v)
        case Failure(_) => List(s"Case #$testIdx: IMPOSSIBLE")
      }
    })

    totalRes.foreach(println)
  }

}
