import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object Solution {

  def readInputArrStr: Array[String] = StdIn.readLine().split(" ")
  def readInputArrInt: Array[Int] = readInputArrStr.map(_.toInt)
  def readInputStr: String = StdIn.readLine()
  def readInputInt: Int = StdIn.readInt()

  def runTestCase[T]: Try[T] = ???

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
