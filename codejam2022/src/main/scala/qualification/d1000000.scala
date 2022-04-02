package qualification

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object d1000000 {

  def readInputArrStr: Array[String] = StdIn.readLine().split(" ")
  def readInputArrInt: Array[Int] = readInputArrStr.map(_.toInt)
  def readInputStr: String = StdIn.readLine()
  def readInputInt: Int = StdIn.readInt()

  def runTestCase: Try[String] = Try {
    val N = readInputInt
    val dice = readInputArrInt.sorted

    var straight = 0
    var idx = 0
    while (idx < dice.length) {
      val currDie = dice(idx)
      if(straight+1 <= currDie){
        straight += 1
      }
      idx += 1
    }

    straight.toString
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
