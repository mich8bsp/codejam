package roundc
import scala.collection.mutable
import scala.io.StdIn

object RoaringYears {
  def readInputArrStr: Array[String] = StdIn.readLine().split(" ")
  def readInputArrInt: Array[Int] = readInputArrStr.map(_.toInt)
  def readInputStr: String = StdIn.readLine()
  def readInputInt: Int = StdIn.readInt()

  def roaringYearsStream(maxNumDigits: Int = 2): Stream[Int] = {
    val bfr: mutable.Buffer[Int] = mutable.Buffer[Int]()

    (1 to (maxNumDigits.toDouble / 2).ceil.toInt).foreach(i => {
      (math.pow(10, i-1).toInt until math.pow(10, (i+1)).toInt).map(num => {
        var reached = false
        var counter = 1
        while(!reached){
          val currToCheck = (0 to counter).map(x => num+x).mkString("")
          if(currToCheck.length<=maxNumDigits){
            bfr.append(currToCheck.toInt)
            counter += 1
          }else{
            reached = true
          }
        }
      })
    })

    bfr.sorted.toStream #::: roaringYearsStream(maxNumDigits+1)
  }

  def getNextY(currY: Int): Int = {
    roaringYearsStream(currY.toString.length).filter(_ > currY).head
  }

  def main(args: Array[String]): Unit = {
    val T = readInputInt
    (0 until T).foreach(i => {
      val Y = readInputInt
      val nextY = getNextY(Y)
      println(s"Case #${i+1}: ${nextY}")
    })
  }
}