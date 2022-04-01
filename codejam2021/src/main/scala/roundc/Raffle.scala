package roundc

import scala.io.StdIn

object Raffle {
  def readInputArrStr: Array[String] = StdIn.readLine().split(" ")
  def readInputArrInt: Array[Int] = readInputArrStr.map(_.toInt)
  def readInputStr: String = StdIn.readLine()
  def readInputInt: Int = StdIn.readInt()

  def findTicketsClosest(tickets: Array[Int], raffle:  Int): Int = {
    if(raffle < tickets.head) {
      tickets.head
    }else if(raffle > tickets.last) {
      tickets.last
    }else {
      var lo = 0
      var hi = tickets.length - 1

      while (lo <= hi) {
        val mid = (hi + lo) / 2

        if (raffle < tickets(mid)) {
          hi = mid - 1
        } else if (raffle > tickets(mid)) {
          lo = mid + 1
        } else {
          return tickets(mid)
        }
      }
      if ((tickets(lo) - raffle) < (raffle - tickets(hi))){
        tickets(lo)
      } else {
        tickets(hi)
      }
    }

  }

  def getProbabilityOfWin( K: Int, firstChoice: Int, secondChoice: Int, closestElementFromPlayers: Option[Int]): Double = {
    val winNums: Int = (1 to K).count(raffle => {
      val distanceFromChoice: Int = math.min(math.abs(firstChoice - raffle), math.abs(secondChoice - raffle))
      val distanceFromOthers: Int = closestElementFromPlayers.map(x => math.abs(x - raffle)).getOrElse(Int.MaxValue)
      distanceFromChoice < distanceFromOthers
    })

    winNums.toDouble / K
  }

  def calculateProbability(tickets: Array[Int], K: Int): Double = {
    val available: Seq[Int] = (1 to K).filterNot(x => tickets.contains(x))
    lazy val ticketsSorted: Array[Int] = tickets.sorted
    if(available.isEmpty){
      0D
    }else{
      (for {
        raffleChoice <- (0 to K)
        closestPlayer = if(tickets.isEmpty){
          None
        }else{
          Some(findTicketsClosest(ticketsSorted, raffleChoice))
        }
        firstChoice <- available
        secondChoice <- available
      } yield{
        if(available.length > 1 && firstChoice >= secondChoice){
          0D
        }else{
          getProbabilityOfWin(K, firstChoice, secondChoice, closestPlayer)
        }
      }).max
    }
  }

  def main(args: Array[String]): Unit = {
    val T = readInputInt

    (0 until T).foreach(i => {
      val Array(n, k): Array[Int] = readInputArrInt
      val tickets: Array[Int] = readInputArrInt

      val res = calculateProbability(tickets, k)
      println(s"Case #${i+1}: $res")
    })


  }
}