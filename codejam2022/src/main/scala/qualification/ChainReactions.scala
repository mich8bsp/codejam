package qualification

import scala.collection.mutable
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object ChainReactions {

  def readInputArrStr: Array[String] = StdIn.readLine().split(" ")

  def readInputArrInt: Array[Int] = readInputArrStr.map(_.toInt)

  def readInputStr: String = StdIn.readLine()

  def readInputInt: Int = StdIn.readInt()

  class Node(val F: Long) {
    var isEnqueued: Boolean = false
    var pointingTo: Option[Node] = None
    var pointedBy: mutable.Buffer[Node] = mutable.Buffer[Node]()
    var maxInChainUpToCurrent: Option[Long] = None

    def pointTo(target: Node): Unit = {
      pointingTo = Some(target)
      target.pointedBy.append(this)
    }
  }

  def runTestCase: Try[Long] = Try {
    def initializeNodes: Array[Node] = {
      val N = readInputInt
      val nodes = readInputArrInt.map(f => new Node(f))
      readInputArrInt.zipWithIndex.foreach {
        case (p, i) => if (p > 0) {
          nodes(i).pointTo(nodes(p - 1))
        }
      }

      nodes
    }

    def calcFunForNodes(nodes: Array[Node]): Long = {
      val queue = mutable.Queue[Node]()

      def enqueue(x: Node) = {
        if(!x.isEnqueued){
          x.isEnqueued = true
          queue.enqueue(x)
        }
      }

      def dequeue(): Node = {
        val x = queue.dequeue()
        x.isEnqueued = false
        x
      }

      val initiators = nodes.filter(_.pointedBy.isEmpty)
      initiators.foreach(enqueue)

      var sumOfChainsFun: Long = 0
      while(queue.nonEmpty){
        val curr = dequeue()
        if (curr.pointedBy.size <= 1) {
          curr.maxInChainUpToCurrent = Some(math.max(curr.F, curr.pointedBy.headOption.map(_.maxInChainUpToCurrent.get).getOrElse(0L)))
          curr.pointingTo match {
            case None => sumOfChainsFun += curr.maxInChainUpToCurrent.getOrElse(0L)
            case Some(x) => enqueue(x)
          }
        } else if(curr.pointedBy.forall(_.maxInChainUpToCurrent.isDefined)) {
          val minPointingToCurrent = curr.pointedBy.map(_.maxInChainUpToCurrent.get).min
          sumOfChainsFun += (curr.pointedBy.map(_.maxInChainUpToCurrent.get).sum - minPointingToCurrent)
          curr.maxInChainUpToCurrent = Some(math.max(curr.F, minPointingToCurrent))
          curr.pointingTo match {
            case None => sumOfChainsFun += curr.maxInChainUpToCurrent.getOrElse(0L)
            case Some(x) => enqueue(x)
          }
        } else {
          enqueue(curr)
        }
      }

      sumOfChainsFun
    }

    val nodes = initializeNodes
    calcFunForNodes(nodes)
  }

  def main(args: Array[String]): Unit = {
    val T: Int = readInputInt
    val totalRes = (1 to T).flatMap(testIdx => {

      val testCaseRes: Try[Long] = runTestCase
      testCaseRes match {
        case Success(v) => List(s"Case #$testIdx: $v")
        case Failure(_) => List(s"Case #$testIdx: IMPOSSIBLE")
      }
    })

    totalRes.foreach(println)
  }

}
