package roundc

import scala.io.StdIn

object GameofLife {

  def readLine = StdIn.readLine()

  def main(args: Array[String]): Unit = {
    val ALIVE = 'O'
    val DEAD  = '.'
    val Array(h, w, n) = (readLine split " ").map (_.toInt)
    val alive = readLine
    println(alive)
    val aliveShouldDie: Map[Int, Boolean] = Range(0, alive.length).map(idx => {
      idx -> (alive(idx)==ALIVE)
    }).toMap
    val dead = readLine
    println(dead)
    val deadShouldRevive: Map[Int, Boolean] = Range(0, dead.length).map(idx => {
      idx -> (dead(idx)=='1')
    }).toMap
    var matrix = Map[(Int, Int), Char]()
    for(i <- 0 until h) {
      val line = readLine
      println(line)
      Range(0, w).foreach(j => {
        matrix = matrix ++ Map((i, j) -> line(j))
      })
    }

    var iterMatrix = Map[(Int, Int), Char]()
    for(_ <- 0 until n){
      for(i <- 0 until h){
        for (j <- 0 until w){
          val currIdx: (Int, Int) = (i, j)
          val neighbors: Seq[(Int, Int)] = (for{
            neighborI <- Seq(i-1, i, i+1)
            neighborJ <- Seq(j-1, j, j+1)
          }yield{
            Some((neighborI, neighborJ))
              .filter(_ != currIdx)
              .filter(_._1 >= 0)
              .filter(_._1 < h)
              .filter(_._2 >= 0)
              .filter(_._2 < w)
          }).flatten

          val numOfAliveNeighbors = neighbors.count(x => matrix(x)=='0')
          if(matrix(currIdx) == '0' && aliveShouldDie(numOfAliveNeighbors)){
            iterMatrix = iterMatrix ++ Map(currIdx -> '.')
          }else if(matrix(currIdx) == '.' && deadShouldRevive(numOfAliveNeighbors)){
            iterMatrix = iterMatrix ++ Map(currIdx -> '0')
          }else{
            iterMatrix = iterMatrix ++ Map(currIdx -> matrix(currIdx))
          }
        }
      }
      matrix = iterMatrix
      iterMatrix = Map[(Int, Int), Char]()
    }

    println(Range(0, h).map(i => {
      Range(0, w).map(j => matrix((i, j))).mkString
    }).mkString("\n"))
  }
}