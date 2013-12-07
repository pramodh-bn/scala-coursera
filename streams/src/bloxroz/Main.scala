package bloxroz

import com.sun.org.apache.xml.internal.serializer.ToStream

object Main extends GameDef with StringParserTerrain {
  //     val level =
  //        """ST
  //          |oo
  //         |oo""".stripMargin
  val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin
      
  def neighborsWithHistory(b:Block, history: List[Move]): Stream[(Block, List[Move])] = {
    val k = for{
      
      i <- b.legalNeighbors
      
    } yield {
      println("legal neighbor" + i._1)
      (i._1, i._2::history)
    } 
    //println("what return??" + k)
    k.toStream
  }
  
    def newNeighborsOnly(neighbors: Stream[(Block, List[Move])], explored: Set[Block]) : Stream[(Block, List[Move])] = {
       val k = for{
        i <- neighbors.toList
        if(!explored.contains(i._1))
      } yield {
        i
      }
      //println("return new " + k)
      k.toStream
  }
    
  def from(initial:Stream[(Block, List[Move])], explored: Set[Block], level: Int): Stream[(Block, List[Move])] = {
    if(initial.isEmpty){
      println("Reached termination..returning empty ")
      Stream.empty
    } 
    else {
      println("this is initial " + initial.toList)
      println("this is explored " + explored)
      println("this is the level " + level)
      val more = for{
        init <- initial
        neighbor <- {
          val k = neighborsWithHistory(init._1, init._2)
          //println("k is " + k.toList)
          k
        }
        if(!explored.contains(neighbor._1))
      } yield {
        if(explored.contains(neighbor._1)) 
          println("****************there is a path already in explored " + neighbor)
        //println("this is initially " + init)
        //println("this is neighbor " + neighbor)
        neighbor
      }
      
      //initial :: more
      println("more is " + more.toList)
      val perform = newNeighborsOnly(more, explored)
      println("explored block is " + perform.toList)
      initial #:::from(more, explored ++ more.map(_._1), level+1)
    }
    
    //Stream()
  }

  def fromNo(initial:Stream[(Block, List[Move])], explored: Set[Block], level: Int): Stream[(Block, List[Move])] = {
    if(initial.isEmpty) {
      println("yes stream is empty at level " + level)
      Stream.empty
    } else {
      val m = initial.head
      val neigh = neighborsWithHistory(m._1, m._2) filter(x => (!explored.contains(x._1)))
      println("these are the neighbors " + neigh.toList)
      neigh #:::fromNo(initial.tail, explored, level+1)
    }
    //Stream()
  }
  
  lazy val pathsToGoal: Stream[(Block, List[Move])] = {
    for{
      path <- from(Stream((startBlock, List())), Set(startBlock), 0)
      if(path._1 == Block(goal, goal))
    } yield {
      println("this is the path " + path._1)
      path
    }
    
  }
  
  lazy val solution: List[Move] = {
    println("paths " + pathsToGoal.take(1).toList )
    if(pathsToGoal.take(1).isEmpty) 
      Nil
      else
        pathsToGoal.take(1).head._2.reverse
  }
  
/*
 * What are the logic steps
 * Understand the case study from class. How from is implemented there.
 * Rephrase the from. Do you use for comprehension? or just if then else?
 * Find the test cases to test it. Very important for testing.
 * */
      
  def main(args: Array[String]): Unit = {
    println("this is terrain " + terrain(Pos(0,2)))
    //println("Legal Neighbors " + startBlock.neighbors)
    //val neighb = neighborsWithHistory(startBlock, List())
    //println("neigh " + neighb.toList)
    val explored = Set(Block(Pos(2,2),Pos(2,3)), Block(Pos(1,1),Pos(1,1)))
    val testInit = Stream((Block(Pos(1,1),Pos(1,1)),List()), (Block(Pos(1,2),Pos(1,3)),List(Right)), (Block(Pos(2,1),Pos(3,1)),List(Down)))
    //println("this is solution " + solution)
    //println(from(Stream((startBlock, List())), Set(startBlock)))
    val neigh = neighborsWithHistory(Block(Pos(1,2),Pos(1,3)), List(Right))
    println("neighb " + neigh.toList)
   // val paths = from(Stream((startBlock, List())), Set(startBlock), 0)
/*    val a = Block(Pos(4,7), Pos(4,7))
    val b = Block(goal, goal)
    if(a == b)
    	println("a is " + a + " b is " + b)
    for{
      i <- pathsToGoal
    } yield {
      println("paths " + i)
      i
    }
*/  }
}