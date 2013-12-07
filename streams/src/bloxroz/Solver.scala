package bloxroz

trait Solver extends GameDef {

  def done(b: Block): Boolean = (b.b1 == goal && b.b2 == goal)

  def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] = {
    val k = for {

      i <- b.legalNeighbors

    } yield {
      println(i._1)
      (i._1, i._2 :: history)
    }
    //println("what return??" + k)
    k.toStream
  }

  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])], explored: Set[Block]): Stream[(Block, List[Move])] = {
    val k = for {
      i <- neighbors.toList
      if (!explored.contains(i._1))
    } yield {
      i
    }
    println("return new " + k)
    k.toStream
  }
  
  def from(initial:Stream[(Block, List[Move])], explored: Set[Block]): Stream[(Block, List[Move])] = {
    if(initial.isEmpty){
      println("Reached termination..returning empty ")
      Stream.empty
    } 
    else {
      println("this is initial " + initial.toList)
      println("this is explored " + explored)
      //println("this is the level " + level)
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
      initial #:::from(more, explored ++ more.map(_._1))
    }
    
    //Stream()
  }

  lazy val pathsFromStart: Stream[(Block, List[Move])] = from(Stream((startBlock, Nil)),Set(startBlock))
  
  lazy val pathsToGoal: Stream[(Block, List[Move])] = {
    for{
      path <- pathsFromStart
      if(done(path._1))
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

  
}