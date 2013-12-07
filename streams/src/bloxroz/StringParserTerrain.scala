package bloxroz

trait StringParserTerrain extends GameDef {

  val level: String

  def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = {
    def pos(initPos: Pos) = {
      //println("am I coming here???")
      val s = for {
        i <- 0 until levelVector.length
        j <- 0 until levelVector(i).length
        if (initPos.x == i && initPos.y == j)
      } yield {
        //println("this is returned" + levelVector(i)(j) + "for " + initPos)
        levelVector(i)(j)
      }
      println("are we here ??? " + initPos+ " " + s + " evaluated bool "+ s.contains('-') + s.isEmpty)
      if(s.isEmpty) false
      else !s.contains('-')
     /* if(s.isEmpty) false
      else {
        //println("are we here ??? " + s.contains('-') + s.contains("-"))
        if(s.contains('-')) { 
          //println("s contains - sending false ")
          false
        }
        else {
          //println("returning true in terrainFunc ")
          true
        }
      } */
    }
    pos
  }
  
  def findChar(c:Char, levelVector: Vector[Vector[Char]]) : Pos = {
    val xPos = levelVector.indexWhere(_.indexOf(c) > -1)
    val yPos = levelVector(xPos).indexOf(c)
    //println(xPos + "  "  + yPos)
    Pos(xPos,yPos)
  }
  
  private lazy val vector: Vector[Vector[Char]] = 
    Vector(level.split("\n").map(str => Vector(str: _*)):_*)
  lazy val terrain = terrainFunction(vector)
  lazy val startPos: Pos = findChar('S', vector)
  lazy val goal:Pos = findChar('T', vector)
}