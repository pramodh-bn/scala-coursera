package bloxroz

trait GameDef {
  case class Pos(x:Int, y:Int){
    def dx(d: Int) = copy(x = x+d)
    def dy(d: Int) = copy(y = y+d)
  }
  
  val startPos: Pos
  val goal: Pos
  
  type Terrain = Pos => Boolean
  
  val terrain: Terrain
  
  sealed abstract class Move
  case object Left  extends Move
  case object Right extends Move
  case object Up    extends Move
  case object Down  extends Move
  
  def startBlock: Block = Block(startPos, startPos)
  
  case class Block(b1: Pos, b2:Pos) {
    require(b1.x <= b2.x && b1.y <= b2.y, "Invalid block position: b1 =" + b1 + ", b2=" + b2)
    def dx(d1: Int, d2: Int) = Block(b1.dx(d1), b2.dx(d2))
    def dy(d1: Int, d2: Int) = Block(b1.dy(d1), b2.dy(d2))

    def left = if(isStanding)        dy(-2, -1)
    		   else if(b1.x == b2.x) dy(-1, -2)
    		   else                  dy(-1, -1)
    
    def right = if(isStanding)        dy(1, 2)
    			else if(b1.x == b2.x) dy(2, 1)
    			else                  dy(1, 1)
    			
    def up = if(isStanding)           dx(-2, -1)
    			else if(b1.x == b2.x) dx(-1, -1)
    			else                  dx(-1, -2)
    			
    def down = if(isStanding)         dx(1, 2)
    			else if(b1.x == b2.x) dx(1, 1)
    			else                  dx(2, 1)

    def isStanding: Boolean = if(b1.x == b2.x && b1.y == b2.y) true else false
    
    def neighbors: List[(Block, Move)] = List((this.left, Left),(this.right, Right),(this.up, Up), (this.down, Down))
    
    def legalNeighbors: List[(Block, Move)] = this.neighbors.filter(p => p._1.isLegal) 
      
/*    def legalNeighbors1: List[(Block, Move)] = {
      val left = if(this.left.isLegal) (this.left, Left)::Nil else Nil
      val right = if(this.right.isLegal) (this.right, Right) :: left else left
      val top = if(this.up.isLegal) (this.up, Up)::right else right
      val down = if(this.down.isLegal) (this.down, Down)::top else top
      down
    }
*/    
    def isLegal: Boolean = if(terrain(b1) && terrain(b2)){ 
      println("b1 is " + b1 + terrain(b1))
      println("b2 is " + b2 + (terrain(b2)))
      println("returning true ")
      true 
    } else {
      println("returning false ")
      false 
    }
  }
  

}