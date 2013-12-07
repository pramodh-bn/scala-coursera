object Test {

  def main(args: Array[String]): Unit = {
    def and(x: Boolean, y: Boolean) : Boolean = {
      if(x) y else false
    }
    
    def or(x: Boolean, y: Boolean) : Boolean = {
      if(x) true else { if(y) true else false}
    }
  }
  
 
    
}