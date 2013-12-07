package week4

trait Expr {
  def isNumber: Boolean
  def isSum:Boolean
  def numValue: Int
  def leftOp: Expr
  def rightOp: Expr
}
class Number(n:Int) extends Expr{
  def isNumber: Boolean = true
  def isSum:Boolean = false
  def numValue: Int = n
  def leftOp: Expr = throw new Error("Number left.top")
  def rightOp: Expr = throw new Error("Number right.top")

}

class sum(e1: Expr, e2: Expr) extends Expr{
  def isNumber: Boolean = false
  def isSum:Boolean = true
  def numValue: Int = throw new Error("Number expression")
  def leftOp: Expr = e1
  def rightOp: Expr = e2
  
}

object first {
  
  def eval(e: Expr): Int = {
    if(e.isInstanceOf[Number]) e.asInstanceOf[Number]numValue
    else if(e.isInstanceOf[sum]) eval(e.asInstanceOf[sum]leftOp) + eval(e.asInstanceOf[sum]rightOp)
    else throw new Error("Unknown Exception" + e)
  }
  
}