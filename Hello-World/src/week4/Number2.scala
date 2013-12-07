package week4

trait Expr1 {
  def eval: Int
}
class Number1(n:Int) extends Expr1{
	def eval: Int = n
}

class sum1(e1: Expr1, e2: Expr1) extends Expr1{
	def eval: Int = e1.eval + e2.eval
}


