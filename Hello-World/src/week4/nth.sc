package week4
import week4._
object nth {
	def nth[T](n:Int, xs:List[T]): T =
		if(xs.isEmpty) throw new IndexOutOfBoundsException
		else if(n == 0) return xs.head
		else nth(n-1, xs.tail)            //> nth: [T](n: Int, xs: week4.List[T])T
		
	val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
                                                  //> list  : week4.Cons[Int] = week4.Cons@70af26de
	
	nth(2, list)                              //> res0: Int = 3
}