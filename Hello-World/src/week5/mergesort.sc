package week5

object mergesort {
	def msort[T](xs:List[T])(implicit ord: Ordering[T]): List[T] = {
		val n = xs.length/2
		if(n == 0) xs
		else {
			def merge(xs:List[T], ys:List[T]): List[T] =
				(xs, ys) match {
					case (xs, Nil) => xs
					case (Nil, ys) => ys
					case (x::xs1, y::ys1) => {
						if(ord.lt(x,y)) x :: merge(xs1, ys)
						else y::merge(xs, ys1)
					}
				
				}
			val (fst, snd) = xs splitAt n
			merge(msort(fst), msort(snd))
		}
	}                                         //> msort: [T](xs: List[T])(implicit ord: Ordering[T])List[T]
	
	val b = List(4,5,3,6,2,1)                 //> b  : List[Int] = List(4, 5, 3, 6, 2, 1)
	val fruits = List("orange", "apple", "pineapple", "melon", "pear")
                                                  //> fruits  : List[String] = List(orange, apple, pineapple, melon, pear)
	msort(b)                                  //> res0: List[Int] = List(1, 2, 3, 4, 5, 6)
	msort(fruits)                             //> res1: List[String] = List(apple, melon, orange, pear, pineapple)
}