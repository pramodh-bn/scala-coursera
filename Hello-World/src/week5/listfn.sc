package week5

object listfn {
	val nums = List(4,5,3,6,2,1)              //> nums  : List[Int] = List(4, 5, 3, 6, 2, 1)
	val fruits = List("orange", "apple", "pineapple", "melon", "pear")
                                                  //> fruits  : List[String] = List(orange, apple, pineapple, melon, pear)
	nums filter(x => x>1)                     //> res0: List[Int] = List(4, 5, 3, 6, 2)
	
	nums filterNot(x => x>1)                  //> res1: List[Int] = List(1)
	
	nums partition(x=> x>1)                   //> res2: (List[Int], List[Int]) = (List(4, 5, 3, 6, 2),List(1))
	
	nums takeWhile(x => x > 3)                //> res3: List[Int] = List(4, 5)
	
	nums dropWhile(x => x > 3)                //> res4: List[Int] = List(3, 6, 2, 1)
	
	nums span(x => x > 1)                     //> res5: (List[Int], List[Int]) = (List(4, 5, 3, 6, 2),List(1))
}