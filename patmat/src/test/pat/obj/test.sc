package test.pat.obj

object test {
  def insertOrder(x:(Char, Int), xs: List[(Char, Int)]): List[(Char, Int)] = xs match {
    case List() => {
    	x::Nil
    }
    case y :: ys => {
      if (x._2 >= y._2) {
        //println(x + " is equal to " + y._1)
        x::xs
      } else {
        //println("didn't find header matching going further in")
        y :: insertOrder(x, ys)
      }
    }
  }                                               //> insertOrder: (x: (Char, Int), xs: List[(Char, Int)])List[(Char, Int)]

  def makeLeafList(unorderedlist: List[(Char, Int)]) : List[(Char, Int)] = unorderedlist match{
    case List() => Nil
    case y::ys => insertOrder(y, makeLeafList(ys))
  }                                               //> makeLeafList: (unorderedlist: List[(Char, Int)])List[(Char, Int)]
	
	val x = List(('a',2),('b',3),('c',1),('d',4),('e',5))
                                                  //> x  : List[(Char, Int)] = List((a,2), (b,3), (c,1), (d,4), (e,5))
	makeLeafList(x)                           //> res0: List[(Char, Int)] = List((e,5), (d,4), (b,3), (a,2), (c,1))
}