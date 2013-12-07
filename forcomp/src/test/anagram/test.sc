package test.anagram

object test {
    def getListwh(ax: List[(Char,Int)]) : List[(Char,Int)] = {
      ax match {
        case List() => List()
        case List(x) =>
          val b = for(count <- 1 to x._2) yield ((x._1, count))
          b.toList
        case x::xs =>
          val b = for(count <- 1 to x._2) yield ((x._1, count))
          b.toList:::getListwh(xs)
        
      }
    }                                             //> getListwh: (ax: List[(Char, Int)])List[(Char, Int)]
    def testMehol(ax: List[(Char,Int)]): List[List[(Char,Int)]] = {
      println("entering with " + ax)
      if(ax.isEmpty) {
        println("empty list so returning empty " + List())
        List()
      } else {
        for {
          ind <- 1 to ax.length
          elem <- getListwh(ax take ind)
          rest <- testMehol(ax drop ind)
        } yield {
          println("I am coming here " + ind +" "+ elem + " the rest " + rest)
          elem :: rest
        }
        
      }.toList
      //Set()
    }                                             //> testMehol: (ax: List[(Char, Int)])List[List[(Char, Int)]]
    
    testMehol(List(('a', 1),('b',2)))             //> entering with List((a,1), (b,2))
                                                  //| entering with List((b,2))
                                                  //| entering with List()
                                                  //| empty list so returning empty List()
                                                  //| entering with List()
                                                  //| empty list so returning empty List()
                                                  //| entering with List()
                                                  //| empty list so returning empty List()
                                                  //| entering with List()
                                                  //| empty list so returning empty List()
                                                  //| entering with List()
                                                  //| empty list so returning empty List()
                                                  //| res0: List[List[(Char, Int)]] = List()
    getListwh(List(('a', 1),('b',2)))             //> res1: List[(Char, Int)] = List((a,1), (b,1), (b,2))
}