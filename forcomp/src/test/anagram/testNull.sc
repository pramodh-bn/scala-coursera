package test.anagram

object testNull {
def dosomething[T](x: T) = List(x);               //> dosomething: [T](x: T)List[T]

def recursiveMethod[T](xs: List[T]): List[List[T]] =
   if(xs.isEmpty)
    List(Nil)
  else {
    	dosomething(xs.head) :: recursiveMethod(xs.tail)
    }                                             //> recursiveMethod: [T](xs: List[T])List[List[T]]

def alternativeRecursion[T](xs: List[T]): List[List[T]] = xs match {
     case x :: Nil  => List(dosomething(x)):::List(Nil)
     case x :: tail => dosomething(x) :: alternativeRecursion(tail)
     case _         => throw new NoSuchElementException
  }                                               //> alternativeRecursion: [T](xs: List[T])List[List[T]]
  
println(alternativeRecursion(List(('a',1),('b',2),('c',3))))
                                                  //> List(List((a,1)), List((b,2)), List((c,3)), List())
    val k = (('a',1)) :: (List(Nil))              //> k  : List[Product with Serializable] = List((a,1), List())
    val a = Set(List("a",1)) ++ Set(List())       //> a  : scala.collection.immutable.Set[List[Any]] = Set(List(a, 1), List())
    val temp = List(List("a",1),List("a",1),List("c"))
                                                  //> temp  : List[List[Any]] = List(List(a, 1), List(a, 1), List(c))
    temp.toSet.toList                             //> res0: List[List[Any]] = List(List(a, 1), List(c))
}