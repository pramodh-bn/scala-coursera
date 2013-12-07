package week3

object IntSets {
	val t1 = new NonEmpty(3, new Empty, new Empty)
                                                  //> t1  : week3.NonEmpty = {.3.}
	val t2 = t1.incl(4)                       //> t2  : week3.IntSet = {.3{.4.}}
	val t3 = new NonEmpty(1, new Empty, new Empty)
                                                  //> t3  : week3.NonEmpty = {.1.}
	val t4 = t2 union t3                      //> this is other {.1.}
                                                  //| this is left .
                                                  //| this is right {.4.}
                                                  //| this is other {.1.}
                                                  //| this is left .
                                                  //| this is right .
                                                  //| t4  : week3.IntSet = {.1{{.3.}4.}}
}

abstract class IntSet {
	def incl(x:Int): IntSet
	def contains(x: Int): Boolean
	def union(other: IntSet): IntSet
}

class Empty extends IntSet {
	def contains(x:Int): Boolean = false
	def incl(x:Int): IntSet = new NonEmpty(x, new Empty, new Empty)
	override def toString = "."
	override def union(other:IntSet): IntSet = other
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
	def contains(x:Int): Boolean =
		if(x < elem) left contains x
		else if(x > elem) right contains x
		else true
		
	def incl(x:Int): IntSet =
		if(x<elem) new NonEmpty(elem, left incl x, right)
		else if(x > elem) new NonEmpty(elem, left, right incl x)
		else this
		
	override def union(other: IntSet): IntSet = {
			println("this is other " + other)
			println("this is left " + left)
			println("this is right " + right)
			((left union right) union other) incl elem
		}
	
	override def toString = "{"+ left + elem + right + "}"
}