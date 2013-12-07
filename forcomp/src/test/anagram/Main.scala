package test.anagram
import anagram._
import Angrms._
object Main {
  
  def combineList(xs: List[(Char, Int)]) : List[(Char, Int)] = {
    val kl = for{
      lst <- xs
    } yield(lst) 
    println(kl)
    val a = List(('a', 2), ('e', 3), ('t', 1))
    val hope = for {
      i <- 1 to a.length
      k <- a take i
      count <- 1 to k._2
    } yield { 
        println(i + " element " + (k._1, count) + " drop " + (a drop i)) 
    	((k._1, count)) :: (a drop i)
    }
    def getList(ax: List[(Char,Int)]) : List[(Char,Int)] = {
      ax match {
        case List() => List()
        case x::xs =>           
          val b = for(count <- 1 to x._2) yield ((x._1, count)) 
          b.toList ::: getList(xs)
      }
    }
    def testMe(ax: List[(Char,Int)]): List[List[(Char,Int)]] = {
      println("entering with " + ax)
      if(ax.isEmpty) {
        println("empty list so returning empty")
        List(Nil) 
      } else {
        for {
          ind <- 1 to ax.length
          elem <- getList(ax take ind)
          rest <- testMe(ax drop ind)
        } yield {
          println("why I am not coming here" + elem)
          elem :: rest
        }
        
      }.toList ::: List(Nil)
      //Set()
    }
    val k = testMe(List(('a', 2),('b',1),('c',2)))
    
    println(k)
    
    println(hope)
    Nil
  }
def powerset[A](s: Set[A]) = s.foldLeft(Set(Set.empty[A])) { case (ss, el) => ss ++ ss.map(_ + el) }

  def subtract1(x: Occurrences, y: Occurrences): Occurrences = {
    println("this is x " + x)
    def functionFold(x1: Map[Char, List[(Char, Int)]], term: (Char, List[(Char,Int)])): Map[Char, List[(Char, Int)]] = {
      println("this is x1 " + x1)
      println("this is the term " + term)
      val (character, count) = term
      println("this is character " + character + " and this is " + count)
      if(x1(character).isEmpty) x1
      else {
        if(x1(character).head._2 == count.head._2) {
          println("they are all equal alright drop it now")
          x1 - character
        } else {
          println(" Trying to minus ")
          x1.updated(character, List((character, x1(character).head._2 - count.head._2)))
        }
      }
      //Map()
    }
    val mapx = x.groupBy(_._1) withDefaultValue(List())
    val mapy = y.groupBy(_._1) withDefaultValue(List())
    val m = (mapy foldLeft(mapx))(functionFold)
    val k = m.unzip._2.flatten
    println(k)
    k.toList.sorted
  }
    def combinationsAsList(occurrences: Occurrences): List[Occurrences] = {
      def getAllPossibleCombo(ax: List[(Char, Int)]): List[(Char, Int)] = {
        ax match {
          case List() => List()
          case x :: xs =>
            val b = for (count <- 1 to x._2) yield ((x._1, count))
            b.toList ::: getAllPossibleCombo(xs)
        }
      }
      //println("entering with " + occurrences)
      if (occurrences.isEmpty) {
        println("empty list so returning empty " + occurrences)
        List(List())
      } else {
        for {
          ind <- 1 to occurrences.length
          elem <- {
            val k = getAllPossibleCombo(occurrences take ind)
            println("All possible combo for " + (occurrences take ind) + " " + k)
            k
          }
          rest <- {
            val z = combinationsAsList(occurrences drop ind)
            println("All rest " + (occurrences drop ind) + " " + z)
            z
          }
        } yield {
          println("I am coming to yield with elem " + elem)
          println("rest in yield " + rest)
          elem :: rest
        }

      }.toSet.toList
    }
    
    def wordOccurrences(w: Word): Occurrences = {
      println("this is the grouping " + w.toLowerCase.groupBy(c=>c))
      w.toLowerCase.groupBy(c=>c).mapValues(_.size).toList.sorted
  }

  def main(args: Array[String]): Unit = {
    //println(wordOccurrences("whatsupwiththat"))
    println(wordOccurrences("Robert"))
    //println((sentenceOccurrences(List("Robert", "was", "here"))))
    //println(combineList(List(List(('b',1), ('e',1), ('o',1), ('r',2), ('t',1)), List(('a',1), ('s',1), ('w',1)), List(('e',2), ('h',1), ('r',1)))))
    //println(sentenceOccurrences(List("abcd", "e")))
    //println(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet))
    //println(combinations(List(('a', 2), ('e', 3), ('t', 1))))
    //println("List" + combinationsAsList(List(('a', 2),('e',1))))
    //println(combinations(List(('a', 2),('e',1))))
    //println("Empty " + combinationsAsList(List()))
    //println(combinations(Nil))
//    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 3))
//    val r = List(('r', 1))
//    println(subtract(lard, r))
    //println(sentenceAnagrams(List("Linux", "Rulez")))
    //println(combinations(Nil))
  }
  
}