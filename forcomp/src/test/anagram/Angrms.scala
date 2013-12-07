package test.anagram
import anagram._

object Angrms {
  type Word = String
  type Sentence = List[Word]

  type Occurrences = List[(Char, Int)]
  val dictionary = loadDictionary

  def wordOccurrences1(w: Word): Occurrences = {
    //println("this is the word " + w)
    def pack(xs: List[Char]): List[List[Char]] = xs match {
      case Nil => Nil
      case x :: xs1 =>
        val (first, rest) = xs span (y => y == x)
        first :: pack(rest)
    }
    def encode(xs: List[Char]): List[(Char, Int)] = {
      pack(xs) map (ys => (ys.head, ys.length))
    }

    encode(w.toLowerCase().toList.sorted).sorted
  }
  def wordOccurrences(w:Word): Occurrences = {
    w.groupBy(c=>c).mapValues(_.size).toList.sorted
  }
  def sentenceOccurrences(s: Sentence): Occurrences = {
    wordOccurrences(s.foldLeft("")(_ + _))
  }

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    /*def sortfunc(a:(List[(Char, Int)], Word),b:(List[(Char, Int)], Word)): Boolean = {
      if(a._1 < b._1) {
        println(a + " is equal to " + b)
        true
      } else {
    	  println(a + " is not equal to " + b)
    	  false
      }
    }*/
    val a = for {
      word <- dictionary
    } yield (wordOccurrences(word) -> word)
    val c = a.groupBy(_._1).mapValues(_.unzip._2.sorted)
    //println(c)
    c.withDefaultValue(List())
  }

  def wordAnagrams(word: Word): List[Word] = {
    dictionaryByOccurrences(wordOccurrences(word)).toList
  }

  def combinations1(occurrences: Occurrences): List[Occurrences] = {
    def doIndividual(occurrences: Occurrences): List[Occurrences] = occurrences match {
      case List() => List(Nil)
      case x :: xs =>
        val c = for {
          i <- 1 to x._2
        } yield ((x._1, i))
        List(c.toList) ::: doIndividual(xs)

    }
    val k = for {
      element <- occurrences
      count <- 1 to element._2
    } yield (element._1, count)

    //println(doIndividual(occurrences))

    def insideCombinations(occurrences: Occurrences): Set[Occurrences] = {
      println("entering " + occurrences)
      if (occurrences.isEmpty) {
        println("Found empty So, returning empty")
        Set(List())
      } else {
        println("No empty doing the deed for " + occurrences)
        for {
          split <- 1 to occurrences.length
          elements <- occurrences take split
          count <- 1 to elements._2
          rest <- insideCombinations(occurrences drop split)
        } yield {

          println(split + " " + (elements._1, count) + " rest  " + rest)
          (elements._1, count) :: rest

        }
      }.toSet

      //Set()
    }

    println(insideCombinations(occurrences))
    Nil
  }

  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def combinationsAsSet(occurrences: Occurrences): Set[Occurrences] = {
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
        Set(occurrences)
      } else {
        for {
          ind <- 1 to occurrences.length
          elem <- {
            val k = getAllPossibleCombo(occurrences take ind)
            println("All possible combo for " + (occurrences take ind) + " " + k)
            k
          }
          rest <- {
            val z = combinationsAsSet(occurrences drop ind)
            println("All rest " + (occurrences drop ind) + " " + z)
            z
          }
        } yield {
          println("why I am not coming here " + rest)
          elem :: rest
        }

      }.toSet
    }
//    val m = combinationsAsSet(occurrences)
//    println(m)
//    m.toList
    if(combinationsAsSet(occurrences).flatten.isEmpty){
      println("any appending for empty?")
      List(Nil)
    }
    else {
        println("It has some elements still adding")
    	combinationsAsSet(occurrences).toList ::: List(Nil)
    }
  }

  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    //println("this is x " + x)
    def functionFold(x1: Map[Char, List[(Char, Int)]], term: (Char, List[(Char, Int)])): Map[Char, List[(Char, Int)]] = {
      //println("this is x1 " + x1)
      //println("this is the term " + term)
      val (character, count) = term
      //println("this is character " + character + " and this is " + count)
      if (x1(character).isEmpty) x1
      else {
        if (x1(character).head._2 == count.head._2) {
          //println("they are all equal alright drop it now")
          x1 - character
        } else {
          //println(" Trying to minus ")
          x1.updated(character, List((character, x1(character).head._2 - count.head._2)))
        }
      }
      //Map()
    }
    val mapx = x.groupBy(_._1) withDefaultValue (List())
    val mapy = y.groupBy(_._1) withDefaultValue (List())
    val m = (mapy foldLeft (mapx))(functionFold)
    val k = m.unzip._2.flatten
    //println(k)
    k.toList.sorted
  }

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def innerAnagram(occursSeq: Occurrences): List[Sentence] = {
      println("This is what goes in occursSeq " + occursSeq)
      if (occursSeq.isEmpty) {
        println("this is empty so returning Empty")
        List(Nil)
      } 
      else {
        val k = for {
          //i <- 1 to b.length
          elem <- combinations(occursSeq)
          word <- {
            val k = dictionaryByOccurrences(elem)
            println("are there any matches ?? " + k)
            k
            }
          rest <- {
            println("am I seeing word " + word)
            val m = innerAnagram(subtract(occursSeq, elem))
            println("recursive anything here?? " +m)
            m
          
          }
          //if(rest.flatten != Nil)
        } yield {
          println("are we here yet? this is in combinations " + elem)
          println("word is " + word + " and this is rest: " + rest)
          //println("this is the rest " + rest)
          //(word, rest)
          word::rest
        }
        println("this is the return " + k)
        
        k
      }
    }
    println("this is the sentence " + sentence)
    val a = sentenceOccurrences(sentence)
    println(a)
    val b = combinations(a)
    println(b)
    innerAnagram(a)
  }

}