package test.pat.obj

import scala.collection.immutable.Nil

object HuffmanTest {
  abstract class CodeTree {
    override def toString = {
      case class TreeLines(node: String, leftLines: List[String], rightLines: List[String]) {
        def prepend(nodePrefix: String, leftPrefix: String, rightPrefix: String) =
          rightLines.map(rightPrefix + _) :::
            (nodePrefix + node) ::
            leftLines.map(leftPrefix + _)
      }

      def getLines(tree: CodeTree): TreeLines =
        tree match {
          case l: Leaf => TreeLines("(" + l.char + ":" + l.weight + ")", Nil, Nil)
          case f: Fork => TreeLines(
            "(" + f.chars.sorted.mkString + ":" + f.weight + ")",
            getLines(f.left).prepend("  \\-", "   ", "  |"),
            getLines(f.right).prepend("  /-", "  |", "   "))
        }

      getLines(this).prepend("", "", "").mkString("\n")
    }
  }
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  //Section 1
  def weight(tree: CodeTree): Int = tree match {
    case Leaf(xchar, wt) => wt
    case Fork(l, r, char, wt) => weight(l) + weight(r)
  }

  def weightHeighest(tree: CodeTree): Int = tree match {
    case Leaf(xchar, wt) => wt
    case Fork(l, r, char, wt) => {
      val wtl = weightHeighest(l)
      val wtr = weightHeighest(r)
      if (wtl > wtr) wtl else wtr
    }
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Leaf(xchar, wt) => List(xchar)
    case Fork(l, r, char, wt) => chars(l) ::: chars(r)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // Section 2
  def string2Chars(str: String): List[Char] = str.toList

  def insert(x: Char, xs: List[(Char, Int)]): List[(Char, Int)] = xs match {
    case List() => {
      List((x, 1))
    }
    case y :: ys => {
      if (x == y._1) {
        //println(x + " is equal to " + y._1)
        List((y._1, y._2 + 1)) ::: ys
      } else {
        //println("didn't find header matching going further in")
        y :: insert(x, ys)
      }
    }
  }

  def times(chars: List[Char]): List[(Char, Int)] =
    chars match {
      case Nil => {
        Nil
        //println("returning empty list")
        //insert(' ', Nil)
      }
      case y :: ys => {
        //println(" calling the head " + y)
        insert(y, times(ys))
      }
    }

  def insertOrder(x: (Char, Int), xs: List[(Char, Int)]): List[(Char, Int)] = xs match {
    case List() => {
      x :: Nil
    }
    case y :: ys => {
      if (x._2 <= y._2) {
        //println(x + " is equal to " + y._1)
        x :: xs
      } else {
        //println("didn't find header matching going further in")
        y :: insertOrder(x, ys)
      }
    }
  }

  def makeLeafList1(unorderedlist: List[(Char, Int)]): List[(Char, Int)] = unorderedlist match {
    case List() => Nil
    case y :: ys => insertOrder(y, makeLeafList1(ys))
  }

  def makeLeafList(unconvlist: List[(Char, Int)]): List[Leaf] = {
    unconvlist match {
      case List() => Nil
      case y :: ys => new Leaf(y._1, y._2) :: makeOrderedLeafList(ys)
    }
  }

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    def insertOrder(x: Leaf, xs: List[Leaf]): List[Leaf] = xs match {
      case List() => {
        x :: Nil
      }
      case y :: ys => {
        if (x.weight <= y.weight) {
          //println(x + " is equal to " + y._1)
          x :: xs
        } else {
          //println("didn't find header matching going further in")
          y :: insertOrder(x, ys)
        }
      }
    }

    def makeLeafList(unconvlist: List[(Char, Int)]): List[Leaf] = {
      unconvlist match {
        case List() => Nil
        case y :: ys => new Leaf(y._1, y._2) :: makeLeafList(ys)
      }
    }
    
    
    freqs match {
      case List() => Nil
      case y :: ys => insertOrder(new Leaf(y._1, y._2), makeOrderedLeafList(ys)) //new Leaf(y._1, y._2) :: makeOrderedLeafList(ys)
    }
  }

  def singleton(trees: List[CodeTree]): Boolean = {

    if (trees.size == 1) {
      println("Tree size is 1")
      true
    } else false
  }

  def insertOrderFork(x: (CodeTree), xs: List[CodeTree]): List[CodeTree] = xs match {
    case List() => {
      x :: Nil
    }
    case y :: ys => {
      //      println(x)
      //      println("weight of x is " + weight(x) + " highest node is " + weightHeighest(x))
      //      println("weight of y is " + weight(y) + " highest node is " + weightHeighest(y))
      //      println(y)
      //      println()
      if (weight(x) <= weight(y)) {
        //        println("weight x is lesser than weight y so ordered!!")
        if (weightHeighest(x) <= weight(y))
          x :: xs
        else y :: insertOrderFork(x, xs)
      } else {
        //        println("weight x is not ordered so putting y in front!!!")
        y :: insertOrderFork(x, ys)
      }

    }
  }

  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case List() => Nil
    case x :: xs => {
      //println("here in the wild of " + x + " tail is " + xs)
      //println("trying to insert order and get a list")
      if (xs.isEmpty)
        x :: Nil
      else {
        insertOrderFork(new Fork(x, xs.head, chars(x) ::: chars(xs.head), weight(x) + weight(xs.head)), xs.tail)
        /*        if(weightHeighest(x) < weightHeighest(xs.head)) {
        println("weight of x is " + weight(x) + " highest node is " + weightHeighest(x))
        println("weight of y is " + weight(xs.head) + " highest node is " + weightHeighest(xs.head))
          insertOrderFork(new Fork(xs.head, x, chars(xs.head):::chars(x), weight(x) + weight(xs.head)), xs.tail)
        }
        else {val t1 = insertOrderFork(new Fork(x, xs.head, chars(x) ::: chars(xs.head), weight(x) + weight(xs.head)), xs.tail)
        println("this is the combined list ---" + t1)
        t1
        }*/
      }
    }
  }

  def until(isSingleTree: List[CodeTree] => Boolean, combineFunc: List[CodeTree] => List[CodeTree])(leaves: List[CodeTree]): List[CodeTree] = {
    val list = combine(leaves)
    if (isSingleTree(list))
      list
    else until(isSingleTree, combineFunc)(list)
  }

  def createCodeTree(chars: List[Char]): CodeTree = {
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head
  }

}