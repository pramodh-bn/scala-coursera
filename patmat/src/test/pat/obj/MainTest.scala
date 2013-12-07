package test.pat.obj

import test.pat.obj.HuffmanTest._
import test.pat.obj.HuffmanDecode._
import test.pat.obj.HuffmanEncode._ 


object MainTest {

  def main(args: Array[String]): Unit = {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    
    val t3 = List(('a',8),('b',3),('c',1),('d',1),('e',1),('f',1),('g',1),('h',1))
    val t4 = List(Fork(Leaf('c',1),Leaf('d',1),List('c', 'd'),2), Leaf('b',3))
    val t5 = List(('a',2), ('b',1), ('c',1))
    //println(insertOrderFork(t4.head, t4.tail))

    //println(weight(t2))
    //println(chars(t2))
    //println(makeOrderedLeafList(makeLeafList(times(string2Chars("helloworld")))))
    //println(makeOrderedLeafList(t3))
     //println(until(singleton, combine)(makeOrderedLeafList(makeLeafList(t5))))
     val t6 = createCodeTree(string2Chars("aaaaaaaabbbcdefgh"))
     //print(t6)
     //val t7 = List(1,0,0,1,1)
     //println(decode(t6,t7))
    // println(weightHeighest(t2))
    //val t4 = createCodeTree(string2Chars("aaaaaaaabbbcdefgh"))
    //println(t4)
    //val t5 = List(1,0,0,0,0,1,1,0,0)
    //println(decode(t4, t5))
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)
  //println(decode(frenchCode,secret))
  //println(frenchCode)
  val courseraCode: CodeTree = Fork(Leaf('a',8),Fork(Fork(Leaf('b',3),Fork(Leaf('c',1),Leaf('d',1),List('c','d'),2),List('b','c','d'),5),Fork(Fork(Leaf('e',1),Leaf('f',1),List('e','f'),2),Fork(Leaf('g',1),Leaf('h',1),List('g','h'),2),List('e','f','g','h'),4),List('b','c','d','e','f','g','h'),9),List('a','b','c','d','e','f','g','h'),17)
  val dlist = List(1,0,1,1)
  val baclist = List(1,0,0,0,1,0,1,0)
  //println(decode(courseraCode,dlist))
  //println(decode(courseraCode,baclist))
  //println("this is the encoded String " + encode(t6)(string2Chars("bac")))
  val myCodeTable = List(('a', List(1)),('b', List(0,0,1)),('c', List(0,0)))
  //println(codeBits(myCodeTable)('c'))
  //println(convert(t6))
  //println(quickEncode(t6)(string2Chars("abcdefgh")))
  //println(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))
  //print(t2)
  //println(encode(t2)("bd".toList))
  //println(decode(t2, quickEncode(t2)("bd".toList)))
  //println(convert(t1))
  println("quick encode output " + quickEncode(t2)("bd".toList))
  println("encode output" + encode(t2)("bd".toList))
  }

}