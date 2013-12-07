package test.pat.obj

import test.pat.obj.HuffmanDecode._
import test.pat.obj.HuffmanTest._
object HuffmanEncode {
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def callTreeCheck(tree: CodeTree, char: Char, accum: List[Bit]): List[Bit] = {
      tree match {
        case Leaf(x, wt) => {
          println("this is chars in leaf " + x)
          println("found leaf so returning accum " + accum)
          accum
        }
        case Fork(l, r, charList, wt) => {
          if (chars(l).contains(char)) {
            println("this char might be in left so adding 0 to accum & this is chars in left " + chars(l))
            callTreeCheck(l, char, 0 :: accum)
          } else {
            println("this char might be in right so adding 1 to accum & this is chars in right " + chars(r))
            callTreeCheck(r, char, 1 :: accum)
          }
        }
      }
    }
    def encodeAccum(tree: CodeTree, text: List[Char], accum: List[Bit]): List[Bit] = {
      if (text.isEmpty) accum
      else {
        println(" calling text tail " + text.tail + " with text head " + text.head)
        encodeAccum(tree, text.tail, callTreeCheck(tree, text.head, accum))
      }
    }
    encodeAccum(tree: CodeTree, text: List[Char], Nil)
  }

  type CodeTable = List[(Char, List[Bit])]
  def codeBits(table: CodeTable)(char: Char): List[Bit] = {
    table.filter(x => x._1 == char).head._2
  }

  def convert(tree: CodeTree): CodeTable = {
    def add(i:Bit, table: CodeTable) : CodeTable = {
      def addAccum(i:Bit, table: CodeTable, accum: CodeTable): CodeTable ={
    		  if(table.isEmpty) accum
    		  else {
    		    val acc = (table.head._1,  i::table.head._2)
    		    addAccum(i, table.tail, acc::accum)
    		  }
        
      }
      addAccum(i,table,Nil)
    }
    
    def convertAccum(tree: CodeTree, splitTree:CodeTable, accum:CodeTable): CodeTable = {
      println("this is in accum for now " + accum)
      println("this is in the split Table " + splitTree)
	    tree match {
	      case Leaf(x, wt) => {
	        println("reached a leaf " + x)
	        accum:::splitTree
	      }
	      case Fork(l,r,charList, wt) => {
	        //convertAccum(r, add(1,splitCodeTable(chars(r), splitTree, Nil)), convertAccum(l, add(0,splitCodeTable(chars(l), splitTree, Nil)), accum))
	        mergeCodeTables(convertAccum(r, add(1,splitCodeTable(chars(r), splitTree, Nil)),accum), convertAccum(l, add(0,splitCodeTable(chars(l), splitTree, Nil)),accum))
	        
/*	        val treevals = chars(tree)
	        if(charList.contains(chars(l))){
	          // create the Code Table with an append
	          val tab = splitCodeTable(chars(l), splitTree, Nil)
	          println("this is what is there in after split left " + tab)
	          val ad = add(0,tab)
	          println("After adding 0 " + tab)
	          val cvert = convertAccum(l, ad, accum)
	          println("after left convert " + cvert)
	          cvert
	          
	        } else {
	          // Create the Code table for right with an append
	          //convertAccum(r, add(1,splitCodeTable(chars(r), accum, Nil)))
	          val tab = splitCodeTable(chars(r), accum, Nil)
	          println("this is what is there in after split right " + tab)
	          val ad = add(1,tab)
	          println("After adding 1 " + tab)
	          val cvert = convertAccum(r, ad, accum)
	          println("after right convert " + cvert)
	          cvert

	        }
*/	        
	      }
	    }
    }
    
    def splitCodeTable(charslist: List[Char], table: CodeTable, accum: CodeTable): CodeTable = {
      if(charslist.isEmpty) accum
      else {
        val acc = table.filter(elem => elem._1 == charslist.head)
        splitCodeTable(charslist.tail, table, acc:::accum)
      }
    }
    
    def getCodeTableAccum(tree:CodeTree, accum:CodeTable): CodeTable = {
      tree match {
        case Leaf(x, wt) => (x, Nil)::accum
        case Fork(l, r, charlist, wt) => {
          getCodeTableAccum(l, getCodeTableAccum(r, accum))
        }
        
      }
    }
    // construct empty codeTable for each character
    def reverseCodeTable(table: CodeTable): CodeTable = {
      def reverseCodeTableAccum(table:CodeTable, accum: CodeTable): CodeTable = {
        if(table.isEmpty) 
          accum
          else {
            
            reverseCodeTableAccum(table.tail, (table.head._1, table.head._2.reverse)::accum)
          } 
            
      }
      reverseCodeTableAccum(table, Nil)
    }
    
   val t = convertAccum(tree, getCodeTableAccum(tree,Nil), Nil)
   val q = reverseCodeTable(t)
   println("What is being returned might need some work " + t)
   println("What is being returned after reversing" + q)
   q
  }
  
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    a ::: b
  }
  
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def quickEncodeAccum(table: CodeTable, text: List[Char], accum:List[Bit]): List[Bit] = {
      if(text.isEmpty) 
        accum
      else {
        var acc = table.filter(p=> p._1 == text.head).head._2
        quickEncodeAccum(table, text.tail, acc:::accum)
      }
    }
    quickEncodeAccum(convert(tree), text, Nil).reverse
  }

  
}