package week6

import scala.io.Source


object x {
	val in = Source.fromFile("C:/Study/Coursera/scala/linuxwords.txt")
                                                  //> in  : scala.io.BufferedSource = non-empty iterator
  val words = in.getLines.toList filter(word => word forall (chr => chr.isLetter))
                                                  //> words  : List[String] = List(Aarhus, Aaron, Ababa, aback, abaft, abandon, ab
                                                  //| andoned, abandoning, abandonment, abandons, abase, abased, abasement, abasem
                                                  //| ents, abases, abash, abashed, abashes, abashing, abasing, abate, abated, aba
                                                  //| tement, abatements, abater, abates, abating, Abba, abbe, abbey, abbeys, abbo
                                                  //| t, abbots, Abbott, abbreviate, abbreviated, abbreviates, abbreviating, abbre
                                                  //| viation, abbreviations, Abby, abdomen, abdomens, abdominal, abduct, abducted
                                                  //| , abduction, abductions, abductor, abductors, abducts, Abe, abed, Abel, Abel
                                                  //| ian, Abelson, Aberdeen, Abernathy, aberrant, aberration, aberrations, abet, 
                                                  //| abets, abetted, abetter, abetting, abeyance, abhor, abhorred, abhorrent, abh
                                                  //| orrer, abhorring, abhors, abide, abided, abides, abiding, Abidjan, Abigail, 
                                                  //| Abilene, abilities, ability, abject, abjection, abjections, abjectly, abject
                                                  //| ness, abjure, abjured, abjures, abjuring, ablate, ablated, ablates, ablating
                                                  //| , ablation, ablative, ab
                                                  //| Output exceeds cutoff limit.
  
  val mnem = Map('2' -> "ABC",'3' -> "DEF",'4' -> "GHI",'5' -> "JKL",'6' -> "MNO",'7' -> "PQRS",'8' -> "TUV",'9' -> "WXYZ")
                                                  //> mnem  : scala.collection.immutable.Map[Char,String] = Map(8 -> TUV, 4 -> GHI
                                                  //| , 9 -> WXYZ, 5 -> JKL, 6 -> MNO, 2 -> ABC, 7 -> PQRS, 3 -> DEF)
/*Invert the mnem map to give a map from chars 'A' .. 'Z' to '2' ... '9'*/
val charCode:Map[Char, Char] =
	for((digit, str) <- mnem; ltr <- str) yield ltr -> digit
                                                  //> charCode  : Map[Char,Char] = Map(E -> 3, X -> 9, N -> 6, T -> 8, Y -> 9, J -
                                                  //| > 5, U -> 8, F -> 3, A -> 2, M -> 6, I -> 4, G -> 4, V -> 8, Q -> 7, L -> 5,
                                                  //|  B -> 2, P -> 7, C -> 2, H -> 4, W -> 9, K -> 5, R -> 7, O -> 6, D -> 3, Z -
                                                  //| > 9, S -> 7)

/* Maps a word to the digit string it can represent eg: "Java" = "5282"
*/
def wordCode(word: String): String = word.toUpperCase() map charCode
                                                  //> wordCode: (word: String)String

/**
	A map from digit strings to the words that represent them,
	eg: "5282" -> List("Java", "Kata", "Lava", ...)
	Note: A missing number should map to the empty set, eg: "1111" -> List()
*/
	val wordsForNum:Map[String, Seq[String]] = words groupBy wordCode withDefaultValue Seq()
                                                  //> wordsForNum  : Map[String,Seq[String]] = Map(63972278 -> List(newscast), 292
                                                  //| 37638427 -> List(cybernetics), 782754448 -> List(starlight), 2559464 -> List
                                                  //| (allying), 862532733 -> List(uncleared), 365692259 -> List(enjoyably), 86843
                                                  //| 7 -> List(unties), 33767833 -> List(deportee), 742533 -> List(picked), 33646
                                                  //| 46489 -> List(femininity), 3987267346279 -> List(extraordinary), 7855397 -> 
                                                  //| List(pulleys), 67846493 -> List(optimize), 4723837 -> List(grafter), 386583 
                                                  //| -> List(evolve), 78475464 -> List(Stirling), 746459 -> List(singly), 847827 
                                                  //| -> List(vistas), 546637737 -> List(lionesses), 28754283 -> List(curlicue), 8
                                                  //| 4863372658 -> List(thunderbolt), 46767833 -> List(imported), 26437464 -> Lis
                                                  //| t(angering, cohering), 8872267 -> List(turbans), 77665377 -> List(spoolers),
                                                  //|  46636233 -> List(homemade), 7446768759 -> List(rigorously), 74644647 -> Lis
                                                  //| t(ringings), 633738 -> List(offset), 847825 -> List(visual), 772832 -> List(
                                                  //| Pravda), 4729378 -> List
                                                  //| Output exceeds cutoff limit.

/* Return all ways to encode a number as a list of words */
def encode(number:String): Set[List[String]] = {
	println("entering " + number)
	if(number.isEmpty()) {
		println("number is empty so returning empty")
		Set(Nil)
	}
	else {
		for {
			split <- 1 to number.length
			word <- wordsForNum(number take split)
			rest <- encode(number drop split)
		} yield {
			println("this is " + split + " "+ word + " this is " + rest)
			word :: rest
		}
	}.toSet
	}                                         //> encode: (number: String)Set[List[String]]

 encode("722524")                                 //> entering 722524
                                                  //| entering 24
                                                  //| entering 
                                                  //| number is empty so returning empty
                                                  //| this is 2 ah this is List()
                                                  //| this is 4 pack this is List(ah)
                                                  //| entering 24
                                                  //| entering 
                                                  //| number is empty so returning empty
                                                  //| this is 2 ah this is List()
                                                  //| this is 4 rack this is List(ah)
                                                  //| entering 24
                                                  //| entering 
                                                  //| number is empty so returning empty
                                                  //| this is 2 ah this is List()
                                                  //| this is 4 sack this is List(ah)
                                                  //| entering 4
                                                  //| res0: Set[List[String]] = Set(List(pack, ah), List(rack, ah), List(sack, ah
                                                  //| ))
 wordsForNum("7225")                              //> res1: Seq[String] = List(pack, rack, sack)
                                                  
	def translate(number: String): Set[String] =
		encode(number) map (_ mkString " ")
                                                  //> translate: (number: String)Set[String]
}