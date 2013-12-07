package week4

object TweetSetScratch {
    val set1 = new Empty                          //> set1  : week4.Empty = .
    val set2 = set1.incl(new Tweet("a", "a body", 20))
                                                  //> set2  : week4.TweetSet = {.a.}
    val set3 = set2.incl(new Tweet("b", "b body", 20))
                                                  //> set3  : week4.TweetSet = {.a{.b.}}
    val c = new Tweet("c", "c body", 7)           //> c  : week4.Tweet = User: c
                                                  //| Text: c body[7]
    val d = new Tweet("d", "d body", 9)           //> d  : week4.Tweet = User: d
                                                  //| Text: d body[9]
    val set4c = set3.incl(c)                      //> set4c  : week4.TweetSet = {.a{.b{.c.}}}
    val set4d = set3.incl(d)                      //> set4d  : week4.TweetSet = {.a{.b{.d.}}}
    val set5 = set4c.incl(d)                      //> set5  : week4.TweetSet = {.a{.b{.c{.d.}}}}
		set1.filter(tw => tw.user == "a") //> calling the tweet with an empty set
                                                  //| res0: week4.TweetSet = .
		set5.filter(tw => tw.user == "a") //> calling the tweet with an empty set
                                                  //| res1: week4.TweetSet = {.a.}
    set5.filter(tw => tw.retweets == 20)          //> calling the tweet with an empty set
                                                  //| res2: week4.TweetSet = {.a.}
}



class Tweet(val user: String, val text: String, val retweets:Int) {
	override def toString: String =
	"User: " + user + "\n" +
	"Text: " + text + "[" + retweets + "]"
}

abstract class TweetSet {
	def filter(p: Tweet => Boolean): TweetSet = {
		println("calling the tweet with an empty set")
		filterAcc(p, new Empty)
	}
	def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet
	def union(that: TweetSet): TweetSet
	def mostRetweeted: Tweet
	def descendingByRetweet: TweetList
	def incl(tweet: Tweet): TweetSet
	def remove(tweet: Tweet): TweetSet
	def contains(tweet: Tweet): Boolean
	def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {
	def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc
	def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)
	def remove(tweet: Tweet): TweetSet = this
	def contains(tweet: Tweet): Boolean = false
	def foreach(f: Tweet => Unit): Unit = ()
	def union(that: TweetSet): TweetSet = that
	def mostRetweeted: Tweet = ???
	def descendingByRetweet: TweetList = ???
	override def toString: String = "."
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {
	def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet =
		if(p(elem)) acc.incl(elem)
		else {
			left.filterAcc(p, acc)
			right.filterAcc(p, acc)
		}
			
	def incl(tweet: Tweet): TweetSet = {
		if(tweet.text < elem.text)
			new NonEmpty(elem, left.incl(tweet), right)
		else if(tweet.text > elem.text)
			new NonEmpty(elem, left, right.incl(tweet))
		else this
	}
	def remove(tweet: Tweet): TweetSet = this
	def contains(tweet: Tweet): Boolean = {
		if(tweet.text < elem.text) left.contains(tweet)
		else if(tweet.text > elem.text) right.contains(tweet)
		else true
	}
	def foreach(f: Tweet => Unit): Unit = ()
	def union(that: TweetSet): TweetSet = ((left union right) union that) incl elem
	def mostRetweeted: Tweet = ???
	def descendingByRetweet: TweetList = ???
	override def toString: String = "{" + left.toString() + elem.user + right.toString() +"}"
}

trait TweetList{
}