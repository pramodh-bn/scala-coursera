package test.sets.obj

abstract class TweetSet {
	def filter(p: Tweet => Boolean): TweetSet = {
		//println("calling the tweet with an empty set")
		filterAcc(p, new Empty)
	}
	def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet
	def union(that: TweetSet): TweetSet
	def mostRetweeted: Tweet 
	def descendingByRetweet: TweetList = descendingByRetweetAcc(Nil)
	def incl(tweet: Tweet): TweetSet
	def remove(tweet: Tweet): TweetSet
	def contains(tweet: Tweet): Boolean
	def foreach(f: Tweet => Unit): Unit
	def mostRetweetedAcc(tweet: Tweet): Tweet 
	def descendingByRetweetAcc(tweets:TweetList): TweetList
}