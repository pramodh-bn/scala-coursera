package test.sets.obj

class Empty extends TweetSet {
	def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
	  //println("Hitting a Null, returning acc " + acc)
	  acc
	}
	def incl(tweet: Tweet): TweetSet = {
	  //println("Adding the tweet to empty" + tweet)
	  new NonEmpty(tweet, new Empty, new Empty)
	}
	def remove(tweet: Tweet): TweetSet = this
	def contains(tweet: Tweet): Boolean = false
	def foreach(f: Tweet => Unit): Unit = ()
	def union(that: TweetSet): TweetSet = that
	def mostRetweeted: Tweet = throw new NoSuchElementException("No tweet exists")
	def mostRetweetedAcc(tweet: Tweet): Tweet = tweet
	def descendingByRetweetAcc(tweets:TweetList): TweetList = tweets
	override def toString: String = "."
}