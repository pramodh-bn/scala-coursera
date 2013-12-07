package test.sets.obj



class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {
	def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
	  // What is going on here? Accumulator is not going being populated as expected. 
		if(p(elem)) {
//			println("What is in FilterAcc " + this)
//			println("The Acc is " + acc)
//			println("Trying to insert " + elem.user+elem.retweets)
//			println("going left of " + elem.user+elem.retweets)
			right.filterAcc(p, left.filterAcc(p, acc.incl(elem)))
			//println("going right " + elem.user+elem.retweets)
			//right.filterAcc(p, acc)
			//new NonEmpty(elem, left.filterAcc(p, acc), right)
		}
		else {
//			left.filterAcc(p, acc)
//			right.filterAcc(p, acc)
			right.filterAcc(p, left.filterAcc(p, acc))
		}
		  
	}
			
	def incl(tweet: Tweet): TweetSet = {
		if(tweet.text < elem.text)
			new NonEmpty(elem, left.incl(tweet), right)
		else if(tweet.text > elem.text)
			new NonEmpty(elem, left, right.incl(tweet))
		else this
	}
	def remove(tweet: Tweet): TweetSet = {
	  if(tweet.text < elem.text) new NonEmpty(elem, left.remove(tweet), right)
	  else if(tweet.text > elem.text) new NonEmpty(elem, left, right.remove(tweet))
	  else left.union(right)
	}
	def contains(tweet: Tweet): Boolean = {
		if(tweet.text < elem.text) left.contains(tweet)
		else if(tweet.text > elem.text) right.contains(tweet)
		else true
	}
	def foreach(f: Tweet => Unit): Unit = {
	  f(elem)
	  left.foreach(f)
	  right.foreach(f)
	}
	def union(that: TweetSet): TweetSet = {
	  filterAcc(elem=>true, that)
	}
	def union1(that: TweetSet): TweetSet = {
	  println("trying to combine" + this)
	  println("to that " + that)
	  ((left union right) union that) incl elem
	}
	def mostRetweeted: Tweet = {
	  mostRetweetedAcc(elem)
	}
	def mostRetweetedAcc(tweet: Tweet): Tweet = {
	  if(elem.retweets > tweet.retweets) {
	    right.mostRetweetedAcc(left.mostRetweetedAcc(elem))
	  } else {
	    right.mostRetweetedAcc(left.mostRetweetedAcc(tweet)
)
	  }
	}
	def descendingByRetweetAcc(tweets:TweetList): TweetList = {
	    val mostRetweet = mostRetweeted 
	    new Cons(mostRetweet, this.remove(mostRetweet).descendingByRetweetAcc(tweets))
	}
	override def toString: String = "{" + left.toString() + elem.user+elem.text + right.toString() +"}"

}