package test.sets.obj

import test.sets.obj._


object TestScratch {
		def asSet(tweets : TweetSet): Set[Tweet] = {
			var res = Set[Tweet]()
			tweets.foreach(res += _)
			res
		}                                 //> asSet: (tweets: test.sets.obj.TweetSet)Set[test.sets.obj.Tweet]
	 
		def size(tweets: TweetSet): Int = asSet(tweets).size
                                                  //> size: (tweets: test.sets.obj.TweetSet)Int

    val set1 = new Empty                          //> set1  : test.sets.obj.Empty = .
    val set2 = set1.incl(new Tweet("a", "a body", 20))
                                                  //> set2  : test.sets.obj.TweetSet = {.a.}
    val set3 = set2.incl(new Tweet("b", "b body", 20))
                                                  //> set3  : test.sets.obj.TweetSet = {.a{.b.}}
    val c = new Tweet("c", "c body", 7)           //> c  : test.sets.obj.Tweet = User: c
                                                  //| Text: c body [7]
    val d = new Tweet("d", "d body", 9)           //> d  : test.sets.obj.Tweet = User: d
                                                  //| Text: d body [9]
    val set4c = set3.incl(c)                      //> set4c  : test.sets.obj.TweetSet = {.a{.b{.c.}}}
    val set4d = set3.incl(d)                      //> set4d  : test.sets.obj.TweetSet = {.a{.b{.d.}}}
    val set5 = set4c.incl(d)                      //> set5  : test.sets.obj.TweetSet = {.a{.b{.c{.d.}}}}
		set1.filter(tw => tw.user == "a") //> calling the tweet with an empty set
                                                  //| Hitting a Null, returning acc .
                                                  //| res0: test.sets.obj.TweetSet = .
    set5.filter(tw => tw.user == "a")             //> calling the tweet with an empty set
                                                  //|  the function does match so adding to acca
                                                  //| Hitting a Null, returning acc .
                                                  //|  the function doesn't match so not adding to accb
                                                  //| Hitting a Null, returning acc .
                                                  //|  the function doesn't match so not adding to accc
                                                  //| Hitting a Null, returning acc .
                                                  //|  the function doesn't match so not adding to accd
                                                  //| Hitting a Null, returning acc .
                                                  //| Hitting a Null, returning acc .
                                                  //| res1: test.sets.obj.TweetSet = .
	//	size(set5.filter(tw => tw.user == "a"))
   // set5.filter(tw => tw.retweets == 20)
    //size(set5.filter(tw => tw.retweets == 20))
}