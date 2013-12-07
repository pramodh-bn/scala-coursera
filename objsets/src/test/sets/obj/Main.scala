package test.sets.obj



object Main { //Started at around 12:30pm
		def asSet(tweets : TweetSet): Set[Tweet] = {
			var res = Set[Tweet]()
			tweets.foreach(res += _)
			res
		}                                 
	 
		def size(tweets: TweetSet): Int = asSet(tweets).size

   def main1(args: Array[String]) {
    val gizmo1 = (new Empty).incl(new Tweet("gizmodo", "Kindle Paperwhite Review: Forget Everything Else, This Is the E-Reader You Want http://t.co/737W6aNC",  51))
    val gizmo2 = gizmo1.incl(new Tweet("gizmodo", "These new Apple patents give a sneak peek at what future iPhone cameras might have in store. http://t.co/0YT9rjxp", 49))
    val gizmo3 = gizmo2.incl(new Tweet("gizmodo", "Ever wonder why the sky is dark at night? Here's your answer. http://t.co/eTKxkcaE", 86))
    val gizmo4 = gizmo3.incl(new Tweet("gizmodo", "The head of Homeland Security stays secure by just not using email, at all. http://t.co/W6KAFEUu",  37))
    val gizmo5 = gizmo4.incl(new Tweet("gizmodo", "This is how graphene will grow the flexible semiconductors of the future: http://t.co/IoEvuxp4",  43))
    val gizmo6 = gizmo5.incl(new Tweet("gizmodo", "It's the tech-based reality TV show you never knew you didn't want: http://t.co/j9J8gAo8",  19))
    val gizmo7 = gizmo6.incl(new Tweet("gizmodo", "How do you make your Steve Jobs sculpture stand out? Easy, mix in some trash you stole from him. http://t.co/mvHBj3CH",  15))
    val gizmo8 = gizmo7.incl(new Tweet("gizmodo", "This awesome baggage roller coaster will make you wish you were a suitcase. http://t.co/ECaE2hgd",  17))
    val gizmo9 = gizmo8.incl(new Tweet("gizmodo", "This price cut is putting the Kindle Paperwhite and the Nook with GlowLight head to head. Fight! http://t.co/1x6nOJGY",  15))
    val gizmo10 = gizmo9.incl(new Tweet("gizmodo", "18 unlucky people who already broke the iPhone 5: http://t.co/9RpvX4te", 79))
    
    val techCrunch1 = (new Empty).incl(new Tweet("Techcrunch", "Resignation Media Hires CEO John Ellis To Run Tapiture, Its Fast-Growing Pinterest For Men  http://t.co/ctn7oWJc by @anthonyha",  18))
    val techCrunch2 = techCrunch1.incl(new Tweet("Techcrunch","FreedomPop Opens Its Freemium Internet Service To The Masses With New Public Beta http://t.co/35mA9Adp by @chrisvelazco",  27))
    val techCrunch3 = techCrunch2.incl(new Tweet("Techcrunch","Dish And The Dream Of Internet TV http://t.co/y8KcSl8G by @ryanlawler",  25))
    val techCrunch4 = techCrunch3.incl(new Tweet("Techcrunch","Adobe's Acrobat XI Boasts New PDF Editor And Touch-Friendly Interface ? Upgrades Start At $139 http://t.co/1YDWvlVI by @anthonyha",  26))
    val techCrunch5 = techCrunch4.incl(new Tweet("Techcrunch", "Most Docks Should Work With The Lightning Adapter And iPhone 5 http://t.co/oGlTupcK by @johnbiggs",  13))
    
    val amazon1 = (new Empty).incl(new Tweet("Amazon", "Resignation Media Hires CEO John Ellis To Run Tapiture, Its Fast-Growing Pinterest For Men  http://t.co/ctn7oWJc by @anthonyha",  18))
    val amazon2 = amazon1.incl(new Tweet("Amazon","FreedomPop Opens Its Freemium Internet Service To The Masses With New Public Beta http://t.co/35mA9Adp by @chrisvelazco",  27))
    val amazon3 = amazon2.incl(new Tweet("Amazon","Dish And The Dream Of Internet TV http://t.co/y8KcSl8G by @ryanlawler",  25))
    val amazon4 = amazon3.incl(new Tweet("Amazon","Adobe's Acrobat XI Boasts New PDF Editor And Touch-Friendly Interface ? Upgrades Start At $139 http://t.co/1YDWvlVI by @anthonyha",  26))
    val amazon5 = amazon4.incl(new Tweet("Amazon", "Most Docks Should Work With The Lightning Adapter And iPhone 5 http://t.co/oGlTupcK by @johnbiggs",  13))

    //println(gizmo10)
    //println(techCrunch5)
    println((((gizmo10.union(techCrunch5))).descendingByRetweet))
    //println(techCrunch5.union(gizmo5).union(amazon5))
  
   }

  def main(args: Array[String]): Unit = {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val set21 = set1.incl(new Tweet("1", "1 body", 30))
    val set31 = set21.incl(new Tweet("2", "2 body", 31))
    val set41 = set31.incl(new Tweet("3", "3 body", 32))
    val set51 = set41.incl(new Tweet("4", "4 body", 34))
    
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val e = new Tweet("e", "e body", 30)

    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    val set6 = set5.incl(e)
    //println(set1)
    //println(set1.filter(tw => tw.user == "a"))
    //println(set5)
    //println(set5.filter(tw => tw.user == "a"))
    //println(set5.filter(tw => tw.retweets == 20))
//    println(set4c + " | " + set4d)
//    println(set4c.union(set4d))
//    println(size(set4c.union(set4d)))
//    println(set5 + " | " + set1)
//    println(set5.union(set1))
//    println(size(set5.union(set1)))
//    println(size(set1.union(set5)))
//    println(set51)
//    println(set6)
//    println(set6.union(set51))
//    println(set51.union(set6))
//    val trends = set5.descendingByRetweet
//    if(trends.head.user == "a" || trends.head.user == "b") print("yes baby pass")
      //println(TweetReader.allTweets)
     // println(TweetReader.allTweets)
//    TweetReader.allTweets.foreach(println)
    
//      var googles = new Empty
//      val google = List("Kindle", "Paperwhite")
//      google.foreach(term => googles.union(TweetReader.allTweets.filter(tweet => tweet.text.contains(term))))
//      println(size(googles))
      
    
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  
  def getMergedFilteredSets(list: List[String]): TweetSet = {
      if(!list.isEmpty)
        TweetReader.allTweets.filter(tweet=> tweet.text.contains(list.head)).union(getMergedFilteredSets(list.tail))
      else 
        new Empty
    }
  lazy val googleTweets: TweetSet = {
  //Hint: use the exists method of List and contains method of class java.lang.String  
  //TweetReader.allTweets returns an instance of TweetSet containing a set of all available tweets.
        //println(google)
      //I know that this works how do you do filter? 
     // TweetReader.allTweets.filter(tweet=> tweet.text.contains("Nexus"))
    getMergedFilteredSets(google)
 }
    
  println(googleTweets)
  println(size(googleTweets))
  //println((TweetReader.allTweets.filter(tweet=> tweet.text.contains("Nexus"))))
    
  lazy val appleTweets: TweetSet = {
  //Hint: use the exists method of List and contains method of class java.lang.String  
  //TweetReader.allTweets returns an instance of TweetSet containing a set of all available tweets.
    getMergedFilteredSets(apple)
  }
    println(size(appleTweets))
println(size(googleTweets.union(appleTweets)))
  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet
  trending.forEach(println)
  }

}