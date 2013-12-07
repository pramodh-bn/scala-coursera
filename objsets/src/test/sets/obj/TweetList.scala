package test.sets.obj

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def forEach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.forEach(f)
    }
  override def toString: String = 
    if(!isEmpty) {
      head.retweets + "," + tail
    } else "\n"
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of Empty List")
  def tail = throw new java.util.NoSuchElementException("tail of Empty List")
  def isEmpty: Boolean = true
}

class Cons(val head:Tweet, val tail: TweetList) extends TweetList {
  def isEmpty: Boolean = false
}