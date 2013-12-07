object factor {

  def factorial(n: Int): Int = {
    def factor(n: Int, accum: Int): Int = {
      if (n == 0) accum
      else factor(n - 1, accum * n)
    }
    factor(n, 1)
  }                                               //> factorial: (n: Int)Int

  factorial(6)                                    //> res0: Int = 720
  
  def max(xs: List[Int]): Int = {
    val firstElement = xs.head
    val nextElement = (xs.tail).head
    def maximum(maxVal: Int, xs1: List[Int]): Int = {
      if(xs1.isEmpty) maxVal
      else if(maxVal > xs1.head) {
        maximum(maxVal, xs1.tail)
      } else
      {
        maximum(xs1.head, xs1.tail)
      }
      
    }
    if(!xs.isEmpty)
    	maximum(0, xs)
    else
      throw new NoSuchElementException
  }                                               //> max: (xs: List[Int])Int
  max(List(200,10,20,10000,8,100, 30))            //> res1: Int = 10000
  
}