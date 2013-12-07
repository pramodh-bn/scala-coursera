object worksheet {
  1 + 2                                           //> res0: Int(3) = 3
  def abs(x: Double) = if (x < 0) -x else x       //> abs: (x: Double)Double

  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double = {
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))
    }

    def isGoodEnough(guess: Double) = {
    		println(abs(guess * guess - x) / x)
    		abs(guess * guess - x)  < 0.001
    	}

    def improve(guess: Double) = (guess + x / guess) / 2

    sqrtIter(1.0)

  }                                               //> sqrt: (x: Double)Double
  14%21                                           //> res1: Int(14) = 14

  sqrt(2)                                         //> 0.5
                                                  //| 0.125
                                                  //| 0.003472222222222099
                                                  //| 3.003652441213589E-6
                                                  //| res2: Double = 1.4142156862745097
  sqrt(4)                                         //> 0.75
                                                  //| 0.5625
                                                  //| 0.05062499999999992
                                                  //| 6.098490481853958E-4
                                                  //| 9.292229696811205E-8
                                                  //| res3: Double = 2.0000000929222947


}