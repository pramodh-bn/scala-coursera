package week2

object test {
	val x = new Rational(1, 3)                //> x  : week2.Rational = 1/3
	val y = new Rational(5, 7)                //> y  : week2.Rational = 5/7
	val z = new Rational(3, 2)                //> z  : week2.Rational = 3/2
	x.addRational(y.neg).addRational(z.neg)   //> res0: week2.Rational = -79/42
}

class Rational(x: Int, y:Int) {
	def numer = x
	def denom = y
	
	def addRational(that:Rational): Rational = {
		new Rational(numer * that.denom + that.numer * denom, denom * that.denom)
	}
	
	def sub(that: Rational) = {
		val negthat = that.neg
		new Rational(numer * negthat.denom + negthat.numer * denom, denom * negthat.denom)
	}
	
	def neg(): Rational = {
		new Rational( -numer, denom)
	}
	override def toString() = numer + "/" + denom
}