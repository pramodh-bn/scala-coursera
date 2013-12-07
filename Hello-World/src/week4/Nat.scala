package week4

//peano numbers

abstract class Nat {
	def isZero: Boolean
	def predecessor: Nat
	def successor: Nat
	def + (that: Nat): Nat
	def - (that: Nat): Nat
}

object zero extends Nat {
	def isZero: Boolean = true
	def predecessor = throw new Error("doesn't exist")
	def successor: Nat = new Succ(this)
	def + (that:Nat) : Nat = that
	def - (that:Nat) : Nat = if(that.isZero) this else throw new Error("Negative Number")
}

class Succ(n:Nat) extends Nat{
	def isZero: Boolean = false
	def predecessor: Nat = n
	def successor: Nat = new Succ(this)
	def + (that:Nat): Nat = new Succ(n + that)
	def - (that: Nat): Nat = if(that.isZero) this else n - that.predecessor
}