import math.abs

/**
  * 2.4
  */
//object exercise {
//  val tolerance = 0.001
//  def isCloseEnough(x: Double, y: Double) =
//    abs((x - y) / x) / x < tolerance
//  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
//    def iterate(guess: Double): Double = {
//      val next = f(guess)
//      if (isCloseEnough(guess, next)) next
//      else iterate(next)
//    }
//    iterate(firstGuess)
//  }
//
//  fixedPoint(x => 1 + x / 2)(1)
//
//  def averageDamp(f: Double => Double)(x: Double) =
//    (x + f(x)) / 2
//
//
//  def sqrt(x: Double) =
//    fixedPoint(averageDamp(y => x / y))(1)
//
//  sqrt(2)
//}

/**
  * 2.5
  */

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def numer = x
  def denom = y

  def < (that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) = if (this< that) that else this

  def + (that: Rational) =
    new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def unary_- : Rational = new Rational(-numer, denom)

  def - (that: Rational) =
    this + -that

  override def toString = {
    val g = gcd(numer, denom)
    numer / g + "/" + denom / g
  }
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x + y
-x

val a = new Rational(2, 3)
val b = new Rational(1, 3)
a - b

x - y - z

/**
  * 2.6
  */
y + y
x < y
x max y

val rat = new Rational(2)

/**
  * 2.7
  */

