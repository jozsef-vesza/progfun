
/**
  * 4.1
  */

// Peano numbers
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}
class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor = n
  def + (that: Nat) = new Succ(n + that)
  def - (that: Nat) = if (that.isZero) this else n - that.predecessor
}
object Zero extends Nat {
  def isZero = true
  def predecessor = throw new Error("0.predecessor")
  def + (that: Nat) = that
  def - (that: Nat) = if (that.isZero) this else throw new Error("negative number")
}
val zero = Zero
val one = zero.successor
zero == one.predecessor
val two = one.successor
two.predecessor == one
val three = one + two
three.predecessor.predecessor.predecessor == zero

/**
  * 4.2
  */

//trait List[+T] {
//  def isEmpty: Boolean
//  def head: T
//  def tail: List[T]
//}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

//class Nil[T] extends List[T] {
//  def isEmpty = true
//  def head = throw new NoSuchElementException("Nil.head")
//  def tail = throw new NoSuchElementException("Nil.tail")
//}

object List {
  def apply[T]() = new Nil
  def apply[T](x: T) = new Cons(x, new Nil)
  def apply[T](x: T, y: T) = new Cons(x, new Cons(y, new Nil))
}
val twothree = List(2, 3)
twothree.head

/**
  * 4.4
  */

// covariant list
trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]

  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

object Nil extends List[Nothing] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
}

val x: List[String] = Nil