

/**
  * 3.1
  * */
abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}
object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def union(other: IntSet): IntSet = other
  override def toString = "."
}
class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  def incl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  }

  def union(other: IntSet): IntSet = {
    ((left union right) union other) incl elem
  }

  override def toString = "{" + left + elem + right + "}"
}
val t1 = new NonEmpty(3, Empty, Empty)
val t2 = t1 incl 4
val t3 = new NonEmpty(6, new NonEmpty(5, Empty, Empty), Empty)
val t4 = t2 union t3
Empty contains 1
(new NonEmpty(7, Empty, Empty)) contains 7

/**
  * 3.2
  * */

trait Planar {
  def height: Int
  def width: Int
  def surface = height * width
}
val x = null
val y: String = x
//val z: Int = null

if (true) 1 else false

/**
  * 3.3
  * */

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
}

def nth[T](n: Int, xs: List[T]): T = {
  if (xs.isEmpty) throw new IndexOutOfBoundsException
  if (n == 0) xs.head
  else nth(n - 1, xs.tail)
}

val list = new Cons(1, new Cons(2, new Cons( 3, new Nil)))
nth(2, list)
nth(-1, list)