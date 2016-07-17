package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Balance")
    val answer1 = balance("(if (zero? x) max (/ 1 x))".toList)
    val answer2 = balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList)
    val answer3 = balance(":-)".toList)
    val answer4 = balance("())(".toList)
    println(answer1)
    println(answer2)
    println(answer3)
    println(answer4)

    println("Count")
    val count1 = countChange(4,List(1,2))
    println(count1)

//    println(sum(x => x * x, 3, 5))
    println(sum(x => x * x)(3, 5))
    println(product(x => x)(1, 3))
    println(factorial(5))

    println(mapReduce(x => x, (x, y) => x * y, 1)(1, 3))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) { return 1 }
    if (c == r) { return 1 }

    pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def balanced(chars: List[Char], open: Int): Boolean = {

      if (chars.isEmpty) {
        open == 0
      } else {
        if (chars.head == '(') {
          balanced(chars.tail, open + 1)
        } else if (chars.head == ')') {
          open > 0 && balanced(chars.tail, open - 1)
        } else {
          balanced(chars.tail, open)
        }
      }
    }

    balanced(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    0
  }

  /**
    * 2.1.: HOF
    */
  def sum(input: Int): Int = {
    if (input == 0) { return 0 }
    else { input + sum(input - 1) }
  }

//  def sum(f: Int => Int, a: Int, b: Int): Int = {
//    if (a > b) 0
//    else f(a) + sum(f, a + 1, b)
//  }

//  def sum(f: Int => Int, a: Int, b: Int): Int = {
//
//    def loop(a: Int, acc: Int): Int = {
//      if (a > b) acc
//      else loop(a + 1, f(a) + acc)
//    }
//    loop(a, 0)
//  }

  /**
    * 2.2.: Currying
    */
//  def sum(f: Int => Int): (Int, Int) => Int = {
//
//    def sumF(a: Int, b: Int): Int = {
//      if (a > b) 0
//      else f(a) + sumF(a + 1, b)
//    }
//
//    sumF
//  }

  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 0 else f(a) + sum(f)(a + 1, b)
  }

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1 else f(a) * product(f)(a + 1, b)
  }

  def factorial(x: Int): Int = {
    product(x => x)(1, x)
  }

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if (a > b) zero else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
  }

  /**
    * 2.3.: Finding points
    */
  
}
