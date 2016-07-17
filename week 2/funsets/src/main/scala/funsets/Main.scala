package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))

  println(contains(diff(singletonSet(2), singletonSet(1)), 1))

  println(contains(filter(union(singletonSet(4), singletonSet(3)), x => x % 2 == 0), 4))

  println("forall")
  println(forall(union(singletonSet(3), singletonSet(4)), x => x % 3 == 0))

  println(exists(union(singletonSet(2), singletonSet(4)), x => x % 2 == 0))

  printSet(union(singletonSet(2), singletonSet(3)))

  println(contains(map(singletonSet(3), x => x * 2), 6))

  println(forall(union(union(union(singletonSet(1), singletonSet(3)), union(singletonSet(4), (singletonSet(5)))), union(singletonSet(7), singletonSet(1000))), x => x < 5))
}
