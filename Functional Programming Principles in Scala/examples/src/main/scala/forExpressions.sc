def isPrime(n: Int): Boolean =
  (2 until n).forall(n % _ != 0)

// for expressions
case class Person(name: String, age: Int)
val persons = List(Person("A",21), Person("B",25), Person("C",15), Person("D",35))
// get names of persons over 20 years old
def forExpGetNames(persons: List[Person]) = for p <- persons if p.age > 20 yield p.name
def funcGetNames(persons: List[Person]) = persons.filter(_.age > 20).map(_.name)

forExpGetNames(persons)
funcGetNames(persons)

// Write function that for all pairs (i,j) where 1<=i<=j<n such that i + j is a prime number
// high order function solution
def highOrderPrimePairs(n: Int) =
  (1 until n).flatMap(x =>
      (1 to x).map(y => (x,y)))
    .filter((x,y) => isPrime(x + y))
// for expression solution
def forExprPrimePairs(n: Int) =
  for
    i <- 1 until n
    j <- 1 to i
    if isPrime(i + j)
  yield (i,j)

highOrderPrimePairs(7)
forExprPrimePairs(7)

// The following does not compile
// x is the int value in the xs collection and x * x does not have .flatMap/.map for y <- x*x
//def mystery(xs: Seq[Int]) =
//  (for
//    x <- xs
//    if x % 2 == 0
//    y <- x * x
//  yield y).sum

// The following does not compile
// x * x is the int value, thee is not .sum available
//def mystery(xs: Seq[Int]) =
//  for x <- xs
//  if x % 2 == 0
//  yield (x * x).sum

// One of the correct implementation for sum of even squares
def mystery(xs: Seq[Int]) =
  val ys =
    for x <- xs
        if x % 2 == 0
    yield x * x
  ys.sum

mystery(List(1,2,3,4,5))