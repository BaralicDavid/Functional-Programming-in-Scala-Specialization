trait Generator[+T]:
 def generate(): T
//   def map[U](f: T => U): Generator[U]
//   def flatMap[U](f: T => Generator[U]): Generator[U]
//   def withFilter(p: T => Boolean): Generator[U]

extension [T,U](g: Generator[T])
 def map(f: T => U) = new Generator[U]:
   override def generate(): U = f(g.generate())

 def flatMap(f: T => Generator[U]) = new Generator[U]:
   override def generate(): U = f(g.generate()).generate()

object Generator:
 def pairs[T,U](t: Generator[T], u: Generator[U]) =
    for x <- t; y <- u yield (x,y)

// example of usage
val integers: Generator[Int] = new Generator[Int]:
 val rand = java.util.Random()
 override def generate(): Int = rand.nextInt()

//val booleans = for x <- integers yield x > 0

// val booleans = new Generator[Boolean]:
//   override def generate(): Boolean = ((x:Int) => x > 0)(integers.generate())

val booleans = new Generator[Boolean]:
 override def generate(): Boolean = integers.generate() > 0

// Create a list generator
//def lists =
//  for
//    ifEmpty <- booleans
//    list <- if ifEmpty then emptyLists else nonEmptyLists
//  yield list
//
//def emptyLists = new Generator[List[Int]]:
//  override def generate(): List[Int] = Nil
//def nonEmptyLists: Generator[List[Int]] =
//    for
//      head <- integers
//      tail <- lists
//    yield head :: tail
//
//lists.generate()

// Create a tree generator
sealed trait Tree
case class Leaf(x: Int) extends Tree
case class Inner(left: Tree, right: Tree) extends Tree

def leaf = new Generator[Tree]:
  override def generate(): Tree = Leaf(integers.generate())
def inner: Generator[Inner] =
  for
    x <- trees
    y <- trees
  yield Inner(x, y)

def trees =
  for
    isLeaf <- booleans
    tree <- if isLeaf then leaf else inner
  yield tree

trees.generate()



