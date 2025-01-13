val a = List(1,2,3).zip(Vector("A","b"))

val b = 1 to 10 by 2

// List all combination of number x and y where  is drawn from 1..M
// and y is drawn from 1..N
val M = 5
val N = 3
val combinations = (1 to M).flatMap(x => (1 to N).map(y => (x, y)))

// Compute scalar product of two vectors
def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  xs.zip(ys).map(_ * _).sum
val scalProd = scalarProduct(Vector(1,2), Vector(3,4))

// Check is the number is prime
def isPrime(n: Int): Boolean =
  (2 until n).forall(n % _ != 0)
isPrime(10)
isPrime(7)

// Maps
val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)

val fruit = List("apple", "pear", "orange", "pineapple")
fruit.sortWith(_.length < _.length)
fruit.sorted

fruit.groupBy(_.head)

// Given a dictionary of words, implement encode such that
// encode(phoneNumber) produces all phrases of words
// that can serve as mnemonics for the phone number
class Coder(words: List[String]):
  val mnemonics = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
  )

  /** Maps a letter to the digit it represents */
  private val charCode: Map[Char, Char] =
//    mnemonics.flatMap((k,v) => v.map((_, k)).toMap)
    for
      (dig, letters) <- mnemonics
      letter <- letters
    yield (letter, dig)

  /** Maps a word to the digit string it can represent */
  private def wordCode(word: String): String =
    word.toUpperCase.map(charCode)

  /** Maps a digit string to all words in the dictionary that represent it */
  private val wordsForNum: Map[String, List[String]] =
    words.groupBy(wordCode).withDefaultValue(Nil)

  /** All ways to encode a number as a list of words */
  def encode(number: String): Set[List[String]] =
    if number.isEmpty then Set(List())
    else
      for
        splitPoint <- (1 to number.length).toSet
        word <- wordsForNum(number.take(splitPoint))
        rest <- encode(number.drop(splitPoint))
      yield word :: rest

















