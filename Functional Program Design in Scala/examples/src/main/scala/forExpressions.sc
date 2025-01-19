def isPrime(n: Int): Boolean =
  (2 until n).forall(n % _ != 0)

def forExprPrimeTriples(n: Int) =
  for
    i <- 1 until n
    j <- 1 to i
    k <- 1 to j
    if isPrime(i + j + k)
  yield (i,j,k)

// create function that does the same logic as forExprPrimeTriples but without for expression
def primeTriples(n: Int) =
  (1 until n).flatMap(i =>
    (1 to i).flatMap(j =>
      (1 to j)
        .withFilter(k => isPrime(i + j + k))
        .map( k => (i, j, k))
    )
  )

forExprPrimeTriples(30)
primeTriples(30)
