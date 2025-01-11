// Write a production function that calculates the product of the values of a function
// for the points on a given interval [a, b]
def product(f: Int => Int)(a: Int, b: Int): Int =
  if a > b then 1
  else f(a) * product(f)(a+1, b)
// type of product is (Int => Int) => (Int, Int) => Int

product(x => x*x)(1, 5)

// Write a production function that calculates the factorial of a number
def factorial(n: Int): Int = product(x => x)(1, n)
factorial(5)

// Generalize both product and factorial
def productG(f: Int => Int)(a: Int, b: Int): Int =
  product(f)(a,b)

def factorialG(n: Int): Int =
  factorial(n)

productG(x => x*x)(1, 5)
factorialG(5)


//