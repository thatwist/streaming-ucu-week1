val lb=List(1,2,3,4,5)
/*
Ex #0
Using for-loop, build the list of squares of lb*/
val squares = for {
  i <- lb
} yield i*i


// Ex #0.1
// Enhance previous by filtering out even squares, leaving only odds
// Enhance previous by println-ing every value
val squares1 = for {
  i <- lb
  sq = i*i
  if (i*i) % 2 != 0
} yield i*i


// Ex #1
// Write a function which for a given number
// returns a List of random integers of that length
def randomList(length: Int): List[Int] = {
  import scala.util.Random.nextInt
  (for {
    i <- 1 to length
  } yield nextInt).toList
}
randomList(3)


// Ex #2
// Write a function to filter a given list of integers
// using for-comprehension
def filter(
            list: List[Int],
            f: Int => Boolean): List[Int] = {
  for {
    i <- list
    if f(i)
  } yield i
}

val ints = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil
val f: Int => Boolean = _ % 2 == 0
assert(filter(ints, f) == List(2,4,6,8))

// Ex #2.1
// Using randomList - generate 10x10 table of random numbers,
// filter negatives using for comprehension and filter
// println a table
//val table = for {
//  _ <- 1 to 10
//  _ = println()
//  el <- filter(randomList(10), _ > 0)
//} {
//  print(el)
//}

/*=============================================rec*/

// Ex #3
// Write a function to sum all integers between two given numbers a and b:
// - use var
// - rewrite using recursion
def sumInts(a: Int, b: Int): Int = {
  var sum = 0
  var i = a
  while(i <= b) {
    sum += i
    i += 1
  }
  sum
}

def sumIntsR(a: Int, b: Int): Int = {
  if (a > b) 0 else a + sumInts(a + 1, b)
}


// Ex #4
// Write a function to sum the squares of all integers
// between two given numbers a and b:
def square(x: Int): Int = x * x
def sumSquares(a: Int, b: Int): Int =
  if (a > b) 0 else square(a) + sumSquares(a + 1, b)


// Ex #5
// write a common function to perform summing
def sum(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sum(f)(a + 1, b)


// Ex #6
// Write previous function in terms of map and reduce,
// converting a,b into a Range
def sum1(f: Int => Int)(a: Int, b: Int): Int =
  a.to(b).map(f).reduce((x, y) => x + y)
  //a to b map f reduce (_ + _)


/*=============================================match*/

// Ex #7
// write a function which returns last element
// of a given list of integers, using pattern matching and recursion
def last(l: List[Int]): Int = {
  l match {
    case Nil => throw new Error()
    case x :: Nil => x
    case _ :: xs => last(xs)
  }
}
last(List(1,2,3))



// Ex #8
// Write a function which returns Nth element
// using pattern matching
// you can use 'assert' to test if input arg is valid
// Re-write previous function using this one and list.length
def nTh(n: Int, l: List[Int]): Int = {
  l match {
    case Nil => throw new Error()
    case x :: Nil => if (n - 1 == 0) x else throw new Error()
    case _ :: xs => nTh(n - 1, xs)
  }
}
nTh(2, List(1,2,3))





// Ex #9
// write a function which computes a median of
// a randomly generated list (reuse function above)
// you can use list.sortWith function to sort a list
//def median(l: List[Int]): Double = {
//  val ls = l.sortWith(_ > _)
//  if (ls.length % 2 == 0) {
//    (nTh(ls.length / 2, l) + nTh(ls.length / 2 + 1, l): Double) / 2
//  } else nTh(ls.length / 2 + 1, l)
//}
//median(List(1,2,3,6))



// Ex #10
// write a function which computes a factorial
// for a given N
// - using while
// - rewrite using recursion
def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n - 1)

/*=============================================tail*/

// Ex #11
// rewrite sum function to be tail-recursive
import scala.annotation.tailrec

def sumTR(f: Int => Int)(a: Int, b: Int): Int = {
  @tailrec
  def iter(a: Int, result: Int): Int = {
    if (a == b) result + a
    else iter(a + 1, result + a)
  }
  iter(a, 0)
}
sumTR(_ * 2)(3, 5)


// Ex #12
// write a tail-recursive version of factorial
// test the speed of it
def factorialTR(n: Long): Long = {
  @tailrec
  def factorialAccumulator(acc: Long, n: Long): Long = {
    if (n == 0) acc else factorialAccumulator(n*acc, n-1)
  }
  factorialAccumulator(1, n)
}
factorialTR(1000)


/*=============================================*/

/* Home Assignment
Simple Expression parser
  1 + 2 * 3 = 7
Build an expression evaluator that works on tokens of strings composed of integers and binary operators + and * (see example)
Grammar:
  expression -> number {operator number}*
  operator -> ‘+’ | ‘*’
  number -> regex([0-9]+)

Hints:
  - use pattern matching to match operator:
    case "+" :: tail
  - use .toInt to parse number
    "1".toInt == 1
*/
def eval(l: List[String]): Int = ???

assert(eval(List("1", "+", "2", "*", "3")) == 7)
assert(eval(List("5", "*", "3", "+", "1")) == 16)
