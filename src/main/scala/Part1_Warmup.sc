// COSC 455 - Programming Languages: Implementation and Design
// Project 2

// NAME: Kevin Pilkerton

// Warmup Question 1
// prime numbers = true, non-prime = false
def prime(i : Int) : Boolean = {
 i < 2 match {
   case true => return false
   case false => primeHelper(i, 2)
 }
}

def primeHelper(i : Int, divisor : Int) : Boolean = {
  (divisor < i, i % divisor != 0) match {
    case (true, false) => return false
    case (false, false) => return true
    case (true, true) => primeHelper(i, divisor + 1)
    case (false, true) => return false
  }
}

// test for values less than 0
println("is -2 prime?")
prime(-2)
// test 0
println("Is 0 prime?")
prime(0)
// test known primes
println("Is 1 prime?")
prime(1)

println("Testing known primes")
prime(2)
prime(3)
prime(5)
prime(7)
prime(11)

// test known non-primes
println("Testing known non-primes")
prime(4)
prime(6)
prime(8)
prime(9)
prime(10)

// Problem 2: Twin Prime
def twinprimes(num1 : Int, num2 : Int): Boolean =
{
  (prime(num1) , prime(num2)) match {
    case (false, _) => return false
    case (true, false) => return false
    case (true, true) => twinprimesHelper(num1, num2)
  }
}
def twinprimesHelper(num1 : Int, num2 : Int): Boolean = {
  math.abs(num1 - num2) match {
    case 2 => return true
    case _ => return false
  }
}
// test valid twin primes
println("Twin Primes Test")
twinprimes(41, 43)

twinprimes(43, 41)
// test non-valid twin primes
twinprimes(41, 49)

// Number 3
def twinprimeslist(n:Int): List[Int] = {
  twinprimeslisthelper(n).reverse
}
def twinprimeslisthelper(n:Int) :List[Int] = {
  n <= 3 match {
    case true => Nil
    case false => {
      (twinprimes(n, n - 2)) match {
        case true => n :: n - 2 :: twinprimeslisthelper(n - 2).distinct
        case false => twinprimeslisthelper(n - 1).distinct
      }
    }
  }
}

twinprimeslist(50)
twinprimeslist(3)
twinprimeslist(0)
twinprimeslist(10)


// Number 4: Goldbach's Conjecture
def goldBach(x : Int) ={
  (isEven(x), x > 2) match{
    case (true, false) => println("Number must be greater than 2")
    case (false, true) => println("Number must be even")
    case (false, false) => println("Number must be even, and greater than 2")
    case (true, true) => goldBachHelp(x, 1 to x toList)
  }
}

def isEven(n : Int) : Boolean ={
  n % 2 match {
    case 0 => true
    case _ => false
  }
}

def goldBachHelp(n : Int, primeList : List[Int]) ={
  primeList.reverse.filter(a=>prime(a)) match{
    case head :: tail => primeList contains (n - head) match {
      case true => println(n + " = " + (n - head) + " + " + head)
      case false => Nil
    }
    case _ => {println("No Goldbach Conjecture for number " + n)}
  }
}
goldBach(28)
goldBach(-1)
goldBach(4)
goldBach(0)