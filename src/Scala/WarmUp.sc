// COSC 455 - Programming Languages: Implementation and Design
// Project 2

// NAME: <David Rogers>

//1.

def prime(n: Int): Boolean = {
  (2 until n) forall (n % _ != 0)
}

//test 1
var number : Int = 374
prime(number)

//2.
//couldn't think of a way to use pattern matching
def twinprimes(prime1: Int, prime2: Int): Boolean = {
  prime1 - prime2 match {
    case 2 => prime(prime1) && prime(prime2)
    case -2 => prime(prime1) && prime(prime2)
    case _ => false
  }
}

//test 2
twinprimes(41,43)

//3.

def twinprimeslist(n: Int) : List[Int] = {
  twinprimeslistHelper(n,3).distinct
}

def twinprimeslistHelper(n : Int, locate : Int) : List[Int] = {
  locate >= n match {
    case true => Nil
    case false =>
      twinprimes(locate, locate + 2) match {
        case true => locate :: locate + 2 :: twinprimeslistHelper(n, locate+1)
        case false => twinprimeslistHelper(n, locate + 1)

      }
  }
}

//test 3
twinprimeslist(50)

//4.

def goldbach(n : Int) : Unit = {
  (n >= 2) && (n % 2 == 0) match {
    case false => println("number must be even and greater than 2")
    case true => goldbachHelper(n, n-2) match {
          case Nil =>
          case head :: tail => println(tail.head + " + " + head + " = " + n + " ")
      }
  }
}

def goldbachHelper(n : Int, locate : Int) : List[Int] = {
  locate >= 1 match {
    case false => Nil
    case true => prime(locate) && locate <= n match {
          case false => goldbachHelper(n, locate - 1)
          case true => locate::goldbachHelper(n-locate, n-locate)
      }
  }
}

//test 4
goldbach(28)












