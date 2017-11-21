// COSC 455 - Programming Languages: Implementation and Design
// Project 2

// NAME: David Rogers>

//1.

def prime(n : Int) : Boolean = {
  primeHelper(n, 2)
}

def primeHelper (n : Int, locate : Int) : Boolean = {
  locate >= n match {
    case true => true
    case false =>
      n % locate == 0 match {
        case true => false
        case false => primeHelper(n, locate+1)
      }
  }
}

//test 1
var number : Int = 374
prime(number)

//2.
//couldn't think of a way to use pattern matching
def twinprimes(n : Int, n2: Int) : Boolean = {
  if(prime(n) && prime(n2)) {
    if ((n - n2 == 2) || (n - n2 == -2)) {
      true
    }
    else false
  }
  else false
}

//test 2
twinprimes(41,43)

//3.

def twinprimeslist(n: Int) : List[Int] = {
  twinprimeslisthelper(n,3).distinct
}

def twinprimeslisthelper(n : Int, locate : Int) : List[Int] = {
  locate >= n match {
    case true => Nil
    case false =>
      twinprimes(locate, locate + 2) match {
        case true => locate :: locate + 2 :: twinprimeslisthelper(n, locate+1)
        case false => twinprimeslisthelper(n, locate + 1)

      }
  }
}

//test 3
twinprimeslist(50)

//4.

def goldbach(n : Int) : Unit = {
  (n >= 2) && (n % 2 == 0) match {
    case false => Nil
    case true =>
      goldbachHelper(n, n-2) match {
        case Nil =>
        case head :: tail => println(tail.head + " + " + head + " = " + n + " ")
      }
  }
}

def goldbachHelper(n : Int, locate : Int) : List[Int] = {
  locate >= 1 match {
    case false => Nil
    case true =>
      prime(locate) && locate <= n match {
        case false => goldbachHelper(n, locate - 1)
        case true => locate::goldbachHelper(n-locate, n-locate)
      }
  }
}

//test 4
goldbach(28)












