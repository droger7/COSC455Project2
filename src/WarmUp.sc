
//********************************************//
//************   David Rogers   **************//
//********************************************//

//1.

def prime(n : Int) : Boolean = {
  if(n == 1) false
  else
    (2 until n) forall (n % _ != 0)
}

//test 1
var number : Int = 374
prime(number)

//2.

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

def goldbach(n: Int): Unit = {
  //error checking
  if ((n > 2) && (n % 2 == 0)) {
    var i: Int = 0
    var j: Int = n
    goldhelp(n, i, j)
  }
  else
    println("Integer was not above 2 or not positive")
}

def goldhelp(n: Int, i: Int, j: Int): Unit = {
  if (prime(j) && prime(i)) {
    if (!(j + i == n)) {
      var alpha = i + 1
      var beta = j - 1
      goldhelp(n, alpha, beta)
    }
    else
      println( i + " + " + j + " = " + n)
  }
  else {
    var alpha = i + 1
    var beta = j - 1
    goldhelp(n, alpha, beta)
  }
}

goldbach(28)












