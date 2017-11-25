val chinese: List[String] = List("ling", "yi", "er", "san", "si", "wu", "liu", "qi", "ba", "jiu", "shi")
val english: List[String] = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")


//Retrieves the translated list and performs the necessary operations on that list
def go(list : List[String]) : Unit = {
  println("Translation:")
  println(transDisp(translate(list)))
  println("Addition:")
  println(addDisp(translate(list)) + " = " + add(translate(list)) + " ")
  println("Multiplication:")
  println(multDisp(translate(list)) + " = " + multiply(translate(list)) + " ")
}

def add(list : List[Int]) : Int = {
  list.foldLeft(0)(_ + _)
}

def addDisp(list : List[Int]) : Unit = {
  if(list.nonEmpty){
    if(list.length != 1) {
      print(list.head + " + ")
      addDisp(list.tail)
    }else{
      print(list.head)
      addDisp(list.tail)
    }
  }
}

def multiply(list : List[Int]) : Int = {
  list.foldLeft(1)(_ * _)
}

def multDisp(list : List[Int]) : Unit = {
  if(list.nonEmpty){
    if(list.length != 1) {
      print(list.head + " * ")
      multDisp(list.tail)
    }else{
      print(list.head)
      multDisp(list.tail)
    }
  }
}

def translate(list : List[String]) : List[Int] = {
  list match {
    case Nil => Nil
    case head :: tail =>
      english.contains(head) match {
        case true => english.indexOf(head) :: translate(tail)
        case false =>
          chinese.contains(head) match {
            case true => chinese.indexOf(head) :: translate(tail)
            // allows for skipping over nonNumbers
            case false => translate(tail)
          }
      }
  }
}
def transDisp(list : List[Int]) : Unit = {
  if(list.nonEmpty){
    print(list.head+" ")
    transDisp(list.tail)
  }
}

go(List("yi", "nine", "six", "ba"))
go(List("yi", "josh", "three", "si"))