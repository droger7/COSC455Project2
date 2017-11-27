// COSC 455 - Programming Languages: Implementation and Design
// Project 2

// NAME: <David Rogers>


// Test Cases
val pTest1: List[Int] = List (1, 1, 1, 1, 0)//30
val qTest1: List[Int] = List(1, 0, 1, 1)//11
val test1ExectedSolution: List[Int] = List(1, 0, 1, 0, 0, 1)//41

val pTest2: List[Int] = List (1, 0, 0, 1, 1, 0, 1)//77
val qTest2: List[Int] = List(1, 0, 0, 1, 0)//18
val test2ExectedSolution: List[Int] = List(1, 0, 1, 1, 1, 1, 1)//95

val pTest3: List[Int] = List (1, 0, 0, 1, 0, 0, 1)//73
val qTest3: List[Int] = List(1, 1, 0, 0, 1)//25
val test3ExectedSolution: List[Int] = List(1, 1, 0, 0, 0, 1, 0)//98

val pTest4: List[Int] = List (1, 0, 0, 0, 1, 1, 1) //71
val qTest4: List[Int] = List(1, 0, 1, 1, 0) //22
val test4ExectedSolution: List[Int] = List(1, 0, 1, 1, 1, 0, 1)//93

val test5ExectedSolution: List[Int] = List(1, 1, 1, 0, 1, 1) //59
val test6ExectedSolution: List[Int] = List(1, 1, 0, 0, 0, 1) //49


// This function does the binary addition when there are uneven lists and still must
// finish the add with the carry bits.
def finishBinaryAdd(remainingBits: List[Boolean], carryBit: Boolean): List[Boolean] = {
  (remainingBits.isEmpty, carryBit) match{
    case (true , true) => List(true)
    case (false , false) => remainingBits
    case (false , true) => (!remainingBits.head)::finishBinaryAdd(remainingBits.tail, remainingBits.head)
    case (true , false) => (!remainingBits.head)::finishBinaryAdd(remainingBits.tail, remainingBits.head)
  }
}


// This function determines what the next carry bit should be based on current bits.
def getNextCarryBit(pBit: Boolean, qBit: Boolean, carryBit: Boolean): Boolean = {
  (pBit && carryBit) || (qBit && carryBit) || (pBit && qBit)
}

// This function does the binary addition of two Booleans and a carry bit.
def addBits(pBit: Boolean, qBit: Boolean, carryBit: Boolean): Boolean = {
  carryBit == (pBit == qBit)
}

// This function does the binary addition of two boolean lists. Note that the lists may not be equal in length.
def doBinaryAddition(pBits: List[Boolean], qBits: List[Boolean], carryBit: Boolean): List[Boolean] = {
  (pBits.isEmpty, qBits.isEmpty, carryBit) match{
    case (true, true, _) => Nil
    case (true, false, _) => finishBinaryAdd(qBits, carryBit)
    case (false, true, _) => finishBinaryAdd(pBits, carryBit)
    case (false, false, _) => addBits(pBits.head, qBits.head, carryBit)::doBinaryAddition(pBits.tail, qBits.tail, getNextCarryBit(pBits.head, qBits.head, carryBit))
  }
}
// This function converts a binary integer list into its corresponding boolean list.
def convertIntListToBooleanList(intList: List[Int]) = { intList.map{
    case 1 => true
    case 0 => false
  }
}

// This function converts a boolean list into its corresponding binary integer list.
def convertBooleanListToIntList(booleanList: List[Boolean]) = { booleanList.map{
    case true => 1
    case false => 0
  }
}

/* This is the "main" function to do binary addition. This function should:
    1. Convert the input parameter lists from integers to boolean. Use Scala reverse
    2. Reverse the lists (since binary addition is performed right to left). Use Scala reverse.
    3. Perform the binary addition with the doBinaryAddition function.
    4. Reverse the lists (to get back in proper order). Use Scala reverse.
    5. Convert the answer back to binary integer form for output.
  Note that the initial carry bit is assumed to be 0 (i.e., false).
*/
def binaryAddition(pList: List[Int], qList: List[Int]) = {
  convertBooleanListToIntList(doBinaryAddition(convertIntListToBooleanList(pList).reverse, convertIntListToBooleanList(qList).reverse, false).reverse)
}

def binarySubtraction(pList: List[Int], qList: List[Int]): List[Int] = {
  val signedBit : Int = 1
  signedBit :: binaryAddition(pList, twosComplement(qList)).drop(pList.length - qList.length)

}

def twosComplement(aList : List[Int]) : List[Int] = {
  val signed : List[Int] = List(1)
  binaryAddition(convertBooleanListToIntList(convertIntListToBooleanList(aList).map(b => !b)), signed)
}




// Testing binary addition.
if (binaryAddition(pTest1, qTest1).equals(test1ExectedSolution)) println("Test 1 passes!") else println("Test 1 fails.")
if (binaryAddition(pTest2, qTest2).equals(test2ExectedSolution)) println("Test 2 passes!") else println("Test 2 fails.")
if (binaryAddition(pTest3, qTest3).equals(test3ExectedSolution)) println("Test 3 passes!") else println("Test 3 fails.")
if (binaryAddition(pTest4, qTest4).equals(test4ExectedSolution)) println("Test 4 passes!") else println("Test 4 fails.")

// Testing binary subtraction.
if (binarySubtraction(pTest2, qTest2).equals(test5ExectedSolution)) println("Test 5 passes!") else println("Test 5 fails.")
if (binarySubtraction(pTest4, qTest4).equals(test6ExectedSolution)) println("Test 6 passes!") else println("Test 6 fails.")

twosComplement(pTest2) //correct
//1, 0, 0, 1, 1, 0, 1
twosComplement(qTest2) //correct
//1, 0, 0, 1, 0
binarySubtraction(pTest2, qTest2)
//1, 0, 1, 1, 0, 1, 1
//correct
//1, 1, 1, 0, 1, 1

twosComplement(pTest4) //correct
//(1, 0, 0, 0, 1, 1, 1) //71

twosComplement(qTest4) //correct
//(1, 0, 1, 1, 0)


binarySubtraction(pTest4,qTest4)
//1, 0, 1, 0, 0, 0, 1
//correct
//1, 1, 0, 0, 0, 1
