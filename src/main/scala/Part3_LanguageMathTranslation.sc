// COSC 455 - Programming Languages: Implementation and Design
// Project 2

// NAME: Kevin Pilkerton

val chinese : List[String] = List("ling", "yi", "er", "san", "si", "wu", "liu", "qi", "ba", "jiu", "shi")
val english : List[String] = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")

// Test Lists
val cList: List[String] = List("yi", "nine", "six", "ba")
val eList: List[String] = List("one", "nine", "six", "eight")
val testList : List[String] = List("yi", "Josh", "three", "si")

// Converts the list of strings to a list of ints and filters out strings that are not used in the language
def convert(s : List[String]) : List[Int] = {
  s.map {
    case ("zero") => 0
    case ("one") => 1
    case ("two") => 2
    case ("three") => 3
    case ("four") => 4
    case ("five") => 5
    case ("six") => 6
    case ("seven") => 7
    case ("eight") => 8
    case ("nine") => 9
    case ("ten") => 10
    case ("ling") => 0
    case ("yi") => 1
    case ("er") => 2
    case ("san") => 3
    case ("si") => 4
    case ("wu") => 5
    case ("liu") => 6
    case ("qi") => 7
    case ("ba") => 8
    case ("jiu") => 9
    case ("shi") => 10
    case _ => -1
  }
    .filter(_ > -1)
}

// gets the sum of the list added together
def sum(n : List[Int]) : Int ={
  n match {
    case Nil => 0
    case head :: tail => head + sum(tail)
  }
}

// prints the numbers that are added together to equal the sum
def numsAdded(n : List[Int]) : Unit = {
  (n.isEmpty, n.length == 1) match {
    case (true, _) => Nil
    case (false, true) => {
          print(n.head)
          numsAdded(n.tail)
        }
    case (false, false) => {
          print(n.head + " + ")
          numsAdded(n.tail)
    }
  }
}

// gets the product of the numbers in the list multiplied together
def product(n : List[Int]) : Int ={
  n match {
    case Nil => 1
    case head :: tail => head * product(tail)
  }
}

// prints the numbers of the list that are multiplied together to equal the product
def numsMultiplied(n : List[Int]) : Unit ={
  (n.isEmpty, n.length == 1) match {
    case (true, _) => Nil
    case (false, true) => {
      print(n.head)
      numsMultiplied(n.tail)
    }
    case (false, false) => {
      print(n.head + " * ")
      numsMultiplied(n.tail)
    }
  }
}

// prints out the contents of the list after it is converted
def displayTranslation(n : List[Int]) : Unit ={
  n.isEmpty match {
    case true => Nil
    case false => {
      print(n.head + " ")
      displayTranslation(n.tail)
    }
  }
}

// prints out the complete string of the numbers added and sum
def additionListString(list : List[Int]) : Unit={
  println(numsAdded(list) + " = " + sum(list))
}

// prints out the complete string of the numbers multiplied and the product
def multipliedListString(list : List[Int]) : Unit={
  println(numsMultiplied(list) + " = " + product(list))
}

// driver of the program
def runProg3(s : List[String]) : Unit = {
  val intList : List[Int] = convert(s)
  print("Translation:")
  displayTranslation(intList)
  println()
  print("Addition: ")
  additionListString(intList)
  print("Multiplication: ")
  multipliedListString(intList)
}

runProg3(cList)

runProg3(eList)

runProg3 (testList)
