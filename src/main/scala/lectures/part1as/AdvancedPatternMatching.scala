package lectures.part1as

object AdvancedPatternMatching extends App {
  val numbers = List(1)
  val description = numbers match {
    case head :: Nil => println(s"the only element is head")
    case _ =>
  }

  /*
    ### how to make a class that can not be case class to be compatible with pattern matching
   */
  class Person(val name: String, val age: Int)
  //1. start defining a companion object
  object Person {
    def unapply(person: Person): Option[(String, Int)] = Some((person.name, person.age))
    def unapply(age: Int): Option[String] =
      Some(if (age<21) "minor" else "major")
  }

  val bob = new Person("Bob", 25)
  val greeting = bob match {
    case Person(n, a) => s"Hi, my name is $n and I am $a years old"
  }
  println(greeting)

  /*
    How does this works -- Breaking it down
    when runtime does pattern matching it goes thru some steps:
    1. Pattern called Person with a name and an age
    2. Looks for the method unapply on an object called Person and returns a tuple with 2 things
    3. compare return type from unapply method with the case pattern

    the pattern in the case Person(n, a) must match an object with the name Person
   */

  val legalStatus = bob.age match {
    case Person(status) => s"My legal status is $status"
  }

  println(legalStatus)

  // Exercise: create a new pattern to match integers against some special pattern
  val n: Int = 10
  val mathProperty = n match {
    case x if x < 10 => "Single digit"
    case x if x % 2 == 0 => "an even number"
    case _ => "no property"
  }

  println(mathProperty)
  // solved creating single objects for each of the conditions
  object even {
    def unapply(arg: Int): Option[Boolean] = {
      if(arg % 2 == 0) Some(true)
      else None
    }
  }

  object singleDigit {
    def unapply(arg: Int): Option[Boolean] =
      if (arg > -10 && arg < 10) Some(true)
      else None
  }

  val mathProperty1 = n match {
    case singleDigit(_) => "Single digit"
    case even(_) => "an even number"
    case _ => "no property"
  }

  println(mathProperty1)

  // because we dont care about the return value we can also write unapply as follows
  object even1 {
    def unapply(arg: Int): Boolean = {
      arg % 2 == 0
    }
  }

  object singleDigit1 {
    def unapply(arg: Int): Boolean =
      arg > -10 && arg < 10
  }

  val mathProperty2 = n match {
    case singleDigit1() => "Single digit"
    case even1() => "an even number"
    case _ => "no property"
  }

  println(mathProperty2)

}
