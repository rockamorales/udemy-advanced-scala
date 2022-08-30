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

  //infix patterns
  case class Or[A, B](a: A, b: B)

  val either = Or(2, "two")
  val humanDescription = either match {
    case Or(number, string) => s"$number is written as $string"
  }

  val humanDescription1 = either match {
    case number Or string => s"$number is written as $string"
  }
  println(humanDescription)
  println(humanDescription1)

  // infix patterns only works when you have only two things in the pattern

  // decomposing sequences
  val vararg = numbers match {
    case List(1, _*) => "starting with 1"
  }

  abstract class MyList[+A] {
    def head: A = ???
    def tail: MyList[A] = ???
  }

  case object Empty extends MyList[Nothing]
  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if (list == Empty) Some(Seq.empty)
      else unapplySeq((list.tail)).map(list.head +: _)
  }

  val myList: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val decomposed = myList match {
    case MyList(1,2,_*) => "starting with 1, 2"
    case _ => "something else"
  }

  // Custom return types for unapply
  // isEmpty: Boolean, get: something

  abstract class Wrapper[T] {
    def isEmpty: Boolean
    def get: T
  }

  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      def isEmpty = false
      def get = person.name
    }
  }
  println(bob match {
    case PersonWrapper(n) => s"This person's name is $n"
    case _ => "An alien"
  })
}
