package lectures.part1as

import scala.annotation.tailrec

object Recap extends App{
  // declaring values and variables
  val aCondition: Boolean = false
  val aConditionVal = if(aCondition) 42 else 65
  // Difference between instructions (imperative language) vs expressions

  //Code block expression
  // Compiler infers types for us
  val aCodeBlock = {
    if (aCondition) 54
    55
  }

  // Unit -- the type of expressions that do not return anything meaningful but execute side efects
  val theUnit = println("hello, Scala")

  // functions
  def aFunction(x: Int): Int = x + 1

  //recursion: stack and tail
  @tailrec def factorial(n: Int, accumulator: Int): Int =
    if (n <= 0) accumulator
    else factorial(n-1, n* accumulator)

  // Object orientation
  class Animal
  class Dog extends Animal
  // Subtyping polymorphism
  val aDog: Animal = new Dog

  trait Carnivore {
    def eat(a: Animal): Unit
  }

  // Implementing a trait
  class Crocodile extends Animal with Carnivore {
    override def eat(a: Animal): Unit = println("crunch!")
  }

  // method notations
  val aCroc = new Crocodile
  aCroc.eat(aDog)
  aCroc eat aDog // natural language -- Infix notation. We also have prefix and sufix notation

  // Anonymous classes

  val aCarnivore = new Carnivore {
    override def eat(a: Animal): Unit = println("roar")
  }

  // generics
  abstract class MyList[+A] // variance and variance problems

  //singletons and companions
  object MyList

  // case classes
  case class Person(name: String, age: Int)

  //Exceptions and try/catch/finally expressions

  val throwsException = throw new RuntimeException //Nothing
  val aPotentialFailure = try {
    throw new RuntimeException
  } catch{
    case e: Exception => "Exception"
  } finally {
    println("some logs")
  }

  //packaging and imports

  // functional programming
  val increementer = new Function1[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }

  increementer(1)

  val anonymousIncrementer = (x: Int) => x + 1
  List(1,2,3).map(anonymousIncrementer) // HOF
  //map, flatMap, filter

  //for-comprehension
  val pairs = for {
    num <- List(1,2,3) //if condition
    char <- List('a','b','c')
  } yield num + "-" + char

  // Scala collections: Seqs, Arrays, Lists, Vectors, Maps, Tuples
  val aMap = Map(
    "Daniel" -> 789,
    "Jess" -> 555
  )

  // "collections": Options, Try
  val anOptin = Some(2)

  //pattern matching
  val x = 2
  val order = x match {
    case 1 => "first"
    case 2 => "second"
    case 3 => "third"
    case _ => x + "th"
  }

  val bob = Person("Bob", 22)
  val greeting = bob match {
    case Person(n, _) => s"Hi, my name is $n"
  }

  // string interpolators

  // all the patterns
  

}

