package lectures.part2afp

import scala.util.{Success, Try}

object PartialFunctions extends App {
  // accepts values in the int domain
  val aFunction = (x: Int) => x + 1 // Function1[Int, Int] === Int => Int

  val aFussyFunction = (x: Int) =>
    if (x == 1) 42
    else if (x == 2) 56
    else if (x == 5) 999
    else throw new FunctionNotApplicableException

  class FunctionNotApplicableException extends RuntimeException

  // 1. proper function --> (but partial in the sense that restricts the input domain?). Cannot be assigned to a partial function type
  val aNicerFussyFunction = (x: Int) => x match {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }
  // this is a function from domain {1,2,5} => Int
  // this is called a partial function because it accepts only a part of the Int domain arguments

  // Scala support partial function types and a shorthand notation for writing it is
  // 2. Partial function, besides restricting the domain it also defined using the partial function shorthand notation

  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }
  // partial function value -- A sweeter shorthand notation

  println(aPartialFunction(2))
  // println(aPartialFunction(57273))

  // Partial function utilities
  println(aPartialFunction.isDefinedAt(67))

  // lift => transform a partial function into somethign that will not throw exception if there is no match or any error ocurred
  val lifted = aPartialFunction.lift // Int => Option[Int]
  println(lifted(2))
  println(lifted(98))

  //orElse
  val pfChain = aPartialFunction.orElse[Int, Int] {
    case 45 => 67
  }
  println(pfChain(2))
  println(pfChain(45))

  //PF extends normal functions. A partial function can be assigned to a Total function
  // At this point I'm confused, partial definition initially was that a partial function was called partial
  // because it restricts the input domain and maybe the output domain, but that can be done with proper functions
  // or with partial functions, so then, what that heck???
  val aTotalFunction: Int => Int = {
    case 1 => 99
  }

  // HOFs accept partial functions as well
  // Supplying a partial function to map
  val aMappedList = List(1,2,3).map({
    case 1 => 42
    case 2 => 78
    case 3 => 1000
  })
  println(aMappedList)

  /*
      NOTE: unlike functions that can have multiple parameters partial function can only have one parameter type
   */

  /**
   * Exercises
   *
   * 1. - construct a PF instance yourself (anonymous class)
   * 2 - dumb chatbot as a PF
   */

  val anonymousPartial = new PartialFunction[Int, Int] {
    override def apply(v1: Int): Int = v1 match {
      case 1 => 236
      case 2 => 237
      case 3 => 238
      case 4 => 239
    }

    override def isDefinedAt(x: Int): Boolean = Try(apply(x)) == Success
  }

  println(anonymousPartial.isDefinedAt(47))
  println(anonymousPartial.isDefinedAt(3))
  println(anonymousPartial(3))
//  println(anonymousPartial(5))

  // Dumb chatbot
  val chatbot: PartialFunction[String, String] = {
    case "hello" => "Hi, my name is HAL9000"
    case "goodbye" => "Once you start talking to me, there is no return, human!"
    case "call mom" => "Unable to find your phone without your credit card"
  }
  scala.io.Source.stdin.getLines().map(chatbot).foreach(println)
}
