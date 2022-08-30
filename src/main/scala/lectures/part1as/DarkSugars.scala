package lectures.part1as

import scala.util.Try

object DarkSugars extends App{
  // syntax sugar #1: methods with single param

  def singleArgMethod(arg: Int): String = s"$arg little ducks..."

  // Using curly braces instead of parenthesis
  val description = singleArgMethod {
    //write some complex code
    42
  }

  val aTryInstance = Try {
    throw new RuntimeException
  }

  List(1,2,3).map( {
    //do something
    // at the end of the block returns the method required for map
    x => x + 1
  })

  // Syntax sugar #2: single abstract method
  trait Action {
    def act(x: Int): Int
  }

  val anInstance: Action = new Action {
    override def act(x: Int): Int = x + 1
  }

  //previous method can be converted to the following expression
  val aFunkyInstance: Action = (x: Int) => x + 1 // magic

  // example: Runnables
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("hello, Scala")
  })

  // previous expression can be rewritten as:
  val aSweeterThread = new Thread(() => println("sweet, Scala"))

  abstract class AnAbstractType {
    def implemented: Int = 23
    def f(a: Int): Unit
  }

  val anAbstractInstance: AnAbstractType = (a: Int) => println("sweet")

  // syntax sugar #3: the :: and #:: methods are special
  val prependedList = 2 :: List(3, 4)
  //usually infix methods call will be normally equivalent to
  // 2.::(List(3,4)) but there is no :: method for Ints
  // compiler convert previous expression to
  // List(3,4).::(2)
  // (this conversion happens based on the following scala spec: The associativity of an operator is determined by the operator's last character. Operators ending in a colon ‘:’ are right-associative. All other operators are left-associative)

  val someVar = 1 :: 2 :: 3 :: List(4, 5)
  // giving that they are right associative then compiler will rewrite this as
  val someVar1 = List(4,5).::(3).::(2).::(1) // equivalent

  // some other example
  class MyStream[T] {
    def -->:(value: T): MyStream[T] = this // actual implementation here
  }

  // Valid because the -->: is right associative
  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]

  //:: & #:: methods they are not special by themselves, they are special because they ends with : which the compiler to "assume"? that they are right associative

  // 4. Syntax sugar #4: multi-word method naming.

  class TeenGirl(name: String) {
    def `and then said`(gossip: String) = println(s"$name said $gossip")
  }

  val lilly = new TeenGirl("Lilly")
  lilly `and then said` "Scala is so sweet"

  //5. Syntax sugar #5: infix types (Generics)
  class Composite[A, B]
  val composite: Composite[Int, String] = ???

  //infix generic types
  // this is equivalent to the previous expression
  val composite1: Int Composite String = ???

  class --> [A,B]
  val towards: Int --> String = ???

  // 6. Syntax sugar #6: update() is very special, much like apply().
  val anArray = Array(1,2,3)
  anArray(2) = 7
  // is rewritten to anArray.update(2,7)
  // remember apply AND update() are special methods

  //Syntax sugar #7: setters for mutable containers
  class Mutable {
    private var _internalMember: Int = 0 // private for OO encapsulation
    def internalMember=_internalMember // "getter"
    def internalMember_=(value: Int): Unit = // "setter"
      _internalMember = value
  }

  val aMutableContainer = new Mutable
  aMutableContainer.internalMember = 42 // rewritten as aMutableContainer.internalMember_=(42)


}

