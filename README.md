# Udemy advance scala course
## Lectures - Part 1 - Dark Syntax Sugar

### 1. syntax sugar #1: methods with single param

   Using curly braces instead of parenthesis to call a method. 
   
These are some examples:

```Scala
  def singleArgMethod(arg: Int): String = s"$arg little ducks..."
   
   val description = singleArgMethod {
     //write some hashCode
     42
   }
   
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

```

### 2. Syntax sugar #2: single abstract method pattern. 

Instances of traits with a single method can actually be reduced to lambdas 

Some Examples:
```scala
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
```
### 3. Syntax sugar #3: The :: and #:: methods are special.

```scala
  val prependedList = 2 :: List(3, 4)
```
usually infix methods call will be normally equivalent to 
```scala 
   2.::(List(3,4)) 
``` 
but there is no :: method for Ints.  
In this case the compiler convert previous expression to
```scala 
   List(3,4).::(2)
```
This conversion happens based on the following scala spec: The associativity of an operator is determined by the operator's last character. 
Operators ending in a colon ‘:’ are right-associative. All other operators are left-associative)

```scala
  val someVar = 1 :: 2 :: 3 :: List(4, 5)
```
given that they are right associative then compiler will rewrite this as:  
```scala
  val someVar1 = List(4,5).::(3).::(2).::(1) // equivalent

  // some other example
  class MyStream[T] {
    def -->:(value: T): MyStream[T] = this // actual implementation here
  }

  // Valid because the -->: is right associative
  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]
```  
```::``` & ```#::``` methods they are not special by themselves, they are special because they ends with : which the compiler to "assume"? that they are right associative

### 4. Syntax sugar #4: multi-word method naming.  
You can name methods using multiple words separated by spaces

```scala
  class TeenGirl(name: String) {
    def `and then said`(gossip: String) = println(s"$name said $gossip")
  }
  
  val lilly = new TeenGirl("Lilly")
  lilly `and then said` "Scala is so sweet"

```

### 5. Syntax sugar #5: infix types (Generics).  

```scala
  class Composite[A, B]
  val composite: Composite[Int, String] = ???

  //infix generic types
  // this is equivalent to the previous expression
  val composite1: Int Composite String = ???

  class -->[A,B]
  val towards: Int --> String = ???

```
### 6. Syntax sugar #6: update() is very special, much like apply().

```scala
  val anArray = Array(1,2,3)
  anArray(2) = 7
```
is rewritten to ```anArray.update(2,7)```  
**remember apply() AND update() are special methods**

###  7. Syntax sugar #7: setters and getters for mutable containers

```scala
  class Mutable {
    private var _internalMember: Int = 0 // private for OO encapsulation
    def internalMember=_internalMember // "getter"
    def internalMember_=(value: Int): Unit = // "setter"
      _internalMember = value
  }

  val aMutableContainer = new Mutable
  // rewritten as aMutableContainer.internalMember_=(42)
  aMutableContainer.internalMember = 42
```

## Lectures - Part 1 - Advanced Pattern Matching

```scala
  val numbers = List(1)
  val description = numbers match {
    case head :: Nil => println(s"the only element is head")
    case _ =>
  }
```
Only structures available for pattern matching are:
- constants
- wildcards
- case classes
- tuples
- some special magic like above

### how to make a class that can not be case class to be compatible with pattern matching

```scala
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

```
How does this works -- Breaking it down
when runtime does pattern matching it goes thru some steps:
1. look for a class called Person with a name and an age
2. Looks for the method unapply on an object called Person and returns a tuple with 2 things
3. compare return type from unapply method with the case pattern

the pattern in the case Person(n, a) must match an object with the name Person
the pattern in the case PersonPattern(n, a) must match an object with the name PersonPattern  
method unapply 

you can define multiple patterns to match by overloading the unapply method

```scala
  val legalStatus = bob.age match {
    case Person(status) => s"My legal status is $status"
  }

  println(legalStatus)
```
#### Exercise: create a new pattern to match integers against some special pattern
```scala
    val n: Int = 45
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

```

#### Infix patterns
```scala
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

```
**Infix patterns only works when you have only two things in the pattern**

#### Decomposing sequences
```scala
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

```

#### Custom return types for unapply
The only methods a custom return type for unapply must have are:  
1. isEmpty: Boolean
2. get: something

```scala
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
```

#### Takeaways
1. We can define our own patterns
2. we can decompose any class that is not a case class by defining an unapply or unapplySeq method in an object that is named as the pattern defined
3. Infix patterns
4. Unapply sequences
5. Custom return types for unapply (really rare)


## Lectures - Part 1 - Advance Functional Programming
### Partial funtions

Following function accepts values in the int domain
```scala
  val aFunction = (x: Int) => x + 1 // Function1[Int, Int] === Int => Int
```

A normal function without patter matching
```scala
val aFussyFunction = (x: Int) =>
  if (x == 1) 42
  else if (x == 2) 56
  else if (x == 5) 999
  else throw new FunctionNotApplicableException

class FunctionNotApplicableException extends RuntimeException

```
proper function(Normal/Total function) --> (but partial in the sense that restricts the input domain?). Cannot be assigned to a partial function type
```scala
val aNicerFussyFunction = (x: Int) => x match {
  case 1 => 42
  case 2 => 56
  case 5 => 999
}
```
this is a function from domain {1,2,5} => Int  
this is called a partial function because it accepts only a part of the Int domain arguments

Scala support partial function types and a shorthand notation for writing a partial function is the following  
Partial function: besides restricting the domain it also defined using the partial function shorthand notation
```scala
val aPartialFunction: PartialFunction[Int, Int] = {
  case 1 => 42
  case 2 => 56
  case 5 => 999
}
```
partial function value -- A sweeter shorthand notation

```scala
    println(aPartialFunction(2))
    println(aPartialFunction(57273)) // will fail
```

Partial function utilities
1. isDefinedAt method
```scala
println(aPartialFunction.isDefinedAt(67))
```

2. lift => transform a partial function into somethign that will not throw exception if there is no match or any error ocurred
```scala
val lifted = aPartialFunction.lift // Int => Option[Int]
println(lifted(2))
println(lifted(98))
```
3. orElse

```scala
val pfChain = aPartialFunction.orElse[Int, Int] {
  case 45 => 67
}
println(pfChain(2))
println(pfChain(45))
```
PF extends normal functions. A partial function can be assigned to a Total function  At this point I'm confused, partial definition initially was that a partial function was called partial because it restricts the input domain and maybe the output domain, but that can be done with proper functions  or with partial functions, so then, what that heck???
```scala
val aTotalFunction: Int => Int = {
  case 1 => 99
}

```
HOFs accept partial functions as well  
Supplying a partial function to map
```scala
val aMappedList = List(1,2,3).map({
  case 1 => 42
  case 2 => 78
  case 3 => 1000
})
println(aMappedList)
```
**NOTE: unlike functions that can have multiple parameters partial function can only have one parameter type**

#### Exercises
1. - construct a PF instance yourself (anonymous class)
2. dumb chatbot as a PF

PF instance
```scala
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
println(anonymousPartial(5))
```

Dumb chat
```scala
  val chatbot: PartialFunction[String, String] = {
    case "hello" => "Hi, my name is HAL9000"
    case "goodbye" => "Once you start talking to me, there is no return, human!"
    case "call mom" => "Unable to find your phone without your credit card"
  }
  scala.io.Source.stdin.getLines().map(chatbot).foreach(println)
```
#### Takeways
1. partial functions
2. how to use them
3. done with pattern matching
3. Utilities:
   - isDefinedAt
   - lift
   - orElse
4. Partial functions are used on other types:
   - map
   - collect on collections


### A Functional Collection: Set
```scala
  val set = Set(1,2,3)
```
Set instances are callable (they have apply method)
```scala
  set(2) //true
  set(42) //false
```

Set instance are callable like functions.
The apply method always returns a value: true/false.
=> Sets behave like actual functions

**Sets ARE Functions**
```scala
  trait Set[A] extends (A) => Boolean with ...
```

Methods --, - and intersect are my implementation
        __1, _1, and intersect1 are the instructor implementation
all other methods are the instructor implementation

```scala
package exercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {
  /*
    Exercise - implement a functional set
   */
  def apply(elem: A): Boolean =
    contains(elem)
  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]
  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(f: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit
  //My definitions
  def -(e: A): MySet[A]
  def &(e: MySet[A]): MySet[A]
  def --(e: MySet[A]): MySet[A]

  def _1(e: A): MySet[A]
  def _&(e: MySet[A]): MySet[A]
  def __1(e: MySet[A]): MySet[A]

}

class EmptySet[A] extends MySet[A] {
  override def contains(elem: A): Boolean = false
  override def +(elem: A): MySet[A] = NonEmptySet(elem, this)
  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet
  override def map[B](f: A => B): MySet[B] = new EmptySet[B]
  override def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]
  override def filter(f: A => Boolean): MySet[A] = this
  override def foreach(f: A => Unit): Unit = ()
  //My implementations for EmptySet
  override def -(e: A): MySet[A] = this
  override def &(set: MySet[A]): MySet[A] = this
  override def --(set: MySet[A]): MySet[A] = this

}

case class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  def contains(elem: A): Boolean = elem == head || tail.contains(elem)
  def +(elem: A): MySet[A] = {
    if (this contains elem) this
    else new NonEmptySet[A](elem, this)
  }

  def ++(anotherSet: MySet[A]): MySet[A] =
    tail ++ anotherSet + head
  def map[B](f: A => B): MySet[B] =
    (tail map f) + f(head)
  def flatMap[B](f: A => MySet[B]): MySet[B] = (tail flatMap f) ++ f(head)
  def filter(f: A => Boolean): MySet[A] = {
    val filteredTail = tail filter f
    if (f(head)) filteredTail + head
    else filteredTail
  }

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }
  
  //My implementations for NonEmptySet
  override def -(e: A): MySet[A] = filter(_!=e)
  override def &(set: MySet[A]): MySet[A] =
    if (set.contains(head)) new NonEmptySet[A](head, tail & set)
    else tail & set

  override def --(set: MySet[A]): MySet[A] =
    if (set.contains(head)) tail -- set
    else (tail -- set) + head
}

object MySet {
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if(valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)
    buildSet(values.toSeq, new EmptySet[A])
  }
}

object MySetPlayground extends App {
  val s = MySet(1,2,3,4)
  s + 5 ++ MySet(-1, -2) + 3 flatMap (x => MySet(x, 10 * x)) filter (_ % 2 == 0) foreach println
}
```

### A potentially infinite set

```scala
package exercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {
  /*
    Exercise - implement a functional set
   */
  def apply(elem: A): Boolean =
    contains(elem)
  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]
  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(f: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit
  def -(e: A): MySet[A]
  def &(e: MySet[A]): MySet[A]
  def --(e: MySet[A]): MySet[A]

  def minus(e: A): MySet[A]

  def intersect(e: MySet[A]): MySet[A]

  def difference(e: MySet[A]): MySet[A]

  def unary_! : MySet[A]

  /*
    Exercise
      - Removing an element
      -- intersection with another set
      - difference with another set
   */
}

class EmptySet[A] extends MySet[A] {
  override def contains(elem: A): Boolean = false
  override def +(elem: A): MySet[A] = NonEmptySet(elem, this)
  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet
  override def map[B](f: A => B): MySet[B] = new EmptySet[B]
  override def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]
  override def filter(f: A => Boolean): MySet[A] = this
  override def foreach(f: A => Unit): Unit = ()

  override def -(e: A): MySet[A] = this

  override def &(set: MySet[A]): MySet[A] = this

  override def --(set: MySet[A]): MySet[A] = this

  override def minus(e: A): MySet[A] = this

  override def intersect(e: MySet[A]): MySet[A] = this

  override def difference(e: MySet[A]): MySet[A] = this

  override def unary_! : MySet[A] = new PropertyBasedSet[A](_ => true)
}

// all elements of type A which satisfy a property
// { x in A | property(x) }
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {
  override def contains(elem: A): Boolean = property(elem)

  // { x in A | property(x) } + element = { x in A | property(x) || x == element}
  override def +(elem: A): MySet[A] =
    new PropertyBasedSet[A]( x=> property(x) || x == elem)

  override def ++(anotherSet: MySet[A]): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || anotherSet(x))

  override def map[B](f: A => B): MySet[B] = politelyFail

  override def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail

  override def filter(f: A => Boolean): MySet[A] = new PropertyBasedSet[A](x => property(x) && f(x)) // Property base set

  override def foreach(f: A => Unit): Unit = politelyFail

  override def -(e: A): MySet[A] = filter( x => x != e )

  override def &(e: MySet[A]): MySet[A] = filter(e)

  override def --(e: MySet[A]): MySet[A] = filter(!e)

  override def minus(e: A): MySet[A] = filter( x => x != e)

  override def intersect(e: MySet[A]): MySet[A] = filter(e)

  override def difference(e: MySet[A]): MySet[A] = filter(!e)

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !property(x))

  def politelyFail = throw new IllegalArgumentException("Really deep rabbit hole!")

}

case class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  def contains(elem: A): Boolean = elem == head || tail.contains(elem)
  def +(elem: A): MySet[A] = {
    if (this contains elem) this
    else new NonEmptySet[A](elem, this)
  }

  def ++(anotherSet: MySet[A]): MySet[A] =
    tail ++ anotherSet + head
  def map[B](f: A => B): MySet[B] =
    (tail map f) + f(head)
  def flatMap[B](f: A => MySet[B]): MySet[B] = (tail flatMap f) ++ f(head)
  def filter(f: A => Boolean): MySet[A] = {
    val filteredTail = tail filter f
    if (f(head)) filteredTail + head
    else filteredTail
  }

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }

  override def -(e: A): MySet[A] = filter(_!=e)
  override def &(set: MySet[A]): MySet[A] =
    if (set.contains(head)) new NonEmptySet[A](head, tail & set)
    else tail & set

  override def --(set: MySet[A]): MySet[A] = {
    val differenceTail = tail -- set
    if (set.contains(head))
      differenceTail
    else differenceTail + head
  }

  override def minus(e: A): MySet[A] =
    if (head == e) tail
    else tail - e + head

  override def intersect(e: MySet[A]): MySet[A] =
    filter(!e(_))

  override def difference(e: MySet[A]): MySet[A] = filter(e)

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !this.contains(x))
}

object MySet {
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if(valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)
    buildSet(values.toSeq, new EmptySet[A])
  }
}

object MySetPlayground extends App {
  val s = MySet(1,2,3,4)
  s -- MySet(1,2) foreach println
  //s + 5 ++ MySet(-1, -2) difference MySet(1,2) + 3 foreach println
  val negative = !s
  println(negative(2))
  println(negative(5))
  val negativeEven = negative.filter(_ % 2 == 0)
  println(negativeEven(5))
  val negativeEven5 = negativeEven + 5
  println(negativeEven5(5))
}
```

### Functional Seq
Sequences are "Callable" through and integer index:
```scala
trait Seq[+A] extends PartialFunction[Int, A] {
  def apply(index: Int): A
}

val numbers = List(1,2,3)
numbers(1) // 2
numbers(3) // java.lang.IndexOutOfBoundsException

```
Seqs are partially defined on the domain [0 ... length - 1]

Sequences are partial functions!


### Functional Map
Maps are "callable" through their keys:

```scala
trait Map[A, +B] extends PartialFunction[A, B] {
  def apply(key: A): B
  def get(key: A): Option[B]
}

val phoneMappings = Map(2 -> "ABC", 3 -> "DEF")
phoneMappings(2) //"ABC"
phoneMappings(1) // java.lang.NoSuchElementException

```
A map is defined on the domain of its keys which is a subdomain on the type A  
Maps are partial functions


### Currying and partially applied functions
Functions returning other functions as a result

```scala
 val superAdder: Int => Int => Int =
      x=> y => x + y
  
 val add3 = superAdder(3)
 println(add3(5))

 println(superAdder(3)(5)) //curried function
```


```scala
  //curried functions
  val superAdder: Int => Int => Int =
    x => y => x + y

  val add3 = superAdder(3)
  println(add3(5))

  println(superAdder(3)(5)) //curried function

  // METHOD!
  def curriedAdder(x: Int)(y: Int): Int = x + y // curried function -- Scala syntactic sugar???

  // Type annotation is required in this case, otherwise compiler will complain
  val add4: Int => Int = curriedAdder(4)

  //lifting

  //Methods are part of instances of the classes

  // Transforming Methods to a function is called lifting or a more technical term ETA-EXPANSION

  // functions != methods (JVM Limitation)
  def inc(x: Int) = x + 1
  List(1,2,3).map(x => inc(x))

  //the way we can force the compiler to do ETA expansion when we want is when want to use partial function applications
  val add5 = curriedAdder(5) _

  //EXERCISE
  val simpleAddFunction = (x: Int, y: Int) => x + y

  def simpleAddMethod(x: Int, y: Int) = x + y

  def curriedAddMethod(x: Int)(y: Int) = x + y

  // add7: Int => Int = y => 7 + y

  val add7 = (y: Int) => simpleAddFunction(7, y)
  println(add7(3))
  val add7_1 = simpleAddFunction.curried(7)

  val add7_2 = curriedAddMethod(7) _ //PAF
  val add7_4 = curriedAddMethod(7)(_) //PAF = alternative syntax

  val add7_5 = simpleAddMethod(7, _: Int) // alternative syntax for turning methods into function values
  // force the compiler to rewrite it as y => simpleAddMethod(7, y)

  // underscores are powerful

  def concatenator(a: String, b: String, c: String) = a + b + c

  val insertName = concatenator("Hello, I'm ", _: String, ", how are you?")
  println(insertName("Roberto"))

  val fillInTheBlanks = concatenator("Hello, ", _: String, _: String) // each underscore will be a different parameter

  println(fillInTheBlanks("Daniel", " Scala is awesome"))

  // EXERCISE
  /*
    1. Process a list of numbers and return their string representations with different formats
      User the %4.2f, %8.6f and %14.12f with a curried formatter function.
   */

  def curriedFormatter(s: String)(number: Double): String = s.format(number)
  val numbers = List(Math.PI, Math.E, 1, 9.8, 1.3e-12)

  val simpleFormat = curriedFormatter("%4.2") _ //lift
  val seriousFormat = curriedFormatter("%8.6f") _
  val preciseFormat = curriedFormatter("%14.12f") _

  println(numbers.map(preciseFormat))

  println(numbers.map(curriedFormatter("14.12f"))) //compiler does the ETA expansion for us, based on the map expected function parameter
  println("%8.6f".format(Math.PI))

  /*
    2. difference between
      - Functions vs Methods
      - parameters: by-name vs 0-lambda
   */

  def byName(n: => Int) = n + 1
  def byFunction(f: () => Int) = f() + 1

  def method: Int = 42
  def parenMethod(): Int = 42

  /*
      calling byName and byFunction
        - int
        - method
        -- parenMethod
        - lambda
        - PAF
   */

  byName(23) // ok
  byName(method) // ok
  byName(parenMethod())
  byName(parenMethod) // ok but beware this is equivalent byName(parentMethod())

  //  byName(() => 42) //not OK
  byName((() => 42)())
//  byName(parenMethod _ ) // Not ok
//  byFunction(45)  // not ok
//  byFunction(method) // not ok!! does not do ETA-expansion
  byFunction(() => 46) //works
  byFunction(parenMethod _) // also works, but _ is unnecessary

```

### Lazy evaluations

```scala
package lectures.part2afp

object LazyEvaluation extends App {
  //lazy delays the evaluation of values
  lazy val x: Int = {
    println("hello")
    42
  }

  println(x)
  println(x)

  //Examples of implications
  // 1. with side effects
  def sideEffectCondition: Boolean = {
    println("Boo")
    true
  }

  def simpleCondition: Boolean = false

  lazy val lazyCondition = sideEffectCondition
  println(if ( simpleCondition && lazyCondition) "yes" else "no")

  // in conjunction with call by name
  def byNameMethod(n: => Int): Int = {
    lazy val t = n
    t + t + t + 1
  }
  //technique called CALL BY NEED
  def retrieveMagicValue = {
    println("waiting")
    Thread.sleep(1000)
    42
  }
  println(byNameMethod(retrieveMagicValue))
  //waiting is printed 3 times. Use lazy val on the byNameMethod to fix it

  def lessThan30(i: Int): Boolean = {
    println(s"$i is less than 30?")
    i < 30
  }

  def greaterThan20(i: Int): Boolean = {
    println(s"$i is greater than 20?")
    i > 20
  }

  val numbers = List(1,25,40,5,23)
  val lt30 = numbers.filter(lessThan30)
  val gt20 = lt30.filter(greaterThan20)
  println(gt20)
  val lt30Lazy = numbers.withFilter(lessThan30)
  val gt20Lazy = lt30Lazy.withFilter(greaterThan20)
  println
  println(gt20Lazy)
  gt20Lazy.foreach(println)

  //for comprehensions use withFilter with guards
  for {
    a <- List(1,2,3) if a % 2 == 0
  } yield a + 1
  List(1,2,3).withFilter(_ % 2 == 0).map(_ + 1) //List[Int]

  /*
     Exercise: Implement a lazily evaluated, singly linked STREAM of elements.

     MyStream.from(1)(x => x + 1) = strewam of natural numbers ( potentially infinite!)
     naturals.take(100) // lazily evaluated stream of the first 100 naturals (finite stream)
     naturals.foreach(println) // will crash - infinite!
     naturals.map(_ * 2) // stream of all even numbers (potentially infinite)
   */

  abstract class MyStream[+A]{
    def isEmpty: Boolean
    def head: A
    def tail: MyStream[A]

    def #::[B >: A](element: B): MyStream[B] //prepend operator
    def ++[B >: A](anotherStream: MyStream[B]): MyStream[B] // concatenate two streams
    def foreach(f: A => Unit): Unit
    def map[B](f: A => B): MyStream[B]
    def flatMap[B](f: A => MyStream[B]): MyStream[B]
    def filter(predicate: A => Boolean): MyStream[A]

    def take(n: Int): MyStream[A]
    def takeAsList(n: Int): List[A]
  }

  object MyStream {
    def from[A](start: A)(generator: A => A): MyStream[A] = ???
  }
}
```

#### Exercises

```scala
package exercises

import scala.annotation.tailrec

/*
   Exercise: Implement a lazily evaluated, singly linked STREAM of elements.

   MyStream.from(1)(x => x + 1) = strewam of natural numbers ( potentially infinite!)
   naturals.take(100) // lazily evaluated stream of the first 100 naturals (finite stream)
   naturals.foreach(println) // will crash - infinite!
   naturals.map(_ * 2) // stream of all even numbers (potentially infinite)
 */

abstract class MyStream[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: MyStream[A]
  def #::[B >: A](element: B): MyStream[B] //prepend operator
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] // concatenate two streams
  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): MyStream[B]
  def flatMap[B](f: A => MyStream[B]): MyStream[B]
  def filter(predicate: A => Boolean): MyStream[A]
  def take(n: Int): MyStream[A]
  def takeAsList(n: Int): List[A] = take(n).toList()

  @tailrec
  final def toList[B >: A](acc: List[B] = Nil): List[B] =
    if (isEmpty) acc.reverse
    else tail.toList(head :: acc)

}

object EmptyStream extends MyStream[Nothing] {
  override def isEmpty: Boolean = true
  override def head: Nothing = throw new NoSuchElementException
  override def tail: MyStream[Nothing] = throw new NoSuchElementException
  override def #::[B >: Nothing](element: B): MyStream[B] = new Cons(element, this)
  override def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream
  override def foreach(f: Nothing => Unit): Unit = ()
  override def map[B](f: Nothing => B): MyStream[B] = this
  override def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this
  override def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this
  override def take(n: Int): MyStream[Nothing] = this
}

class Cons[+A](hd: A, tl: => MyStream[A]) extends MyStream[A] {
  override def isEmpty: Boolean = false
  override val head: A = hd
  override lazy val tail: MyStream[A] = tl // call by need
  override def #::[B >: A](element: B): MyStream[B] = new Cons(element, this)
  override def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] = new Cons(head, tail ++ anotherStream)
  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def map[B](f: A => B): MyStream[B] = new Cons(f(head), tail.map(f))
  override def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)
  override def filter(predicate: A => Boolean): MyStream[A] =
    if (predicate(head)) new Cons(head, tail.filter(predicate))
    else tail.filter(predicate)

  override def take(n: Int): MyStream[A] = {
    if(n < 0) EmptyStream
    else if (n == 1) new Cons(head, EmptyStream)
    else new Cons(head, tail.take(n - 1))
  }

}

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] = new Cons(start, MyStream.from(generator(start))(generator))
}

object StreamPlayground extends App{
  val naturals = MyStream.from(1)( _ + 1)
  println(naturals.head)
  println(naturals.tail.head)
  println(naturals.tail.tail.head)

  val startFrom0 = 0 #:: naturals // naturals.#::(0)
  println(startFrom0.head)

  startFrom0.take(10000).foreach(println)

    // map, flatMap
  println(startFrom0.map( _ * 2).take(100).toList())
  println(startFrom0.flatMap(x => new Cons(x, new Cons(x + 1, EmptyStream))).take(10).toList())
  println(startFrom0.filter(_ < 10).take(10).toList())

  // Exercises on streams
  // 1. - stream of Fibonacci numbers
  // 2. - stream of prime numbers with Eratosthenes' sieve
  /*
    [2, 3, 4]
   */

  def fibonacci (first: Int, second: Int): MyStream[Int] = new Cons(first, fibonacci(second, first + second))
  println(fibonacci(1, 1).take(100).toList())

  def eratosthenes(numbers: MyStream[Int]): MyStream[Int] =
    if(numbers.isEmpty) numbers
    else new Cons(numbers.head, eratosthenes(numbers.tail.filter(_ % numbers.head != 0)))

  println(eratosthenes(MyStream.from(2)(_ + 1)).take(100).toList())
}
```

### Monads
Monads are a king of types which have some fundamental ops

```scala
trait MonadTemplate[A] {
  def unit(value: A): MonadTemplate[A]
  def flatMap[B](f: A => MonadTemplate[B]): MonadTemplate[B]
}
```

List, Option, Try, Future, Stream, Set area all monads.

Operations must satisfy the monad laws:
1. left-identity
```scala
  unit(x).flatMap(f) == f(x)
```

2. right-identity
```scala
    aMonadInstance.flatMap(unit) == aMonadInstance
```

3. associativity
```scala
m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))
```

#### Example: List
1. left-identity
```scala
  List(x).flatMap(f) = f(x) ++ Nil.flatMap(f) = f(x)
```
2. Right-identity
```scala
list.flatMap(x => List(x)) = list
```
3. Associativity:

```scala
[a b c].flatMap(f).flatMap(g) = (f(a) ++ f(b) ++ f(c)).flatMap(g) =
f(a).fltMap(g) ++ f(b).flatMap(g) ++ f(c).flatMap(g) = 
[a b c].flatMap(f(_).flatMap(g)) =
[a b c].flatMap(x => f(x).flatMap(g))
```

#### Example: Option
1. Left indentity:
```scala
Option(x).flatMap(f) = f(x)
Some(x).flatMap(f) = f(x)
```
2. Right identity
```scala
opt.flatMap(x => Option(x)) = opt

Some(v).flatMap(x => Option(x)) = Option(v) = Some(v)
```

3. Associativity
```scala
    o.flatMap(f).flatMap(g) = o.flatMap(x => f(x).flatMap(g))

  Some(v).flatMap(f).flatMap(g) = f(v).flatMap(g)
  Some(v).flatMap(x => f(x).flatMap(g)) = f(v).flatMap(g)

```

```scala
package lectures.part2afp

object Monads extends App{
  trait Attempt[+A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }

  object Attempt {
    def apply[A](a: => A): Attempt[A] = try {
      Success(a)
    } catch {
      case e: Throwable => Fail(e)
    }
  }

  case class Success[+A](value: A) extends Attempt[A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B] =
      try{
        f(value)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Fail(e: Throwable) extends Attempt[Nothing] {
    override def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  }

  //Proving the Monad laws
  // 1. left-identity
  /*
   * unit.flatMap(f) = f(x)
   */

  val attempt = Attempt {
    throw new RuntimeException("My own monad, yes!")
  }

  class Lazy[+A](value: => A){
    // call by need
    private lazy val internalValue = value
    def use: A = value
    def flatMap[B](f: => A => Lazy[B]): Lazy[B] = f(internalValue)
  }

  object Lazy {
    def apply[A](value: => A): Lazy[A] = new Lazy(value)
  }

  val lazyInstance = Lazy {
    println("Today I don't feel like doing anything")
    42
  }

  println(lazyInstance.use)


  val flatMappedInstance = lazyInstance.flatMap(x => Lazy {
    10 * x
  })

  val flatMappedInstance2 = lazyInstance.flatMap(x => Lazy {
    10 * x
  })

  flatMappedInstance.use
  flatMappedInstance2.use


  /* Proving the Monad Laws
  1. left-identity
    unit.flatMap(f) = f(v)
    Lazy(v).flatMap(f) = f(v)

  2. Right-identity
  l.flatMap(unit) = l
  Lazy(v).flatMap(x => Lazy(x)) = Lazy(v)

  3. associativity: l.flatMap(f).flatMap(g) = l.flatMap(x => f(x).flatMap(g)
  Lazy(v).flatMap(f).flatMap(g) = f(v).flatMap(g)
  Lazy(v).flatMap(x => f(x).flatMap(g)) = f(v).flatMap(g)

   */

  /*
    Monad[T] { //List
      def flatMap[B](f: T => Monad[B]): Monad[B] = (implemented)
      def map[B](f: T => B): Monad[B] = flatMap(x => unit(f(x)))
      def flatten(m: Monad[Monad[T]])): Monad[T] = m.flatMap(x: Monad[T] => x)
    }

    List(1,2,3).map(_ * 2) = List(1,2,3).flatMap(x => List(x*2))
    List(List(1,2), List(3,4)).flatten = List(List(1,2), List(3, 4)).flatMap(x => x) = List(1,2,3,4)

   */

}

```

## Lectures - Part 3 - Concurrency
Creation, Manipulation & Communication of JVM threads

### Intro

```scala
package lectures.part3async

import java.util.concurrent.Executors

object Intro extends App {
  // Creation, Manipulation & Communication of JVM threads

  // JVM Threads
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("Running in parallel")
  })

  // Creates a JVM thread which will run on top of an OS thread
  aThread.start() // gives the signal to the JVM to start a JVM thread

  //Blocks until a thread finishes running
  aThread.join()

  val threadHello = new Thread(() => (1 to 5).foreach(_ => println("hello")))
  val threadGoodbye = new Thread(() => (1 to 5).foreach(_ => println("goodbye")))

  threadHello.start()
  threadGoodbye.start()
  // Different runs within a multithreaded env produce different results

  // Executors
  val pool = Executors.newFixedThreadPool(10)
  pool.execute(() => println("Something in the thread pool"))

  pool.execute(() => {
    Thread.sleep(1000)
    println("done after 1 second")
  })

  pool.execute(() => {
    Thread.sleep(1000)
    println("almost done")
    Thread.sleep(1000)
    println("Done after 2 seconds")
  })

  //  pool.shutdown()
  //  //Following should throw an exception in the calling thread
  //  pool.execute(() => println("Should not appear"))

  //Will interrupt current running threads
  //pool.shutdownNow()

  // will return true after shutdown has been called even if there are still threads running
  println(pool.isShutdown)


}
```
### Concurrency problems on the JVM

```scala
package lectures.part3async

object ConcurrencyProblems {
  def runInParallel(): Unit = {
    var x = 0
    val thread1 = new Thread(() => { // abstract method pattern
      x = 1
    })

    val thread2 = new Thread(() => {
      x = 2
    })

    thread1.start()
    thread2.start()
    println(x) // race condition
  }

  case class BankAccount (var amount: Int)

  def buy(bankAccount: BankAccount, thing: String, price: Int): Unit = {
    /*
      involves 3 steps:
        - read old value
        - compute result
        - write new value
     */
    bankAccount.amount -= price
  }

  /*
    Example race condition:
    thread1 (shoes)
      - reads amount 50000
      - compute result 50000 - 3000 = 47000
    thread2 (iPhone)
      - reads amount 50000
      - compute result 50000 - 4000 = 46000
    thread1 (shoes)
      - write amount 47000
    thread2 (iPhone)
      - write amount 46000

   */

  def buySafe(bankAccount: BankAccount, thing: String, price: Int): Unit = {
    bankAccount.synchronized { // does not allow multiple thread to run the critical section AT THE SAME TIME
      bankAccount.amount -= price // critical section
    }
  }

  def demoBankingProblem(): Unit = {
    (1 to 10000000).foreach {
      _ =>
        val account = BankAccount(50000)
        val thread1 = new Thread(() => buy(account, "shoes", 3000))
        val thread2 = new Thread(() => buy(account, "iPhone", 4000))
        thread1.start()
        thread2.start()
        thread1.join()
        thread2.join()
        if (account.amount != 43000) println(s"AHA! I've just broken the bank: ${account.amount}")
        //else println(s"Everything looks good here: ${account.amount}")
    }
  }

  /**
   * Exercises
   * 1 - create "inception threads"
   *  thread1
   *    -> thread 2
   *      -> thread 3
   *   each thread prints "hello from thread $i)
   *   print all messages  IN REVERSE ORDER
   *  2. what's the max/min value of x
   *  3 - "sleep fallacy"
   */

    // 1- inception threads
    def inceptionThreads(maxThreads: Int, i: Int = 1): Thread =
      new Thread(() => {
        if (i < maxThreads) {
          val newThread = inceptionThreads(maxThreads, i + 1)
          newThread.start()
          newThread.join()
        }
        println(s"Hello from thread $i")
      })
  // 2
  /*
      Max value of x = 100
      min value = 1
        all thread read x = 0 at the same time
        all threads (in parallel) compute 0 + 1 = 1
        all threads try to write x = 1
   */

  def minMaxX(): Unit = {
    var x = 0
    val threads = (1 to 100).map(_ => new Thread(() => x += 1))
  }

  // 3
  /*
      almost always message = "Scala is awesome"
      is it guaranteed? NO
      Obnoxious situation (possible):
        main thread:
        message = "Scala sucks"
        awesomeTrhead.start(
        sleep(1001) - yields execution
      awesome thread:
        sleep(1000) - yields execution
      OS gives the CPU to some important thread, takes > 2s
      OS gives the CPU back to main thread
      main thread:
        println(message) // Scala sucks
      awesome thread:
        message = "Scala is awesome"

   */
  def demoSleepFallacy(): Unit = {
      var message = ""
      val awesomeThread = new Thread(() => {
        Thread.sleep(1000)
        message = "Scala is awesome"
      })
      message = "Scala sucks"
      awesomeThread.start()
      Thread.sleep(1001)
      // Solution: join the worker thread
      awesomeThread.join()
      println(message)
    }


  def main(args: Array[String]): Unit = {
//    runInParallel()
//    demoBankingProblem()
    inceptionThreads(50).start()
  }
}
```

#### JVM Thread Communication

```scala
package lectures.part3async

object ThreadCommunication extends App {
  /*
    the producer-consumer problem

    producer -> [ x ] -> consumer
    // Problem: forcing threads to run actions in a guaranteed certain order

   */

  class SimpleContainer {
    private var value: Int = 0
    def isEmpty: Boolean = value == 0
    def set(newValue: Int) = value = newValue
    def get: Int = {
      val result = value
      value = 0
      result
    }
  }

  def naiveProdCons(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting...")
      while(container.isEmpty) {
        println("[consumer] actively waiting...")
      }
      println("[consumer] I have consumed " + container.get)
    })

    val producer = new Thread(() => {
      println("[producer] computing...")
      Thread.sleep(500)
      val value = 42
      println("[producer] I have produced, after long work the value " + value)
    })

    consumer.start()
    producer.start()
  }
  //naiveProdCons()

  // wait and notify

  //Synchronized
  // Entering a synchronized expression on an object locks the object:
  // Monitor: is a data structure internally used by JVM to keep track of which object is locked by which thread
//  val someObject = "hello"
//  someObject.synchronized { // lock the object's monitor
//    // code // Any other thread trying to run this will block
//  }

  // Only AnyRefs can have synchronized blocks
  // General principles:
  // - make no assumptions about who gets the lock first
  // - keep locking to a minimum
  // - maintain thread safety at ALL times in parallel applications

  //wait and notify()
  // wait() -ing on an object's monitor suspends you (the thread) indefinitely
  // Thread 1
//  val someObject1 = "hello"
//  someObject1.synchronized { // lock the object's monitor
//    // ... code part 1
//    someObject.wait() // release the lock and wait
//    // ... code part 2 // when allowed to proceed, lock the monitor again and continue
//  }
//
//  // Thread 2
//  someObject1.synchronized{ // lock the object's monitor
//    //... code
//    someObject1.notify() // signal ONE sleeping thread they may continue (which thread? You don't know) (use notifyAll to awaken ALL threads)
//    // ... more code
//  }

  // Waiting and notifying only work in synchronized expressions.


  //Smarted producer - Consumer

  def smartProdCons(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting ...")
      container.synchronized {
        container.wait()
      }

      // container must hace some value
      println("[consumer] I have consumed " + container.get)
    })

    val producer = new Thread(() => {
      println("[producer] Hard at work...")
      Thread.sleep(2000)
      val value = 42
      container.synchronized {
        println("[producer] I'm producing " + value)
        container.set(value)
        container.notify()
      }
    })
    consumer.start()
    producer.start()
  }

  smartProdCons()
}
```

#### Thread comunication Level 2
```scala
package lectures.part3async

import scala.collection.mutable
import scala.util.Random
object ThreadCommunication extends App {
  /*
    the producer-consumer problem

    producer -> [ x ] -> consumer
    // Problem: forcing threads to run actions in a guaranteed certain order

   */

  class SimpleContainer {
    private var value: Int = 0
    def isEmpty: Boolean = value == 0
    def set(newValue: Int) = value = newValue
    def get: Int = {
      val result = value
      value = 0
      result
    }
  }

  def naiveProdCons(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting...")
      while(container.isEmpty) {
        println("[consumer] actively waiting...")
      }
      println("[consumer] I have consumed " + container.get)
    })

    val producer = new Thread(() => {
      println("[producer] computing...")
      Thread.sleep(500)
      val value = 42
      println("[producer] I have produced, after long work the value " + value)
    })

    consumer.start()
    producer.start()
  }
  //naiveProdCons()

  // wait and notify

  //Synchronized
  // Entering a synchronized expression on an object locks the object:
  // Monitor: is a data structure internally used by JVM to keep track of which object is locked by which thread
//  val someObject = "hello"
//  someObject.synchronized { // lock the object's monitor
//    // code // Any other thread trying to run this will block
//  }

  // Only AnyRefs can have synchronized blocks
  // General principles:
  // - make no assumptions about who gets the lock first
  // - keep locking to a minimum
  // - maintain thread safety at ALL times in parallel applications

  //wait and notify()
  // wait() -ing on an object's monitor suspends you (the thread) indefinitely
  // Thread 1
//  val someObject1 = "hello"
//  someObject1.synchronized { // lock the object's monitor
//    // ... code part 1
//    someObject.wait() // release the lock and wait
//    // ... code part 2 // when allowed to proceed, lock the monitor again and continue
//  }
//
//  // Thread 2
//  someObject1.synchronized{ // lock the object's monitor
//    //... code
//    someObject1.notify() // signal ONE sleeping thread they may continue (which thread? You don't know) (use notifyAll to awaken ALL threads)
//    // ... more code
//  }

  // Waiting and notifying only work in synchronized expressions.


  //Smarted producer - Consumer

  def smartProdCons(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting ...")
      container.synchronized {
        container.wait()
      }

      // container must hace some value
      println("[consumer] I have consumed " + container.get)
    })

    val producer = new Thread(() => {
      println("[producer] Hard at work...")
      Thread.sleep(2000)
      val value = 42
      container.synchronized {
        println("[producer] I'm producing " + value)
        container.set(value)
        container.notify()
      }
    })
    consumer.start()
    producer.start()
  }

  // smartProdCons()

  /* Level 2: at the producer/consumer problem
    producer -> [? ? ?] -> consumer

    1 problem: producer and consumer may block each other

   */

  def prodConsLargeBuffer(): Unit = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]
    val capacity = 3
    val consumer = new Thread(() => {
      val random = new Random()
      while (true) {
        buffer.synchronized {
          if (buffer.isEmpty) {
            println("[consumer] buffer empty, waiting...")
            buffer.wait()
          }
          // there must be at least ONE value in the buffer
          val x = buffer.dequeue()
          println(s"[consumer] consumed ${x}")

          // todo
          buffer.notify()
          Thread.sleep(random.nextInt(500))
        }
      }
    })

    val producer = new Thread(() => {
      val random = new Random()
      var i = 0
      while(true) {
        buffer.synchronized {
          if (buffer.size == capacity) {
            println("[producer] buffer is full, waiting...")
            buffer.wait()
          }
          // there emust be at least ONE EMPTY SPACE in the buffer
          println(s"[producer] producing ${i}")
          buffer.enqueue(i)
          // todo
          buffer.notify()
          i+=1
        }
      }
    })
    consumer.start()
    producer.start()
  }

  prodConsLargeBuffer()
}
```

### Thread communication -- Multiple producers/consumers problem

```scala
package lectures.part3async

import scala.collection.mutable
import scala.util.Random
object ThreadCommunication extends App {
  /*
    the producer-consumer problem

    producer -> [ x ] -> consumer
    // Problem: forcing threads to run actions in a guaranteed certain order

   */

  class SimpleContainer {
    private var value: Int = 0
    def isEmpty: Boolean = value == 0
    def set(newValue: Int) = value = newValue
    def get: Int = {
      val result = value
      value = 0
      result
    }
  }

  def naiveProdCons(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting...")
      while(container.isEmpty) {
        println("[consumer] actively waiting...")
      }
      println("[consumer] I have consumed " + container.get)
    })

    val producer = new Thread(() => {
      println("[producer] computing...")
      Thread.sleep(500)
      val value = 42
      println("[producer] I have produced, after long work the value " + value)
    })

    consumer.start()
    producer.start()
  }
  //naiveProdCons()

  // wait and notify

  //Synchronized
  // Entering a synchronized expression on an object locks the object:
  // Monitor: is a data structure internally used by JVM to keep track of which object is locked by which thread
//  val someObject = "hello"
//  someObject.synchronized { // lock the object's monitor
//    // code // Any other thread trying to run this will block
//  }

  // Only AnyRefs can have synchronized blocks
  // General principles:
  // - make no assumptions about who gets the lock first
  // - keep locking to a minimum
  // - maintain thread safety at ALL times in parallel applications

  //wait and notify()
  // wait() -ing on an object's monitor suspends you (the thread) indefinitely
  // Thread 1
//  val someObject1 = "hello"
//  someObject1.synchronized { // lock the object's monitor
//    // ... code part 1
//    someObject.wait() // release the lock and wait
//    // ... code part 2 // when allowed to proceed, lock the monitor again and continue
//  }
//
//  // Thread 2
//  someObject1.synchronized{ // lock the object's monitor
//    //... code
//    someObject1.notify() // signal ONE sleeping thread they may continue (which thread? You don't know) (use notifyAll to awaken ALL threads)
//    // ... more code
//  }

  // Waiting and notifying only work in synchronized expressions.


  //Smarted producer - Consumer

  def smartProdCons(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting ...")
      container.synchronized {
        container.wait()
      }

      // container must hace some value
      println("[consumer] I have consumed " + container.get)
    })

    val producer = new Thread(() => {
      println("[producer] Hard at work...")
      Thread.sleep(2000)
      val value = 42
      container.synchronized {
        println("[producer] I'm producing " + value)
        container.set(value)
        container.notify()
      }
    })
    consumer.start()
    producer.start()
  }

  // smartProdCons()

  /* Level 2: at the producer/consumer problem
    producer -> [? ? ?] -> consumer

    1 problem: producer and consumer may block each other

   */

  def prodConsLargeBuffer(): Unit = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]
    val capacity = 3
    val consumer = new Thread(() => {
      val random = new Random()
      while (true) {
        buffer.synchronized {
          if (buffer.isEmpty) {
            println("[consumer] buffer empty, waiting...")
            buffer.wait()
          }
          // there must be at least ONE value in the buffer
          val x = buffer.dequeue()
          println(s"[consumer] consumed ${x}")

          // todo
          buffer.notify()
          Thread.sleep(random.nextInt(500))
        }
      }
    })

    val producer = new Thread(() => {
      val random = new Random()
      var i = 0
      while(true) {
        buffer.synchronized {
          if (buffer.size == capacity) {
            println("[producer] buffer is full, waiting...")
            buffer.wait()
          }
          // there emust be at least ONE EMPTY SPACE in the buffer
          println(s"[producer] producing ${i}")
          buffer.enqueue(i)
          // todo
          buffer.notify()
          i+=1
        }
      }
    })
    consumer.start()
    producer.start()
  }

  //prodConsLargeBuffer()

  /*
    Prod-cons, level 3
      producer1 -> [? ? ?] -> consumer1
      producer2 -> [? ? ?] -> consumer2
   */

  class Consumer(id: Int, buffer: mutable.Queue[Int]) extends Thread {
    override def run(): Unit = {
      val random = new Random()
      while (true) {
        buffer.synchronized {
          /*
           * Producer produces value, two Cons are waiting
           * Notifies ONE consumer, notifies on buffer
           * notifies the other consumer
           *
           */
          while (buffer.isEmpty) {
            println(s"[consumer $id] buffer empty, waiting...")
            buffer.wait()
          }
          // there must be at least ONE value in the buffer
          val x = buffer.dequeue()
          println(s"[consumer $id] consumed ${x}")

          // todo
          buffer.notifyAll()
          Thread.sleep(random.nextInt(500))
        }
      }
    }
  }

    class Producer(id: Int, buffer:mutable.Queue[Int], capacity: Int) extends Thread {
      val random = new Random()
      var i = 0
      while (true) {
        buffer.synchronized {
          while (buffer.size == capacity) {
            println("[producer] buffer is full, waiting...")
            buffer.wait()
          }
          // there must be at least ONE EMPTY SPACE in the buffer
          println(s"[producer] producing ${i}")
          buffer.enqueue(i)
          buffer.notifyAll()
          i += 1
        }
        Thread.sleep(random.nextInt(500))
      }
    }

  def multiProdCons(nConsumers: Int, nProducers: Int): Unit = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]
    val capacity = 12

    (1 to nConsumers).foreach( i => new Consumer(i, buffer).start())
    (1 to nProducers).foreach( i => new Producer(i, buffer, capacity).start())
  }

  //multiProdCons(3,6)

  /*
      Exercises.
      1) think of an example where notifyALL acts in a different way than notify?
      2) create a deadlock
      3) create a livelock
   */

    //notifyAll
  def testNotifyAll(): Unit = {
    val bell = new Object
    (1 to 10).foreach(i => new Thread(() => {
      bell.synchronized {
        println(s"[Thread ${i}] waiting")
        bell.wait()
        println(s"[thread ${i}] hooray!")
      }
    }).start())

    new Thread(() => {
      Thread.sleep(2000)
      println("[announcer] Rock'n roll")
      bell.synchronized{
        bell.notifyAll()
      }
    }).start()
  }
  //testNotifyAll()

  // 2 - deadlock
  case class Friend(name: String) {
    def bow(other: Friend): Unit = {
      this.synchronized {
        println(s"$this: I am bowing to my friend $other")
        other.rise(this)
        println(s"$this: my friend $other has risen ")
      }
    }

    def rise(other: Friend): Unit = {
      this.synchronized{
        println(s"$this: I am rising to my friend other")
      }
    }
    var side = "right"
    def switchSide(): Unit = {
      if (side == "right") side = "left"
      else side = "right"
    }

    // 3 - livelock
    def pass(other: Friend): Unit = {
      while (this.side == other.side){
        println(s"$this: Oh, but please, $other, feel free to pass...")
        switchSide()
        Thread.sleep(1000)
      }
    }
  }

  val sam = Friend("Sam")
  val pierre = Friend("Pierre")
  new Thread(() => sam.pass(pierre)).start()
  new Thread(() => pierre.pass(sam)).start()
}
```

### Futures and promises
Future[T] is a computation which will finish at some point

```scala
// a default ExecutionContext already implemented
import scala.concurrent.ExecutionContext.Implicits.global

val recipesFuture: Future[List[Recipe]] = Future {
   // some code that takes a long time to run
   jamieOliveerDb.getAll("chicken")
} // (global) execution context is passed implicitly
```
non-blocking processing
```scala
future.onComplete{ case Success(recipes) => ...}
```

Future is a monad 
- map, flatMap, filter, for-comprehensions

falling back
```scala
future.recover { case NotFoundException => ... }
```

blocking if need be
```scala
  val txStatus = Await.result(transaction, 1.seconds)
```

####Promise Pattern
Future are immutable, "read-only" objects.
Promises are "writable-once" containers over a future.

thread1: 
   - creates an empty promise
   - knows hoe to handle the result

thread 2:
   - holds the promise
   - fulfills or fails the promise

promise wraps a future 

```scala
val p = Promise[Int]()
val future = p.future

future.onComplete {
   case Success(value) =>
   case Failure(ex) =>
}
```

## Lectures - Part 4 - Implicits

### Enter Implicits

```scala
package lectures.part4implicits

object ImplicitsIntro extends App {
  val pair = "Daniel" -> "555"
  val intPair = 1 -> 2

  case class Person(name: String) {
    def greet = s"Hi, my name is $name!"
    implicit def fromStringToPerson(str: String): Person = Person(str)

    println("Peter".greet) //println(fromStringToPerson("Peter").greet

//    class A {
//      def greet: Int = 2
//    }
//    implicit def fromStringToA(str: String): A = new A

    // Implicit parameters
    def increment(x: Int)(implicit amount: Int) = x + amount
    implicit val defaultAmount = 10

    increment(2)
    // NOT default args
    // implicit values are found by the compiler


  }
}

```

### Organizing Implicits
#### Best practices
Where to define an implicit val:
1. if there is a single possible value for it and you can edit the code for the type; then, define the implicit in the companion
2. if there are many possible values for it but a single good one and you can edit the code for the type
   then define the good implicit in the companion
3. if there can be more than one good implicit, then package them separately and dev should import them as they see fits

```scala
package lectures.part4implicits

object OrganizingImplicits extends App {
  // Implicit vals defined here will take precedence over implicit values defined in scala.Predef
  implicit def reverseOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)
  //implicit val normalOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)
  println(List(1,4,5,3,2).sorted)

  // scala.Predef

  /*
    Potencial implicits values are (used as implicit parameters):
      - val/var
      - object
      - accessor methods = defs with no parentheses
   */

  // Exercise
  case class Person(name: String, age: Int)

  object Person {
    implicit val orderingPerson: Ordering[Person] = Ordering.fromLessThan((a, b) => a.name.compareTo(b.name) < 0)
  }
  implicit val ageOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.age < b.age)
  val persons = List (
    Person("Steve", 30),
    Person("Amy", 22),
    Person("John", 44),
    Person("Roberto", 35)
  )

  println(persons.sorted)

  /*
    Implicit scope (sorted from highest priority to lower
      - normal scope = LOCAL SCOPE
      - imported scope
      - companions of all types involved in the method signature
        def sorted[B > A](implicit ord: Ordering[B]): List[B]
        for this definition
          - List
          - Ordering
          - all the types involved = A or any supertype
   */


  /*
    Exercise.
      - totalPrice = most used (50%)
      - by unit count = 25%
      - by unit price = 25%


   */

  case class Purchase(nUnits: Int, unitPrice: Double)

  object Purchase {
    implicit val totalPriceOrdering: Ordering[Purchase] = Ordering.fromLessThan((a,b) => a.nUnits * a.unitPrice < b.nUnits * b.unitPrice)
  }
  object UnitCountOrdering {
    implicit val unitCountOrdering: Ordering[Purchase] = Ordering.fromLessThan((a,b) => a.nUnits < b.nUnits)
  }
  object UnitPriceOrdering {
    implicit val unitPriceOrdering: Ordering[Purchase] = Ordering.fromLessThan((a,b) => a.unitPrice < b.unitPrice)
  }
}
```

### Type Classes
```scala
package lectures.part4implicits

object TypeClasses extends App {
  trait HTMLWritable {
    def toHtml: String
  }

  case class User(name: String, age: Int, email: String) extends HTMLWritable {
    override def toHtml: String = s"<div>$name ($age yo) <a href=$email /> </div>"
  }

  User("John", 32, "john@hot.com").toHtml

  /*
    1 - only works for the types WE write
    2 - ONE implementation out of quite a number
   */

  // option 2 - pattern matching
  object HTMLSerializerPM {
    def serializeToHtml(value: Any) = value match {
      case User(n, a, e) =>
      case _ =>
    }
  }

  /*
    Disadvantages
    1. list type safety
    2 - need to modify the code every time
    3. still ONE implementation
   */

  trait HTMLSerializer[T] {
    def serialize(value: T): String
  }

  implicit object UserSerializer extends HTMLSerializer[User] {
    def serialize(user: User): String = s"<div>${user.name} (${user.age} yo) <a href=${user.email} /> </div>"
  }

  val john = User("John", 32, "john@rockthejvm.com")
  println(UserSerializer.serialize(john))

  //1 - we can define serializers for other types
  import java.util.Date
  object DateSerializer extends HTMLSerializer[Date] {
    override def serialize(date: Date): String = s"<div>${date.toString()}</div>"
  }

  //2 - we can define MULTIPLE serializers
  object PartialUserSerializer extends HTMLSerializer[User] {
    def serialize(user: User): String = s"<div>${user.name} </div>"
  }

  // TYPE CLASS
  // specify a set of operations that can be applied to a given type
  // So anyone who extends needs to provide the functionality defined in the type class
  // all implementors are called type instances

  trait MyTypeClassTemplate[T] {
    def action(value: T): String
  }

  object MyTypeClassTemplate {
    def apply[T](implicit instance: MyTypeClassTemplate[T]) = instance
  }

  /**
   * Equality type class
   */
  trait Equal[T] {
    def apply(a: T, b: T): Boolean
  }

  implicit object NameEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name
  }

  object FullEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name && a.email == b.email
  }

  // Part 2
  object HTMLSerializer {
    def serialize[T](value: T)(implicit serializer: HTMLSerializer[T]): String =
      serializer.serialize(value)

    def apply[T](implicit serializer: HTMLSerializer[T]) = serializer
  }

  implicit object IntSerializer extends HTMLSerializer[Int] {
    override def serialize(value: Int): String = s"<div style: color=blue>$value</div>"
  }

  println(HTMLSerializer.serialize(42))
  println(HTMLSerializer.serialize(john))
  // access to the entire type class interface
  println(HTMLSerializer[User].serialize(john))

  /*
    Exercise: implement the TC patter for the Equality tc.
   */
  object Equal {
    def apply[T](a: T, b: T)(implicit equalizer: Equal[T]): Boolean = equalizer.apply(a, b)
  }

  val anotherJohn = User("John", 45, "anotherJohn@hot.com")

  // Example of what is often called AD-HOC polymorphism
  println(Equal(john, anotherJohn))

}
```
