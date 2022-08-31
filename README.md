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
