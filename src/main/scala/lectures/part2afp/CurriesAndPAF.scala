package lectures.part2afp

object CurriesAndPAF extends App{
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



}
