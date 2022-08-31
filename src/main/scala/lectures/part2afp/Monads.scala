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
