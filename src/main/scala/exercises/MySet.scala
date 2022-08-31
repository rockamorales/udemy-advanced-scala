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