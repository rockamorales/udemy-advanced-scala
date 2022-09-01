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
