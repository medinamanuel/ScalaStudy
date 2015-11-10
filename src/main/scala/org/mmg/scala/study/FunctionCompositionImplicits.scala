package org.mmg.scala.study

/**
 * Created by mmedina on 14/10/06.
 */


object FunctionCompositionImplicits {
  /*
  Given two functions f and g, we can compose them into
  a new function.

  Scala has two ways of doing this:
  compose
  andThen
   */

  def toInt(s: String) = s.toInt
  def addOne(i: Int) = i + 1
  def by4(i: Int) = i * 4

  /*
    With compose:
    composed1 = addOne(toInt(x))
   */
  val composed1 = addOne _ compose toInt

  /*
    With andThen
    composed2 = addOne(toInt(x))
   */
  val composed2 = toInt _ andThen addOne

  /*
  Be careful with the order.
  These two are not the same.
   */
  val composed3 = addOne _ compose by4
  val composed4 = addOne _ andThen by4

  // Scalaz
  /*addOne _ >>> by4*/

  /*
  Remember the concept of function composition, as
  it is used in higher abstractions.
   */


  /* Implicits
   When defining functions, you can define the last argument as implicit
   and Scala will search for implicit values to fill it automatically.
   */

  implicit val naruto = "dattebayo!"

  def printMessage(msg: String)(implicit suffix: String) = println("%s %s".format(msg,suffix))

  /* Implicits are type-safe. The following code won't compile
     because there is no appropriate implicit in scope.
    */

  //def addSomething(a: Int, b: Int)(implicit c: Int) = a + b + c

  /*
     Also, you can't have more than one implicit value of the same static type, because
     the compiler can't decide which one it should use.
   */

  //implicit val lumChan = "ccha"

  // You can be specific in the types
  def doSomething(implicit l: List[Int]) = println(l.reduce{_ + _})


  // Or parametrize them
  //def sayThings[T](implicit l: List[T]) = l foreach println

  implicit val listOfSomething = List("Hello world!")
  implicit val listOfInts = (1 to 10) toList

  /*
     By using implicits, you can pimp types
   */

  // Without this line, anotherAddOne does not compile
  implicit def toIncrementableInteger(x: Int) = new IncrementableInteger(x)

  def anotherAddOne(x: Int) = x.inc

  /*
  Scala searches for implicits in:
   - Current local scope
   - Companion objects of imported classes
   */

  def main(a: Array[String]): Unit = {
    val strFunctions = List(composed1, composed2)
    val intFunctions = List(composed3, composed4)

    strFunctions foreach (f => println(f("3")))
    intFunctions foreach (f => println(f(4)))

    printMessage("I am a ninja")

    // You can explicitly pass the argument
    printMessage("I am not a ninja")("kerokero")

    /*addSomething(1,2)

    sayThings

    doSomething*/

    // You can access what's in the implicit context
    // This will search for an implicit List[String]
    implicitly[List[String]].foreach(println)
    implicitly[List[String]] foreach println

    println(anotherAddOne(234))
    println(1.inc)

  }

}

class IncrementableInteger(x: Int) {
  def inc = x + 1
}