package org.mmg.scala.study

import scalaz._
import Scalaz._

/*
  Monads are entities composed by:
  - A type constructor that defines how to obtain a corresponding
    monadic type.
  - A unit function that maps a value in a underlying type to
    the corresponding monadic type.
  - A binding operation that allows to combine values in a monadic type
    with functions that expect values in the underlying type.
    That is:

     F a => (a => F[B]) => F[B]

     So:

     Functors:
       F[A] => (A => B) : F[A] => F[B]

     Applicatives:
       F[A] => F[A => B] : F[A] => F[B]

    Monads:
      F[A] => (A => F[B]) : F[A] => F[B]

 */

trait Monad[M[_]] {
  def unit[A](a: A): M[A]
  def bind[A,B](f: A => M[B]): M[A] => M[B]

  def bind[A,B](fa: M[A])(f: A => M[B]): M[B] =
    bind(f)(fa)

}

object Monad {
  def unit[A, M[_]](a: A)(implicit monad: Monad[M]): M[A] =
     monad unit a

  final def bind[A,B,M[_]](fa: M[A])(f: A => M[B])(implicit monad: Monad[M]): M[B] =
    monad.bind(fa)(f)

  implicit object OptionMonad extends Monad[Option] {
    override def unit[A](a: A): Option[A] = Option(a)
    override def bind[A,B](f: A => Option[B]): Option[A] => Option[B] =
      o => o flatMap f
  }

  implicit object ListMonad extends Monad[List] {
    override def unit[A](a: A): List[A] = List(a)
    override def bind[A,B](f: A => List[B]): List[A] => List[B] =
      l => l flatMap f
  }
}

object Monads {

  import Monad._

  def plusOne(x: Int) = Option(x + 1)

  def plusTwo(x: Int) = Option(x + 2)

  def divide(x: Int, y: Int) = if (y == 0) None else Some(x/y)

  def main(a: Array[String]): Unit = {

    val opA = divide(15,3)

    val opB = bind(opA)(plusOne)

    opB foreach println

    val opD = bind(divide(15,0))(plusOne)

    println(opD.getOrElse("Nothing here"))

/*
    testLaw1
    testLaw2
    testLaw3
*/

    /*testLawsWithScalaAndScalaz

    println(withForComprehension(15,3))
    println(withForComprehension(19,0))*/

  }


  /*Monads must follow some laws:

  - (unit a) bind f == f a
  - m bind unit == m
  - (m bind f) bind g == m bind {x => ( (f x) bind g)}

  */

  def testLaw1(implicit monad: Monad[Option]) =
    println(bind(monad.unit(5))(plusOne) == plusOne(5))

  def testLaw2(implicit monad: Monad[Option]) =
    println(bind(5.some)(monad.unit) == Some(5))

  def testLaw3(implicit monad: Monad[Option]) = {
    val res1 = bind(bind(monad.unit(15))(plusOne))(plusTwo)
    val res2 = bind(monad.unit(15)){x => bind(plusOne(x))(plusTwo)}

    println(res1 == res2)
  }

  /*
    Too many parenthesis and difficult to read?
    Of course we can do better!

    Scala:
      - unit = Class Constructor (Option, List, etc.)
      - bind = flatMap

    Scalaz:
      - unit = point
      - bind = >>=

    Haskell:
      - unit = return
      - bind = >>=


    So:
   */

  def testLawsWithScalaAndScalaz = {
    testLaw1Scalaz
    testLaw2Scala
    testLaw3Scalaz
  }

  def testLaw1Scalaz =
    println((5.point[Option] >>= plusOne) == plusOne(5))

  def testLaw2Scala =
    println((Some(5) flatMap {Option(_)}) == Some(5))

  def testLaw3Scalaz = {
    val res1 = (15.some >>= plusOne) >>= plusTwo
    val res2 = 15.some >>= {x => plusOne(x) >>= plusTwo}

    println(res1 == res2)
  }

  /*
   Remember for-comprehensions?
   That's just syntactic sugar for bind (flatMap)
   */

  def withForComprehension(x: Int, y: Int) = for {
    a <- divide(x,y)
    b <- plusOne(a)
  } yield b

  /*
   .NET uses monadic code too.
   Ever heard of LINQ?
   (Language Integrated Query):
   Select = map
   SelectMany = flatMap = bind

   This is C#:

   public class PhoneNumber
{
    public string Number { get; set; }
}

public class Person
{
    public IEnumerable<PhoneNumber> PhoneNumbers { get; set; }
}

IEnumerable<Person> people = new List<Person>();

// Select gets a list of lists of phone numbers
IEnumerable<IEnumerable<PhoneNumber>> phoneLists = people.Select(p => p.PhoneNumbers);

// SelectMany flattens it to just a list of phone numbers.
IEnumerable<PhoneNumber> phoneNumbers = people.SelectMany(p => p.PhoneNumbers);


Or:

var phoneLists = from p in people Select p.PhoneNumbers
var phoneNumbers = from p in people SelectMany p.phoneNumbers
   */



}
