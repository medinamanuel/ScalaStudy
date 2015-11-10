package org.mmg.scala.study

import Functor._

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def apply[A,B](f: F[A => B]): F[A] => F[B]

  final def apply2[A,B](fa: F[A])(f: F[A => B]): F[B] =
     apply(f)(fa)

  override def fmap[A,B](f: A => B): F[A] => F[B] =
     apply(pure(f))
}

object Applicative {
  def pure[A, F[_]](a: A)(implicit applicative: Applicative[F]): F[A] =
    applicative pure a

  def apply[A,B,F[_]](fa: F[A])(f: F[A => B])(implicit applicative: Applicative[F]): F[B] =
    applicative.apply2(fa)(f)

  implicit object OptionApplicative extends Applicative[Option] {
    override def pure[A](a: A): Option[A] = Option(a)
    override def apply[A,B](f: Option[A => B]): Option[A] => Option[B] =
      o => for {
        a <- o
        p <- f
      } yield p(a)
  }
}


object Applicatives {

  import Applicative._

  // Arity 2
  def sum2NumbersPlus10(x: Int, y: Int) = x + y + 10

  def main(a: Array[String]): Unit = {


    //What happens here?
    println(fmap(Option(1))(sum2NumbersPlus10 _ curried))

    // So...
    //fmap(Option(2))(fmap(Option(1))(sum2NumbersPlus10 _ curried))

    // Not possible. fmap wants a pure function. What do we do?

    //println(apply(Option(1))(apply(Option(2))(pure(sum2NumbersPlus10 _ curried))))

    /* Too many parenthesis! Can we do better?
       Enter Scalaz's Applicatives
     */
    withScalaz

    // Example with class constructor:
    val theJavaWay = createInstanceOfMyClassTheJavaWay(2,"ja ja ja")
    if (theJavaWay != null)
      println(theJavaWay)

    createInstanceOfMyClassTheCoolWay(2, "ja ja ja") foreach println

    val theJavaWrong = createInstanceOfMyClassTheJavaWay(3,"")
    if (theJavaWrong != null)
      println(theJavaWrong)
    else
      println("Could not create an instance of MyClass - Java")

    createInstanceOfMyClassTheCoolWay(2,"xxx") match {
      case Some(myclass) => println(myclass)
      case _ => println("Could not create an instance of MyClass - Scala")
    }

  }

/*
Scalaz defines a lot of default implementations of Functors, Applicatives,
Monads, etc. for many Scala types. No need to reinvent the wheel.
 */
  def withScalaz = {
    import scalaz._
    import Scalaz._

    val f = (x: Int) => (y: Int) => x + y + 10
    println("With Scalaz:")
    println("Method 1: " + (Option(1) <*> (Option(2) <*> Option(f))))
    val b = f.point[Option]
    println("Method 1.5: " + (Option(1) <*> (Option(2) <*> b)))
    println("Method 2: " + (Option(1) |@| Option(2))(Function.uncurried(f)))
    val a = Apply[Option].lift2(sum2NumbersPlus10)
    println("Method 3: " + a(1.some, 2.some))

    println("More arguments :P")
    println(Option(2) <*> ((Option(3) <*> (Option(4) <*> b)) <*> b))
  }

  /* ******* An example ******** */
  case class MyClass(n: Int, s: String)

  def createInstanceOfMyClassTheJavaWay(n: Int, s: String) = {
    val a = theJavaWayInt(n)
    val b = theJavaWayString(s)

    if (a != null && b != null)
      MyClass(a.asInstanceOf[Int],b)
    else
      null
  }

  def theJavaWayInt(i: Int) =
    if (i % 2 == 0 && i > 0) i else null

  def theJavaWayString(s: String) =
    if (s.contains(" "))
      s.replaceAll(" ", "")
    else
      null

  /* The Applicative way*/


  import scalaz._
  import Scalaz._

  def createInstanceOfMyClassTheCoolWay(n: Int, s: String) = {
    (maybeAnInt(n) |@| maybeAString(s))(MyClass)
    //Apply[Option].lift2(MyClass)(maybeAnInt(n), maybeAString(s))
  }

  def maybeAnInt(i: Int) = if (i % 2 == 0 && i > 0) Some(i) else None
  def maybeAString(s: String) = {
    s.contains(" ") match {
      case true => Some(s.replaceAll(" ", ""))
      case false => None
    }
  }


}
