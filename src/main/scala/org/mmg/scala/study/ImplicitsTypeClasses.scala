package org.mmg.scala.study

import org.mmg.scala.study.Math.NumberLike

/**
 * Created by mmedina on 14/10/31.
 */


/* First definition. Only work with Doubles.
  What if we want these to work with Ints too?
 */
object Statistics {
  def median(xs: Vector[Double]): Double = xs(xs.size / 2)
  def quartiles(xs: Vector[Double]): (Double, Double, Double) =
    (xs(xs.size / 4), median(xs), xs(xs.size / 4 * 3))
  def iqr(xs: Vector[Double]): Double = quartiles(xs) match {
    case (lowerQuartile, _, upperQuartile) => upperQuartile - lowerQuartile
  }
  def mean(xs: Vector[Double]): Double = {
    xs.reduce(_ + _) / xs.size
  }
}

/*
If Double and Int had a common base class or implement def main(args: Array[String]){
common trait, we could generalize the code to that.

In Ruby: Monkey Patching

In functional programming; Type classes.

A type class defines behavior operations that a
certain type must support in order to be a member
of it.

The idea of type classes is somehow similar to
OOP interfaces.

 */

object Math {
  trait NumberLike[T] {
    def plus(x: T, y: T): T
    def divide(x: T, y: Int): T
    def minus(x:T, y:T): T
  }

  // Companion object
  object NumberLike {
    implicit object NumberLikeDouble extends NumberLike[Double] {
      def plus(x: Double, y: Double): Double = x + y
      def divide(x: Double, y:Int): Double = x / y
      def minus(x: Double, y:Double): Double = x - y
    }

    implicit object NumberLikeInt extends NumberLike[Int] {
      def plus(x: Int, y: Int): Int = x + y
      def divide(x: Int, y:Int): Int = x / y
      def minus(x: Int, y: Int): Int = x - y
    }

    implicit object NumberLikeSiTuple extends NumberLike[(String,Int)] {
      def plus(x: (String, Int), y: (String, Int)) = (x._1, x._2 + y._2)
      def divide(x: (String, Int), y: Int) = (x._1, x._2 / y)
      def minus(x: (String, Int), y: (String, Int)) = (x._1, x._2 - y._2)
    }

    implicit def NumberLikeOption[T](implicit ev: NumberLike[T]) = new NumberLike[Option[T]] {
     def plus(x: Option[T], y: Option[T]) = {
       println(x, y, x.isDefined, y.isDefined)
        if (!x.isDefined) y
        else if (!y.isDefined) x
        else {
          for {
            cx <- x
            cy <- y
          } yield (ev.plus(cx,cy))
        }
      }

      def minus(x: Option[T], y: Option[T]) = {
        if (x.isEmpty) y
        else if (y.isEmpty) x
        else {
          for {
            cx <- x
            cy <- y
          } yield (ev.minus(cx,cy))
        }
      }

      def divide(x: Option[T], y: Int) = {
        if (x.isEmpty) None
        else if (y == 0) None
        else {
          x map {ev.divide(_,y)}
        }
      }
    }

  }

}

object Statistics2 {
  import Math.NumberLike

  /* Without implicit, we need to add always the ev parameter when calling this function.
     Remember that the compiler will look for implicits first in scope and then in the
     companion object of the implicit parameter.
   */

  def mean[T](xs: Vector[T])(implicit ev: NumberLike[T]): T =
    ev.divide(xs.reduce(ev.plus(_,_)), xs.size)

}

object Statistics3 {
  import Math.NumberLike
  def mean[T](xs: Vector[T])(implicit ev: NumberLike[T]): T =
    ev.divide(xs.reduce(ev.plus(_, _)), xs.size)
  def median[T : NumberLike](xs: Vector[T]): T = xs(xs.size / 2)
  def quartiles[T: NumberLike](xs: Vector[T]): (T, T, T) =
    (xs(xs.size / 4), median(xs), xs(xs.size / 4 * 3))
  def iqr[T: NumberLike](xs: Vector[T]): T = quartiles(xs) match {
    case (lowerQuartile, _, upperQuartile) =>
      implicitly[NumberLike[T]].minus(upperQuartile, lowerQuartile)
  }
}

object ImplicitsTypeClasses {

  import Math.NumberLike._
  def calculateDifference[T](a: T, b: T)(implicit nLikeT: NumberLike[T]) = nLikeT.minus(a,b)

  def reduce[T](l: List[T])(implicit nLikeT: NumberLike[T]) = l reduce nLikeT.plus

  def main(a: Array[String]): Unit = {
    val numbers = Vector[Double](13, 23.0, 42, 45, 61, 73, 96, 100, 199, 420, 900, 3839)
    println(Statistics2.mean(numbers))

    val integers = Vector[Int](2,4,5,6,7,8)
    println(Statistics2.mean(integers))

    val tuples = Vector[(String, Int)](("ja",1), ("jo", 34), ("ju", 199))
    println(Statistics2.mean(tuples))

    val options = Vector[Option[Int]](Some(1), None)
    println(Statistics2.mean(options))

    val x: Option[Int] = Some(3)
    val y: Option[Int] = Some(1)
    val z: Option[Int] = Some(6)

    println(calculateDifference(x, y))
    println(reduce(List(x,y,z)))
    println(reduce(List(1,2,3)))
    println(reduce(List(("ja",1), ("jo", 34), ("ju", 199))))
/*
    import Statistics._
    import JodaImplicits._
    import org.joda.time.Duration._

    val durations = Vector(standardSeconds(20), standardSeconds(57), standardMinutes(2),
      standardMinutes(17), standardMinutes(30), standardMinutes(58), standardHours(2),
      standardHours(5), standardHours(8), standardHours(17), standardDays(1),
      standardDays(4))
    println(Statistics2.mean(durations).getStandardHours)*/
  }

/*
We can define implementations of type classes for
existing types
 */

  /*object JodaImplicits {
    import Math.NumberLike
    import org.joda.time.Duration
    implicit object NumberLikeDuration extends NumberLike[Duration] {
      def plus(x: Duration, y: Duration): Duration = x.plus(y)
      def divide(x: Duration, y: Int): Duration = Duration.millis(x.getMillis / y)
      def minus(x: Duration, y: Duration): Duration = x.minus(y)
    }
  }*/



}