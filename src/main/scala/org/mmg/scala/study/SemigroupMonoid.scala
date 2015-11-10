package org.mmg.scala.study

trait Semigroup[T] {
  def append(a: T, b: T) : T
}

trait Monoid[T] extends Semigroup[T] {
  def zero : T
}

/*
Three monoid laws:
  - For all a,b,c in M, (a op b) op c = a op (b op c) holds
  - Identity element: e op a = a op e = a
  - Closure: For all a,b in M, a op b also belongs to M
 */

object MonoidDefinitions {
  implicit def mapMonoid[A,B](implicit ev: Monoid[B]) : Monoid[Map[A,B]] = new Monoid[Map[A,B]] {

    def append(a: Map[A,B], b: Map[A,B]): Map[A,B] = {
      val z = ev.zero

      (a.keys ++ b.keys).foldLeft(zero){case (newMap, key) =>
        newMap + (key -> ev.append(a.getOrElse(key,z), b.getOrElse(key,z)))
      }

    }

    def zero = Map[A,B]()
  }

  implicit def intMonoid : Monoid[Int] = new Monoid[Int] {
    def append(a: Int, b: Int) = a + b
    def zero = 0
  }


}


object SemigroupMonoid {

  import MonoidDefinitions._

  def sum[T](m1: T, m2: T)(implicit ev: Monoid[T]) = ev.append(m1,m2) // m1 |+| m2

  def main(a: Array[String]): Unit = {
    val m1 = Map("a" -> 1, "b" -> 2, "c" -> 3)
    val m2 = Map("a" -> 4, "c" -> 14, "d" -> 5)
    //val m3 = Map("a" -> 12.2, "c" -> 14.2, "d" -> 5.2)

    sum(m1,m2) foreach println
    println(sum(34,45))
    //sum(m1,m3) foreach println


  }


}