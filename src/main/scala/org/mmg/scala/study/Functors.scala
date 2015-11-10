package org.mmg.scala.study


/*
   ->>[_,_] = 2 generic parameters
   ->>>[_,_] = Another 2 generic parameters
   F[_] = The functor
 */
trait GenericFunctor[->>[_, _], ->>>[_, _], F[_]] {
  def fmap[A, B](f: ->>[A,B]): ->>>[F[A],F[B]]
}

/* Endofunctor: source and target categories are the same
   (Scala types and Scala functions).

    Function is defined as Function1[A,B], that is,
    the mapping of something to something
 */
trait Functor[F[_]] extends GenericFunctor[Function,Function, F] {
  /* map  A -> B to F[A] -> F[B]
     take as (F[A]) and apply the lifted function to it so we get F[B]
   */
  final def fmap[A,B] (as: F[A])(f: A => B): F[B] = fmap(f)(as)
}

object Functor {
  // implicit type so we don't have to write it every time.
  def fmap[A,B,F[_]](as: F[A])(f: A => B)(implicit functor: Functor[F]): F[B] =
    functor.fmap(as)(f)

  //some implementations
  implicit object ListFunctor extends Functor[List] {
    def fmap[A,B](f: A => B): List[A] => List[B] = as => as map f
  }

  implicit object OptionFunctor extends Functor[Option] {
    def fmap[A,B](f: A => B): Option[A] => Option[B] = o => o map f
  }

}

object Functors {
  import Functor._

  def main(a: Array[String]): Unit = {
    println(fmap(List(1,2,3))(x => x + 1))
    println(fmap(Option(5))(_ * 2))
  }
}
