package de.codecentric

//snippet:functor typeclass
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
//end

object FreeFunctor1 {
  //snippet:free functor
  sealed abstract class FreeFunctor[F[_], A]
  case class Fmap[F[_], X, A](fa: F[X])(f: X => A) extends FreeFunctor[F, A]
  //end
}
