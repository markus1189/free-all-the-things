package de.codecentric

//snippet:monad typeclass
trait Monad[F[_]] {
  def pure[A](x: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}
//end

//snippet:free monad
sealed abstract class Free[F[_], A]

final case class Pure[F[_], A](a: A) extends Free[F, A]

final case class FlatMap[F[_], A, B](
  fa: Free[F, A], f: A => Free[F, B]) extends Free[F, B]
//end
