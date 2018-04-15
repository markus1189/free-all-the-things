package de.codecentric

//snippet:applicative typeclass
trait Applicative[F[_]] {
  def pure[A](x: A): F[A]
  def ap[A, B](fab: F[A => B], fa: F[A]): F[B]
}
//end

object FreeAp {
  //snippet:free applicative
  sealed abstract class FreeAp[F[_], A]

  final case class Pure[F[_], A](a: A) extends FreeAp[F, A]

  final case class Ap[F[_], A, B](fab: FreeAp[F, A => B], fa: FreeAp[F, A]) extends FreeAp[F, B]
  //end
}
