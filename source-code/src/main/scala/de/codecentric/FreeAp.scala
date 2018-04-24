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

  final case class Inject[F[_], A](fa: F[A]) extends FreeAp[F, A]
  //end
}

object FreeApInstance {
  import FreeAp._
  //snippet:instance freeap
  implicit def freeApApplicative[F[_]]: Applicative[FreeAp[F, ?]] = new Applicative[FreeAp[F, ?]] {
    def pure[A](x: A) = Pure(x)

    def ap[A, B](fab: FreeAp[F, A => B], fa: FreeAp[F, A]): FreeAp[F, B] = Ap(fab, fa)
  }
  //end
}
