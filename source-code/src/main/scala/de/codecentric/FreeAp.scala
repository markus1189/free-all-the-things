package de.codecentric

//snippet:applicative typeclass
trait Applicative[F[_]] {
  def pure[A](x: A): F[A]

  def ap[A, B](fab: F[A => B], fa: F[A]): F[B]
}
//end

object Applicative {
  def apply[F[_]](implicit A: Applicative[F]) = A
}

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

object FreeApInstanceOpt {
  import FreeAp._
  implicit def freeApApplicativeOpt[F[_]]: Applicative[FreeAp[F, ?]] = new Applicative[FreeAp[F, ?]] {
    def pure[A](x: A) = Pure(x)

  //snippet:opt instance freeap
    def ap[A, B](fab: FreeAp[F, A => B], fa: FreeAp[F, A]): FreeAp[F, B] = (fab, fa) match {
      case (Pure(f), Pure(x)) => Pure(f(x)) // homomorphism
      case (u, Pure(y)) => Ap(Pure((f: A => B) => f(y)), u) // interchange
      case (_,_) => Ap(fab, fa)
    }
  //end
  }
}

object FreeApInterp {
  import FreeAp._
  //snippet:freeap interp
  def runFreeAp[F[_], M[_]:Applicative, A](nat: F ~> M)(free: FreeAp[F, A]): M[A] = free match {
    case Pure(x) => Applicative[M].pure(x)
    case Inject(fa) => nat(fa)
    case Ap(fab, fa) => Applicative[M].ap(runFreeAp(nat)(fab), runFreeAp(nat)(fa))
  }
  //end
}
