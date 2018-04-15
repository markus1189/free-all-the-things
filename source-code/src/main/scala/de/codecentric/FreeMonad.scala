package de.codecentric

//snippet:monad typeclass
trait Monad[F[_]] {
  def pure[A](x: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}
//end

object FreeMonad {
  //snippet:free monad
  sealed abstract class Free[F[_], A]

  final case class Pure[F[_], A](a: A) extends Free[F, A]

  final case class FlatMap[F[_], A, B](
    fa: Free[F, A], f: A => Free[F, B]) extends Free[F, B]
  //end
}


object FreeInstance {
  import FreeMonad._
  //snippet:free instance
  implicit def freeMonad[F[_], A]: Monad[Free[F, ?]] =
    new Monad[Free[F, ?]] {
      def pure[A](x: A): Free[F, A] = Pure(x)

      def flatMap[A, B](fa: Free[F, A])(
        f: A => Free[F, B]): Free[F, B] = FlatMap(fa, f)
    }
  //end
}

object FreeInterpreter {
  import FreeMonad._
  abstract class FunctionK[F[_], G[_]] {
    def apply[A](input: F[A]): G[A]
  }

  //snippet:free interp
  def runFree[F[_], M[_]:Monad, A](
    nat: FunctionK[F, M]): Free[F, A] => M[A] = ???
  //end
}
