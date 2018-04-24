package de.codecentric

//snippet:monad typeclass
trait Monad[F[_]] {
  def pure[A](x: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}
//end

object Monad {
  def apply[F[_]](implicit M: Monad[F]) = M
}

object FreeMonad {
  //snippet:free monad
  sealed abstract class Free[F[_], A]

  final case class Pure[F[_], A](a: A) extends Free[F, A]

  final case class FlatMap[F[_], A, B](fa: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  final case class Inject[F[_], A](fa: F[A]) extends Free[F, A]
  //end
}

object FreeInstance {
  import FreeMonad._

  implicit def freeMonadFunctor[F[_]]: Functor[Free[F, ?]] = new Functor[Free[F, ?]] {
    def map[A, B](fa: Free[F, A])(f: A => B): Free[F, B] = Monad[Free[F, ?]].flatMap(fa)(a => Pure(f(a)))
  }

  //snippet:free instance
  implicit def freeMonad[F[_], A]: Monad[Free[F, ?]] =
    new Monad[Free[F, ?]] {
      def pure[A](x: A): Free[F, A] = Pure(x)

      def flatMap[A, B](fa: Free[F, A])(
        f: A => Free[F, B]): Free[F, B] = FlatMap(fa, f)
    }
  //end
}

object FreeInstanceOpt {
  import FreeMonad._
  //snippet:opt free instance
  implicit def freeMonadOpt[F[_]: Functor, A]: Monad[Free[F, ?]] =
    new Monad[Free[F, ?]] {
      def pure[A](x: A): Free[F, A] = Pure(x)

      def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa match {
        case Pure(x) => f(x)
        case Inject(fa) => FlatMap(Inject(fa), f)
        case FlatMap(ga, g) =>
          FlatMap(ga, (a: Any) => FlatMap(g(a), f))
      }
    }
  //end
}

object FreeSugar {
  import FreeMonad._
  import FreeInstance._
  final class FreeOps[F[_], A](self: Free[F, A]) {
    def flatMap[B](f: A => Free[F, B]) = Monad[Free[F, ?]].flatMap(self)(f)
  }
}

object FreeInterpreter {
  import FreeMonad._
  abstract class FunctionK[F[_], G[_]] {
    def apply[A](input: F[A]): G[A]
  }

  //snippet:free interp
  def runFree[F[_], M[_]:Monad, A](
    nat: FunctionK[F, M])(free: Free[F, A]): M[A] = free match {
    case Pure(x) => Monad[M].pure(x)
    case Inject(fa) => nat(fa)
    case FlatMap(fa, f) =>
      Monad[M].flatMap(runFree(nat)(fa))(x => runFree(nat)(f(x)))
  }
  //end
}

object WhatAboutLaws {
  import FreeMonad._
  type Id[A] = A
  def example(f: Int => Free[Id, Int], g: Int => Free[Id, Int], fa: Free[Id, Int]) = {
    //snippet:what about laws
    val exp1 = FlatMap(FlatMap(fa, f), g)
    val exp2 = FlatMap(fa, (a: Int) => FlatMap(f(a), g))

    exp1 != exp2
    //end
  }
}
