package de.codecentric

//snippet:functor typeclass
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
//end

object Functor {
  def apply[F[_]](implicit F: Functor[F]): Functor[F] = F
}

object FreeFunctor1 {
  //snippet:free functor1
  sealed abstract class FreeFunctor[F[_], A]

  case class Fmap[F[_], X, A](fa: F[X])(f: X => A) extends FreeFunctor[F, A]

  case class Inject[F[_], A](fa: F[A]) extends FreeFunctor[F, A]
  //end
}

object FreeFunctor2 {
  //snippet:free functor2
  sealed abstract class FreeFunctor[F[_], A]

  case class Fmap[F[_], X, A](fa: F[X])(f: X => A) extends FreeFunctor[F, A]

  def inject[F[_], A](value: F[A]) = Fmap(value)(identity)
  //end
}

object FreeFunctor3 {
  //snippet:free functor3
  sealed abstract class Fmap[F[_], A] {
    type X
    def fa: F[X]
    def f: X => A
  }

  def inject[F[_], A](v: F[A]) = new Fmap[F, A] {
    type X = A
    def fa = v
    def f = identity
  }
  //end
}

object FreeFunctor4 {
  //snippet:free functor4
  sealed abstract class Coyoneda[F[_], A] {
    type X
    def fa: F[X]
    def f: X => A
  }

  def inject[F[_], A](v: F[A]) = new Coyoneda[F, A] {
    type X = A
    def fa = v
    def f = identity
  }
  //end

  //snippet:functor coyoneda
  implicit def coyoFun[F[_]]: Functor[Coyoneda[F, ?]] = new Functor[Coyoneda[F, ?]] {
    def map[A, B](coyo: Coyoneda[F, A])(g: A => B): Coyoneda[F, B] = new Coyoneda[F, B] {
      type X = coyo.X
      def fa = coyo.fa
      def f = g.compose(coyo.f)
    }
  }
  //end

  //snippet:coyoneda interp
  def runCoyo[F[_]:Functor, A](coyo: Coyoneda[F, A]): F[A] = Functor[F].map(coyo.fa)(coyo.f)
  //end

}
