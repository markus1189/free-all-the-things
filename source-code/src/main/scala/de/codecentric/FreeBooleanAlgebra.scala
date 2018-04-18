package de.codecentric

//snippet:boolean algebra
trait BooleanAlgebra[A] {
  def tru: A
  def fls: A

  def not(value: A): A

  def and(lhs: A, rhs: A): A
  def or(lhs: A, rhs: A): A
}
//end

object BooleanAlgebra {
  def apply[A](implicit B: BooleanAlgebra[A]): BooleanAlgebra[A] = B

  implicit val bool: BooleanAlgebra[Boolean] = new BooleanAlgebra[Boolean] {
    def tru: Boolean = true
    def fls: Boolean = false

    def not(value: Boolean): Boolean = !value

    def and(lhs: Boolean, rhs: Boolean): Boolean = lhs && rhs
    def or(lhs: Boolean, rhs: Boolean): Boolean = lhs || rhs
  }
}

object FreeBool {
  sealed abstract class FreeBool[+A]

  final case object Tru extends FreeBool[Nothing]
  final case object Fls extends FreeBool[Nothing]

  final case class Not[A](value: FreeBool[A]) extends FreeBool[A]

  final case class And[A](lhs: FreeBool[A], rhs: FreeBool[A]) extends FreeBool[A]
  final case class Or[A](lhs: FreeBool[A], rhs: FreeBool[A]) extends FreeBool[A]

  final case class Gen[A](value: A) extends FreeBool[A]

  def interp[A,B:BooleanAlgebra](f: A => B, fb: FreeBool[A]): B = fb match {
    case Not(v) => BooleanAlgebra[B].not(interp(f, v))
    case And(lhs, rhs) => BooleanAlgebra[B].and(interp(f, lhs), interp(f, rhs))
    case Or(lhs, rhs) => BooleanAlgebra[B].or(interp(f, lhs), interp(f, rhs))
    case Tru => BooleanAlgebra[B].tru
    case Fls => BooleanAlgebra[B].fls
    case Gen(v) => f(v)
  }
}

sealed trait Predicate
case class Greater(value: Int) extends Predicate
case class LessThan(value: Int) extends Predicate
case class Equals(value: Int) extends Predicate

trait Monoid[A] {
  def empty: A
  def combine(a: A, b: A): A
}

object FreeMonoid {
  sealed trait FM[+A]
  case object Empty extends FM[Nothing]
  case class Combine[A](lhs: A, rhs: FM[A]) extends FM[A]
}

/*
Definition: free bool algebra. A free bool algebra on type Int is an
object F[Int] such that F is a boolean algebra, and a function i ::
Int -> F[Int] such that any other boolean algebra S and a function f
:: Int -> S there exists a unique boolean algebra morphism f' such
that f' . i = f.
 */
