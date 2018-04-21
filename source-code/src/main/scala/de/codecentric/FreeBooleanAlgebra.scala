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

  def lift[A](v: A): FreeBool[A] = FreeBool.Gen(v)
  def and[A](lhs: FreeBool[A], rhs: FreeBool[A]): FreeBool[A] = And(lhs, rhs)
  def or[A](lhs: FreeBool[A], rhs: FreeBool[A]): FreeBool[A] = Or(lhs, rhs)
  def not[A](fb: FreeBool[A]): FreeBool[A] = Not(fb)
  val tru: FreeBool[Nothing] = Tru
  val fls: FreeBool[Nothing] = Fls
}

sealed trait Predicate
case class Greater(value: Int) extends Predicate
case class LessThan(value: Int) extends Predicate
case class Equals(value: Int) extends Predicate

object FreeBoolDsl {
  import FreeBool._

  val predicate = Not(And(Gen(Greater(5)), Gen(LessThan(10))))


  def evalInt(input: Int)(pred: FreeBool[Predicate]): Boolean = {
    def f(p: Predicate): Boolean = p match {
      case Greater(lowerBound) => input > lowerBound
      case LessThan(upperBound) => input < upperBound
      case Equals(expected) => input == expected
    }

    interp(f, pred)
  }
}
