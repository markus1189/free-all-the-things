package de.codecentric

//snippet:boolean algebra
trait BoolAlgebra[A] {
  def tru: A
  def fls: A

  def not(value: A): A

  def and(lhs: A, rhs: A): A
  def or(lhs: A, rhs: A): A
}
//end

object BoolAlgebra {
  def apply[A](implicit B: BoolAlgebra[A]): BoolAlgebra[A] = B

  implicit val bool: BoolAlgebra[Boolean] = new BoolAlgebra[Boolean] {
    def tru: Boolean = true
    def fls: Boolean = false

    def not(value: Boolean): Boolean = !value

    def and(lhs: Boolean, rhs: Boolean): Boolean = lhs && rhs
    def or(lhs: Boolean, rhs: Boolean): Boolean = lhs || rhs
  }
}

object FreeBool {
  //snippet:free bool
  sealed abstract class FreeBool[+A]

  case object Tru extends FreeBool[Nothing]
  case object Fls extends FreeBool[Nothing]

  case class Not[A](value: FreeBool[A]) extends FreeBool[A]
  case class And[A](lhs: FreeBool[A], rhs: FreeBool[A]) extends FreeBool[A]
  case class Or[A](lhs: FreeBool[A], rhs: FreeBool[A]) extends FreeBool[A]
  case class Inject[A](value: A) extends FreeBool[A]
  //end

  //snippet:free bool interp
  def runFreeBool[A,B](f: A => B, fb: FreeBool[A])(implicit B: BoolAlgebra[B]): B = {
    fb match {
      case Tru => B.tru
      case Fls => B.fls
      case Inject(v) => f(v)
      case Not(v) => B.not(runFreeBool(f, v))
      case Or(lhs, rhs) => B.or(runFreeBool(f, lhs), runFreeBool(f, rhs))
      case And(lhs, rhs) => B.and(runFreeBool(f, lhs), runFreeBool(f, rhs))
    }
  }
  //end

  def lift[A](v: A): FreeBool[A] = FreeBool.Inject(v)
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

  val predicate = Not(And(Inject(Greater(5)), Inject(LessThan(10))))


  def evalInt(input: Int)(pred: FreeBool[Predicate]): Boolean = {
    def f(p: Predicate): Boolean = p match {
      case Greater(lowerBound) => input > lowerBound
      case LessThan(upperBound) => input < upperBound
      case Equals(expected) => input == expected
    }

    runFreeBool(f, pred)
  }
}
