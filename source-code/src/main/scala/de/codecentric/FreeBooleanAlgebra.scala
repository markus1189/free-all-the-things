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

  def inject[A](v: A): FreeBool[A] = FreeBool.Inject(v)
  def and[A](lhs: FreeBool[A], rhs: FreeBool[A]): FreeBool[A] = And(lhs, rhs)
  def or[A](lhs: FreeBool[A], rhs: FreeBool[A]): FreeBool[A] = Or(lhs, rhs)
  def not[A](fb: FreeBool[A]): FreeBool[A] = Not(fb)
  val tru: FreeBool[Nothing] = Tru
  val fls: FreeBool[Nothing] = Fls
}

object FreeBoolSugar {
  import FreeBool._
  implicit def toFreeBoolOps[A](v: FreeBool[A]) = new FreeBoolOps(v)

  sealed class FreeBoolOps[A](self: FreeBool[A]) {
    def &(other: FreeBool[A]): FreeBool[A] = And(self, other)
    def |(other: FreeBool[A]): FreeBool[A] = Or(self, other)
    def unary_! : FreeBool[A] = Not(self)
  }

}

object FreeBoolDsl {
  import FreeBool._
  import FreeBoolSugar._

  //snippet:predicate
  sealed trait Predicate
  case class Greater(value: Int) extends Predicate
  case class LessThan(value: Int) extends Predicate
  case class Equals(value: Int) extends Predicate

  def greaterThan(i: Int): FreeBool[Predicate] = Inject(Greater(i))
  def lessThan(i: Int): FreeBool[Predicate] = Inject(LessThan(i))
  def equals(i: Int): FreeBool[Predicate] = Inject(Equals(i))

  def oneOf(is: List[Int]) = is.foldLeft(Fls: FreeBool[Predicate])((acc,i) => acc | equals(i))
  //end

  def positive = greaterThan(0)

  //snippet:example predicate
  val predicate: FreeBool[Predicate] = !(greaterThan(5) & lessThan(10)) & !(oneOf(List(42, 1337, 0)))
  //end

  def evalInt(pred: FreeBool[Predicate])(input: Int): Boolean = {
    def f(p: Predicate): Boolean = p match {
      case Greater(lowerBound) => input > lowerBound
      case LessThan(upperBound) => input < upperBound
      case Equals(expected) => input == expected
    }

    runFreeBool(f, pred)
  }

  val result = evalInt(predicate)(7)
}

object FreeBoolPartial extends App {
  import FreeBoolDsl._
  import FreeBool._
  import FreeBoolSugar._

  sealed trait Predicate
  case class OlderThan(i: Int) extends Predicate
  case class MinSalary(i: Int) extends Predicate
  case class HasSkill(s: String) extends Predicate

  def olderThan(i: Int): FreeBool[Predicate] = Inject(OlderThan(i))
  def minSalary(i: Int): FreeBool[Predicate] = Inject(MinSalary(i))
  def hasSkill(s: String): FreeBool[Predicate] = Inject(HasSkill(s))

  def runFreeBoolP[A,B](f: A => Option[B], fb: FreeBool[A])(implicit B: BoolAlgebra[B]): Either[FreeBool[A], B] = {
    fb match {
      case Tru => Right(B.tru)
      case Fls => Right(B.fls)
      case Inject(v) => f(v).toRight(fb)
      case Not(v) => runFreeBoolP(f, v).map(B.not(_))
      case Or(lhs, rhs) =>
        val l = runFreeBoolP(f, lhs)
        val r = runFreeBoolP(f, rhs)

        (l, r) match {
          case (Right(a), Right(b)) => Right(B.or(a,b))
          case (Right(a), _) if a == B.tru=> Right(B.tru)
          case (Right(_), b) => b
          case (_, Right(b)) if b == B.tru => Right(B.tru)
          case (a, Right(_)) => a
          case _ => Left(fb)
        }
      case And(lhs, rhs) =>
        val l = runFreeBoolP(f, lhs)
        val r = runFreeBoolP(f, rhs)

        (l, r) match {
          case (Right(a), Right(b)) => Right(B.and(a,b))
          case (Right(a), _) if a == B.fls=> Right(B.fls)
          case (Right(_), b) => b
          case (_, Right(b)) if b == B.fls => Right(B.fls)
          case (a, Right(_)) => a
          case _ => Left(fb)
        }
    }
  }

  val p: FreeBool[Predicate] = !olderThan(80) & minSalary(100) & hasSkill("Scala") & hasSkill("DevOps")

  def partially(age: Int, skills: List[String])(p: Predicate): Option[Boolean] = p match {
    case OlderThan(i) => Some(age > i)
    case HasSkill(s) => Some(skills.contains(s))
    case _ => None
  }

  println(runFreeBoolP(partially(28, List("Scala", "Haskell", "FP")), p))
  println(runFreeBoolP(partially(28, List("Scala", "Haskell", "DevOps")), p))
}
