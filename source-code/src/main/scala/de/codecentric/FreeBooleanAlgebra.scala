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
  def runFreeBool[A,B](fb: FreeBool[A])(f: A => B)(implicit B: BoolAlgebra[B]): B = {
    fb match {
      case Tru => B.tru
      case Fls => B.fls
      case Inject(v) => f(v)
      case Not(v) => B.not(runFreeBool(v)(f))
      case Or(lhs, rhs) => B.or(runFreeBool(lhs)(f), runFreeBool(rhs)(f))
      case And(lhs, rhs) => B.and(runFreeBool(lhs)(f), runFreeBool(rhs)(f))
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

  type Date = String

  //snippet:site type
  case class Site(terms: List[String], url: String, indexedAt: Date, text: String)
  //end

  object Sites {
    //snippet:functionalconf site
    Site(terms = List("FP", "bangalore"), url = "functionalconf.com", indexedAt = "20181212", text = "lots of body")
    //end

    val functionalconf = Site(List("FP", "conference", "bangalore", "functionalconf"), "functionalconf.com", "20181212", "body")
    val spring = Site(List("Java", "spring", "boot", "cloud"), "spring.io", "20180929", "spring")

    def all() = List(functionalconf, spring)
  }

  //snippet:search predicate
  sealed trait Search
  case class Term(t: String) extends Search
  case class After(date: Date) extends Search
  case class InText(t: String) extends Search
  case class InUrl(url: String) extends Search

  // and the usual smart ctors
  //end

  def term(t: String): FreeBool[Search] = Inject(Term(t))
  def after(date: Date): FreeBool[Search] = Inject(After(date))
  def inText(t: String): FreeBool[Search] = Inject(InText(t))
  def inUrl(url: String): FreeBool[Search] = Inject(InUrl(url))

  //snippet:example search predicate
  val search = term("FP") &
               after("20180101") &
               !(term("Java") | inText("spring")) &
               inUrl("functionalconf")
  //end

  //snippet:eval search predicate
  def evalSearch(pred: FreeBool[Search])(site: Site): Boolean = {
    def nat(s: Search): Boolean = s match {
      case Term(t) => site.terms.contains(t)
      case After(d) => site.indexedAt > d
      case InText(t: String) => site.text.contains(t)
      case InUrl(w) => site.url.contains(w)
    }

    runFreeBool(pred)(nat)
  }

  val result = Sites.all().filter(evalSearch(search))
  //end
}

object FreeBoolPartial extends App {
  import FreeBoolDsl._
  import FreeBool._
  import FreeBoolSugar._

  type Date = String

  object SitesMeta {
    //snippet:functionalconf meta site
    SiteMetadata(terms = List("FP", "bangalore"), url = "functionalconf.com", indexedAt = "20180929")
    //end
    val functionalconf = SiteMetadata(List("FP", "conference", "bangalore", "functionalconf"), "functionalconf.com", "20180929")

    //snippet:functionalconf spring meta site
    SiteMetadata(terms = List("Java", "spring"), url = "spring.io", indexedAt = "20180929")
    //end
    val spring = SiteMetadata(List("Java", "spring", "boot", "cloud"), "spring.io", "20180929")

    def all() = List(functionalconf, spring)
  }

  //snippet:optimizing boolean algebras
  def optimize[A](fa: FreeBool[A]): FreeBool[A] = fa match {
    case Tru => Tru
    case Fls => Fls
    case Inject(v) => Inject(v)
    case Not(Not(v)) => optimize(v)
    case Not(v) => Not(optimize(v))
    case Or(Tru,_) => Tru
    case Or(_,Tru) => Tru
    case Or(x,y) => Or(optimize(x),optimize(y))
    case And(Fls, _) => Fls
    case And(_, Fls) => Fls
    case And(x,y) => And(optimize(x),optimize(y))
  }
  //end

  //snippet:partial evaluator
  def partialEvaluator[A,B](p: FreeBool[A])(f: A => Option[B])(implicit B: BoolAlgebra[B]): Either[FreeBool[A], B] = p match {
    case Tru => Right(B.tru)
    case Inject(v) => f(v).toRight(p)
    case Or(lhs, rhs) =>
      val (l,r) = (partialEvaluator(lhs)(f), partialEvaluator(rhs)(f))
      // check if fully evaluated
      ???
    case _ => ???
  }
  //end

  def runFreeBoolP[A,B](f: A => Option[B], fb: FreeBool[A])(implicit B: BoolAlgebra[B]): Either[FreeBool[A], B] = {
    fb match {
      case Tru => Right(B.tru)
      case Fls => Right(B.fls)
      case Inject(v) => f(v).toRight(fb)
      case Not(v) => runFreeBoolP(f, v).map(B.not(_)).left.map(Not(_))
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

  //snippet:partially
  case class SiteMetadata(terms: List[String], url: String, indexedAt: Date)

  def partially(meta: SiteMetadata)(p: Search): Option[Boolean] = p match {
    case Term(t) => Some(meta.terms.contains(t))
    case After(d) => Some(meta.indexedAt > d)
    case InUrl(w) => Some(meta.url.contains(w))
    case InText(t: String) => None
  }
  //end

  println(runFreeBoolP(partially(SitesMeta.functionalconf), search))
  println(runFreeBoolP(partially(SitesMeta.spring), search))
}
