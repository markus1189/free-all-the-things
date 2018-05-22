package de.codecentric

//snippet:monoid typeclass
trait Monoid[A] {
  def empty: A
  def combine(x: A, y: A): A
}
//end

object FreeMonoid1 {
  //snippet:free monoid type
  sealed abstract class FreeMonoid[+A]
  case object Empty extends FreeMonoid[Nothing]
  case class Inject[A](x: A) extends FreeMonoid[A]
  case class Combine[A](x: FreeMonoid[A], y: FreeMonoid[A]) extends FreeMonoid[A]
  //end
}

object FreeMonoidExample {
  import FreeMonoid1._
  def program: FreeMonoid[Int] = {
    //snippet:free monoid use
    // 1 |+| 2 |+| 3

    // (1 + 2) + 3
    Combine(Combine(Inject(1), Inject(2)), Inject(3))

    // (1 + (2 + 3))
    Combine(Inject(1), Combine(Inject(2), Inject(3)))
//end
  }
}

object FreeMonoid2 {
  //snippet: free monoid type 2
  sealed trait NotCombine[+A]

  sealed abstract class FreeMonoid[+A]
  case object Empty extends FreeMonoid[Nothing] with NotCombine[Nothing]
  case class Inject[A](x: A) extends FreeMonoid[A] with NotCombine[A]
  case class Combine[A](x: NotCombine[A], y: FreeMonoid[A]) extends FreeMonoid[A]
  //end
}

object FreeMonoid3 {
  //snippet: free monoid type 3
  case class Inject[A](x: A)

  sealed abstract class FreeMonoid[+A]
  case object Empty extends FreeMonoid[Nothing]
  case class Combine[A](x: Inject[A], y: FreeMonoid[A]) extends FreeMonoid[A]
  //end
}

object FreeMonoid4 {
  //snippet: free monoid type 4
  sealed abstract class FreeMonoid[+A]
  case object Empty extends FreeMonoid[Nothing]
  case class Combine[A](x: A, y: FreeMonoid[A]) extends FreeMonoid[A]
  //end
}

object FreeMonoid5 {
  //snippet: free monoid type 5
  sealed abstract class List[+A]
  case object Nil extends List[Nothing]
  case class Cons[A](head: A, tail: List[A]) extends List[A]
  //end
}
