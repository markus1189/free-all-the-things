package de.codecentric

abstract class ~>[F[_], G[_]] {
  def apply[A](input: F[A]): G[A]
}
