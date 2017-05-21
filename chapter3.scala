// Exercise 3.1

// The result is `3` The third case is the first one that matches (the other two
// cases that follow also match).

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x,xs) => f(x, foldRight(xs, z)(f)) // can overflow =/
  }

  // Exercise 3.2

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  // The tail function returns the empty list when the argument is the empty
  // list. It could also throw an error or return another value that indicates
  // whether or not it was able to obtain the tail of the list and that wraps
  // the tail of the list in case of succcess.

  // Exercise 3.3

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => Cons(h, t)
  }

  // Exercise 3.4

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else drop(tail(l), n-1)

  // Exercise 3.5

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f)
                       else l
  }

  // Exercise 3.6

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t)) // can overflow =/
  }

  // Exercise 3.7

  // The only place where we may be able to halt the recursion is in the `f`
  // function which is invoked with an element of the list and the result of
  // applying `foldRight` to the elements that follow it:
  // `f(x, foldRight(xs, z)(f))`. If the arguments of `f` are evaluated before
  // applying `f` we won't be able to short-circuit.

  // Exercise 3.8

  // We obtain the original list because `foldRight` replaces the List
  // constructors with its arguments and we are supplying them back.

  // Exercise 3.9

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, n) => n+1)
  }

  // Exercise 3.10

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // Exercise 3.11

  def sumViaFoldLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def productViaFoldLeft(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((n, _) => n+1)

  // Exercise 3.12

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((t, h) => Cons(h, t))

  // TODO: Exercise 3.13

  // Exercise 3.14

  def append[A](l1: List[A], l2: List[A]) =
    foldRight(l1, l2)(Cons(_,_)) // replaces Nil from l1 with l2 and keeps Cons

  // Exercise 3.15

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append) // replaces Cons with append and keeps Nil

}
