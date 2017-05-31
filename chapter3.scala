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

  // Exercise 3.16

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((n, acc) => Cons(n+1, acc))

  // Exercise 3.17

  def doubles2Strings(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((d, acc) => Cons(d.toString, acc))

  // Exercise 3.18

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a, bs) => Cons(f(a), bs))

  // Exercise 3.19

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, fas) => if (f(a)) Cons(a, fas) else fas)

  def removeOdds(l: List[Int]): List[Int] =
    filter(l)(_ % 2 == 0)

  // Exercise 3.20

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  // Exercise 3.21

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  // Exercise 3.22

  def addCorresponding(as: List[Int], bs: List[Int]): List[Int] =
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(a, tas), Cons(b, tbs)) => Cons(a+b, addCorresponding(tas,tbs))
    }

  // Exercise 3.23

  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(a, tas), Cons(b, tbs)) => Cons(f(a,b), zipWith(tas,tbs)(f))
    }

  // Exercise 3.24

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match {
      case (Nil, Nil) => true
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(hsup, tsup), Cons(hsub, tsub)) =>
        if (hsup == hsub) hasSubsequence(tsup, tsub) || hasSubsequence(tsup, sub)
        else hasSubsequence(tsup, sub)
    }

  @annotation.tailrec
  def startsWith[A](l: List[A], start: List[A]): Boolean =
    (l, start) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(hl, tl), Cons(hstart, tstart)) =>
        if (hl == hstart) startsWith(tl, tstart)
        else false
    }

  @annotation.tailrec
  def hasSubsequenceTailRec[A](sup: List[A], sub: List[A]): Boolean =
    if (startsWith(sup, sub)) true
    else sup match {
      case Nil => false
      case Cons(_, tsup) => hasSubsequenceTailRec(tsup, sub)
    }

}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 3.25

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // Exercise 3.26

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // Exercise 3.27

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // Exercise 3.28

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // Exercise 3.29

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(n => n)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)(1 + _ max _)
}
