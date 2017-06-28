import Stream._

sealed trait Stream[+A] {

  // Exercise 5.1

  // def toList: List[A] = this match {
  //   case Cons(h,t) => h() :: t().toList
  //   case Empty => List()
  // }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => go(t(), h() :: acc)
      case Empty => acc
    }
    go(this, List()).reverse
  }

  // Exercise 5.2

  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) => if (n > 0) cons(h(), t().take(n - 1)) else empty
    case Empty => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h,t) => if (n > 0) t().drop(n - 1) else this
    case Empty => empty
  }

  // Exercise 5.3

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) => if (p(h())) cons(h(), t().takeWhile(p)) else empty
    case Empty => empty
  }

  // Exercise 5.4

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case Empty => z
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // Exercise 5.5

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) => if (p(h)) cons(h, t) else empty)

  // Exercise 5.6

  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // Exercise 5.7

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h,t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) => if (p(h)) cons(h, t) else t)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h) append t)

  // Exercise 5.13

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h,t) => Some((f(h()), t()))
      case Empty => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h,t), n) => if (n > 0) Some((h(), (t(), n-1))) else None
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h,t) => if (p(h())) Some((h(), t())) else None
      case Empty => None
    }

  def zipWithViaUnfold[B,C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(h1,t1), Cons(h2,t2)) => None
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s: Stream[B]): Stream[(Option[A],Option[B])] =
     unfold((this, s)) {
       case (Cons(h1,t1), Cons(h2,t2)) =>
         Some(((Some(h1()),Some(h2())), (t1(), t2())))
       case (Cons(h1,t1), Empty) => Some(((Some(h1()), None), (t1(), Empty)))
       case (Empty, Cons(h2,t2)) => Some(((None, Some(h2())), (Empty, t2())))
       case _ => None
    }

  // Exercise 5.14

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(p => !p._2.isEmpty) forAll {
      case (h1, h2) => h1 == h2
    }

  // Exercise 5.15

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Cons(h,t) => Some((cons(h(), t()), t()))
      case _ => None
    } append Stream(empty)

  def exists(p: A => Boolean) =
    foldRight(false)((a, b) => p(a) || b)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  // Exercise 5.16

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p) => {
      lazy val lp = p
      val b = f(a, lp._1)
      (b, cons(b, lp._2))
    })._2

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  // Exercise 5.8

  // def constant[A](a: A) =
  //   cons(a, constant(a))

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = cons(a, tail)
    tail
  }

  // Exercise 5.9

  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  // Exercise 5.10

  val fibs = {
    def go(a: Int, b: Int): Stream[Int] =
      cons(a, go(b, a+b))
    go(0,1)
  }

  // Exercise 5.11

  def unfold[A,S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map((p: (A, S)) => cons(p._1, unfold(p._2)(f))).getOrElse(empty[A])

  // Exercise 5.12

  val fibsViaUnfold =
    unfold((0,1)) { case (a,b) => Some(a, (b, a+b)) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n+1)))

  def constantViaUnfold[A](a: A) =
    unfold(a)(_ => Some((a, a)))

  val onesViaUnfold =
    unfold(1)(_ => Some((1, 1)))

}
