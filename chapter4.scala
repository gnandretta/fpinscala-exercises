sealed trait Option[+A] {

  // Exercise 4.1

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  // def flatMap[B](f: A => Option[B]): Option[B] = this match {
  //   case None => None
  //   case Some(a) => f(a)
  // }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  // def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
  //   case None => ob
  //   case _ => this
  // }

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  // def filter(f: A => Boolean): Option[A] = this match {
  //   case Some(a) => if (f(a)) this else None
  //   case _ => this
  // }

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  // Exercise 4.2

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x-m, 2))))

  // Exercise 4.3

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (sa => b map (sb => f(sa, sb)))

  // Exercise 4.4

  def sequence[A](l: List[Option[A]]): Option[List[A]] = l match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (sh => sequence(t) map (sh :: _))
  }

  // NOTE: the `sequence` definition is very similar to the definition of
  // `List.foldRight` from the previous chapter and the second case's result
  // is almost identical to `map2`.

  def sequenceViaMap2[A](l: List[Option[A]]): Option[List[A]] =
    l.foldRight[Option[List[A]]](Some(Nil))((oh,ot) => map2(oh,ot)(_ :: _))

  // Exercise 4.5

  def traverseViaSequence[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(a map f)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  // NOTE: `traverse` uses the pattern matching from `sequence` and the `map2`
  // function from `sequenceViaMap2`.

  def traverseViaMap2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h, ot) => map2(f(h), ot)(_ :: _))

  def sequenceViaTraverse[A](l: List[Option[A]]): Option[List[A]] =
    traverse(l)(x => x)

}

sealed trait Either[+E, +A] {

  // Exercise 4.6

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):
    Either[EE, C] = for { ra <- this; rb <- b } yield f(ra, rb)

}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  // Exercise 4.7

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((a, rt) => f(a).map2(rt)(_ :: _))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

  // Exercise 4.8

  // `map2` uses `flatMap` (via for-comprehension) which stops the computation
  // when the first stage fails (when it can't obtain a name in the example). We
  // need to continue if we want to also capture a failure in the second stage
  // (when the age can't be obtained). The implementations of `traverse` and
  // `sequence` will need to be changed for the same reason. They can return a
  // `Either[List[E],A]` or we can make the fact that we have a list of errors
  // explicit in the `Left` constructor of a new data type.
  // I'm not sure about how `orElse` should behave.

}
