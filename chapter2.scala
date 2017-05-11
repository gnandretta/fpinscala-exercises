object MyModule {

  // Exercise 2.1

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, a: Int, b: Int): Int =
      if (n <= 0) a
      else go(n - 1, b, a + b)

    go(n, 0, 1)
  }


  // Exercise 2.2

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n+1))) loop(n+1)
      else false

    loop(0)
  }

  // Exercise 2.3

  def curry[A,B,C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  // Exercise 2.4

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // Exercise 2.5

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

}
