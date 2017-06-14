/**
  * Created by reece on 13/06/2017.
  */
object Chapter2 {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(a: Int, b: Int, n: Int): Int = {
      if (n == 0) a + b
      else go(b, a + b, n - 1)
    }
    if (n == 1 || n == 2) n-1
    else go(0, 1, n)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(sas: Array[A]): Boolean = {
      if (sas.length == 1) true
      else if(!ordered(sas(0), sas(1))) false
      else go(sas.drop(1))
    }
    go(as)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
