package fpinscala.laziness

import fpinscala.laziness.Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = foldRight[List[A]](Nil) {
    _ :: _
  }

  //NOT TAILREC
  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_, _) if n == 0 => Empty
    case Cons(h, t) => Cons(h, () => t().take(n - 1))
    //    case Cons(h, t) => cons(h(), t().take(n-1))
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case s => s
  }

  @annotation.tailrec
  final def dropWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if p(h()) => t().dropWhile(p)
    case s => s
  }

  //NOT TAILREC
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case Empty => true
  }

  //NOT TAILREC
  def takeWhileFR(p: A => Boolean): Stream[A] = this.foldRight(empty[A]) { (e, s) =>
    lazy val el = e
    if (p(el)) cons(el, s) else s
  }

  def headOption: Option[A] = this.foldRight[Option[A]](None) { (e, _) =>
    Some(e)
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = this.foldRight(empty[B]) { (e, r) => cons(f(e), r)}

/*
  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight(empty[B]) { (e, r) =>


  }

  def append[B>:A](a: => Stream[B]): Stream[B] =this.foldRight(empty[B]) { (e, r) =>

  }
*/

  def startsWith[B](s: Stream[B]): Boolean = ???
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
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}

object Test extends scala.App {
  println(Stream(1, 2, 3, 4, 5).toList)
  println(Stream(1, 2, 3, 4, 5).take(2).toList)
  println(Stream(1, 2, 3, 4, 5).drop(2).toList)
  println(Stream(1, 2, 3, 4, 5).dropWhile(_ < 4).toList)
  println(Stream(1, 2, 3, 4, 5).takeWhile(_ < 4).toList)
  println(Stream(1, 2, 3, 4, 5).forAll(_ < 6))
  println(Stream(1, 2, 3, 4, 5).forAll(_ < 4))
  println(Stream(1, 2, 3, 4, 5).takeWhileFR(_ < 4).toList)
  println(Stream(1, 2, 3, 4, 5).headOption)
  println(Stream().headOption)
  println(Stream(1, 2, 3, 4, 5).map{ print("! "); _ * 10}.take(2).toList) //WTF???

}