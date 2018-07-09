package fpinscala.datastructures.sol

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List extends App {

  var trace = false

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    if (trace) println(s"FR : $as : $z")
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  // =======================================================================================================================


  //EX 3.1
  val ix = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y //<== This one wins ix = 1 + 2 = 3
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
  println(s"3.1. $ix should be 3")


  //EX 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  println(s"3.2 ${tail(List(1, 2, 3, 4))} should be [2, 3, 4, Nil]")
  println(s"3.2 ${tail(Nil)} should be [Nil]")


  //EX 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case _ => Cons(h, tail(l))
  }

  println(s"3.3 ${setHead(List(1, 2, 3, 4), 0)} should be [0, 1, 2, 3, 4, Nil]")
  println(s"3.3 ${setHead(Nil, 0)} should be [Nil]")


  //EX 3.4
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case _ if n <= 0 => l
    case Cons(_, t) => drop(t, n - 1)
  }

  println(s"3.4 ${drop(List(1, 2, 3, 4), 2)} should be [3, 4, Nil]")
  println(s"3.4 ${drop(Nil, 2)} should be [Nil]")


  //EX 3.5
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  println(s"3.5 ${dropWhile(List(1, 1, 2, 1))(_ == 1)} should be [2, 1, Nil]")


  //EX 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  println(s"3.6 ${init(List(1, 2, 3, 4))} should be [1, 2, 3, Nil]")
  println(s"3.6 ${init(List(1, 2))} should be [1]")


  //EX 3.7 (ugly)
  def foldRightShort[A, B](as: List[A], z: B, tr: A)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) if h == tr => f(h, foldRightShort(Nil: List[A], z, tr)(f))
    case Cons(x, xs) => f(x, foldRightShort(xs, z, tr)(f))
  }

  //TODO: Why types in second arg list did not inferred???
  def productShort(ns: List[Double]) = foldRightShort(ns, 1.0, 0.0)((x: Double, y: Double) => x * y)

  println(s"3.7 ${productShort(List(1, 2, 3, 4))} should be 24")
  println(s"3.7 ${productShort(List(1, 2, 0, 4))} should be 0")


  //EX 3.8 (constructing list in reversed order)
  println(s"3.8 ${foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))} should be [3,2,1]")


  //EX 3.9
  def length[A](l: List[A]): Int = foldRight(l, 0) { (_, a) => a + 1 }

  println(s"3.9 ${length(List(1, 2, 3, 4))} should be 4")
  println(s"3.9 ${length(Nil)} should be 0")


  //EX 3.10
  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    if (trace) println(s"FL: $l : $z")
    l match {
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
      case _ => z
    }
  }


  //EX 3.11
  def sumL(ns: List[Int]) = foldLeft(ns, 0)((x, y) => x + y)

  def productL(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def lengthL[A](l: List[A]): Int = foldLeft(l, 0) { (a, _) => a + 1 }

  println(s"3.11 ${sumL(List(1, 2, 3, 4))} should be 10")
  println(s"3.11 ${productL(List(1, 2, 3, 4))} should be 24")
  println(s"3.11 ${lengthL(List(1, 2, 3, 4))} should be 4")


  //EX 3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A]) { (acc, e) => Cons(e, acc) }

  println(s"3.12 ${reverse(List(1, 2, 3, 4))} should be [4, 3, 2, 1, Nil]")


  //EX 3.13 (only foldRight via foldLeft)
  def foldRightSafe[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z) { (b: B, a: A) => f(a, b) }
  println("\n\n\n")
  trace = true
  println(s"3.13 ${foldRightSafe(List(1, 2, 3), Nil: List[Int])(Cons(_, _))} should be [3, 2, 1, Nil]")
  println(s"3.13 test ${foldRightSafe(List(10, 20, 30), 1)(_ - _)} should be ${foldRight(List(10, 20, 30), 1)(_ - _)}")
  trace = false
  println("\n\n\n")

  //EX 3.14
  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  println(s"3.14 ${append2(List(1, 2, 3), Nil: List[Int])} should be [3, 2, 1, Nil]")


  //EX 3.15 //TODO: Simplify using foldRight
  def flattern[A](l: List[List[A]]): List[A] = l match {
    case Cons(h, Nil) => h
    case Cons(h, t) => append(h, flattern(t))
  }

  println(s"3.15 ${flattern(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))} should be [1, 2, 3, 4, 5, 6, 7, Nil]")


  //EX 3.16
  def add1(l: List[Int]): List[Int] =
    foldRightSafe(l, Nil: List[Int]) { (el, acc) => Cons(el + 1, acc) }

  println(s"3.16 ${add1(List(10, 20, 30, 40))} should be [11, 21, 31, 41, Nil]")


  //EX 3.17
  def doubleToString(l: List[Double]): List[String] =
    foldRightSafe(l, Nil: List[String]) { (el, acc) => Cons(el.toString, acc) }

  println(s"3.17 ${doubleToString(List(10, 20, 30, 40))} should be [10, 20, 30, 40, Nil]")


  //EX 3.18
  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRightSafe(l, Nil: List[B]) { (el, acc) => Cons(f(el), acc) }

  println(s"3.18 ${map(List(10, 20, 30, 40))(_ * 2)} should be [20, 40, 60, 80, Nil]")


  //EX 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => filter(t)(f)
    case Cons(h, t) => Cons(h, filter(t)(f))
    case Nil => Nil
  }

  println(s"3.19 ${filter(List(1, 2, 3, 4))(_ % 2 == 0)} should be [1, 3, Nil]")


  //EX 3.20
  //TODO: Should be a better way...
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = flattern(map(l)(f))
  println(s"3.20 ${flatMap(List(1, 2, 3, 4))(e => List(e,e*2))} should be [1, 2, 2, 4, 3, 6, 4, 8, Nil]")


  //EX 3.21
  def filter2[A](l: List[A])(p: A => Boolean): List[A] =
    flatMap(l)((a: A) => if (p(a)) List(a) else List())

  println(s"3.21 ${filter2(List(1, 2, 3, 4))(_ % 2 == 0)} should be [2, 4, Nil]")


  //EX 3.22
  def accepts(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
      case (Cons(hl, tl), Cons(hr, tr)) => Cons(hl + hr, accepts(tl, tr))
      case _ => Nil
  }


  //accepts(List(1,2,3), List(1,2)
  println(s"3.22 ${accepts(List(1, 2, 3, 4), List(10, 20, 30, 40, 50))} should be [11, 22, 33, 44, Nil]")


  //EX 3.23
  def zip[A, B](l: List[A], r: List[B]): List[(A,B)] = (l, r) match {
    case (Cons(hl, tl), Cons(hr, tr)) => Cons((hl, hr), zip(tl, tr))
    case _ => Nil
  }

  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] = (l, r) match {
    case (Cons(hl, tl), Cons(hr, tr)) => Cons(f(hl, hr), zipWith(tl, tr)(f))
    case _ => Nil
  }

  println(s"3.23a ${zip(List(1, 2, 3, 4), List(10, 20, 30, 40))} should be [(1, 10), (2, 20), (3, 30), (4, 40), Nil]")
  println(s"3.23b ${zipWith(List(1, 2, 3, 4), List(10, 20, 30, 40))((a, b) => (a, b))} should be [(1, 10), (2, 20), (3, 30), (4, 40), Nil]")


  //EX 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def hasIntro(l: List[A], intro: List[A]): Boolean = (l, intro) match {
      case (_, Nil) => true
      case (Cons(hl, tl), Cons(hr, tr)) => hl == hr && hasIntro(tl, tr)
      case _ => false
    }

    if (hasIntro(sup, sub)) true else (sup, sub) match {
      case (Nil, _) => false
      case (Cons(h, t), _) => hasSubsequence(t, sub)
    }
  }
  println(s"3.24 ${hasSubsequence(List(1,2,3,4,5,6), List(3))} should be true")
  println(s"3.24 ${hasSubsequence(List(1,2,3,4,5,6), List(1))} should be true")
  println(s"3.24 ${hasSubsequence(List(1,2,3,4,5,6), List(1,2))} should be true")
  println(s"3.24 ${hasSubsequence(List(1,2,3,4,5,6), List(1,3))} should be false")
  println(s"3.24 ${hasSubsequence(List(1,2,3,4,5,6), List(5,6))} should be true")
  println(s"3.24 ${hasSubsequence(List(1,2,3,4,5,6), List(6))} should be true")
  println(s"3.24 ${hasSubsequence(List(1,2,3,4,5,6), List(1,2,3,4,5,6))} should be true")
  println(s"3.24 ${hasSubsequence(List(1,2,3,4,5,6), Nil)} should be true")
  println(s"3.24 ${hasSubsequence(List(1,2,3,4,5,6), List(1,2,3,4,5,6,7))} should be false")
  println(s"3.24 ${hasSubsequence(Nil, List(1,2))} should be false")
  println(s"3.24 ${hasSubsequence(Nil, Nil)} should be true")

}