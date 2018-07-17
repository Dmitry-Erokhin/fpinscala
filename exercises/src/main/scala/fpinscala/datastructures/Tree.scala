package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree extends App {

  val testTree: Tree[Int] =
    Branch(
      Branch(
        Branch(
          Leaf(1),
          Branch(
            Leaf(2),
            Branch(
              Leaf(3),
              Leaf(4)))),
        Branch(
          Branch(
            Leaf(5),
            Leaf(6)),
          Leaf(7))),
      Leaf(8))

  //EX 3.25
  def size(t: Tree[_]): Int = t match {
    case Branch(l, r) => 1 + size(l) + size(r)
    case Leaf(_) => 1
  }

  println(s"3.25 ${size(testTree)} should be 15")


  //EX 3.26
  def max(t: Tree[Int]): Int = t match {
    case Branch(l, r) =>
      val lm = max(l)
      val rm = max(r)
      if (lm > rm) lm else rm
    case Leaf(v) => v
  }

  println(s"3.26 ${max(testTree)} should be 8")


  //EX 3.27
  def depth(t: Tree[_]): Int = t match {
    case Branch(l, r) =>
      val ld = depth(l) + 1
      val rd = depth(r) + 1
      if (ld > rd) ld else rd
    case Leaf(_) => 1
  }

  println(s"3.27 ${depth(testTree)} should be 6")

  //EX 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(v) => Leaf(f(v))
  }

  println(
    s"""3.28 ${map(testTree)(_ % 2 == 0)} should be
       |    Branch(
       |      Branch(
       |        Branch(
       |          Leaf(false),
       |          Branch(
       |            Leaf(true),
       |            Branch(
       |              Leaf(false),
       |              Leaf(true)))),
       |        Branch(
       |          Branch(
       |            Leaf(false),
       |            Leaf(true)),
       |          Leaf(false))),
       |      Leaf(true))
     """.stripMargin)


  //EX 3.29
  def fold[A, B](t: Tree[A], z: B)(f: (B, B) => B, g: (B, A) => B): B = t match {
    case Branch(l, r) =>
      val ld: B = fold(l, z)(f, g)
      val rd: B = fold(r, z)(f, g)
      f(ld, rd)
    case Leaf(a) => g(z, a)
  }

  println(s"3.29 (depth) ${fold(testTree, 0)(_+_, (b, _) => b + 1)} should be 6")
  println(s"3.29 (max) ${fold(testTree, Int.MinValue)((a, b) => if (a > b) a else b, (_, a) => a)} should be 8")

}