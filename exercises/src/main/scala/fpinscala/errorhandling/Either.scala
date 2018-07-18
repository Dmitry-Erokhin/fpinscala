package fpinscala.errorhandling


import scala.{Either => _, Left => _, Option => _, Right => _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {

  //EX. 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a) match {
      case Right(b) => Right(b)
      case Left(e) => Left(e)
    }
    case Left(e) => Left(e)
  }

  def flatMap2[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this.map(f) match {
    case Right(Right(a)) => Right(a)
    case Right(Left(e)) => Left(e)
    case Left(e) => Left(e)
  }

  //From answer - way more elegant
  def flatMap3[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case _ => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
    case (Right(x), Right(y)) => Right(f(x, y))
    case (Left(e), _) => Left(e)
    case (_, Left(e)) => Left(e)
  }

  def map22[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      x <- this
      y <- b
    } yield f(x, y)

  def map23[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap { x: A =>
      b.map { y: B =>
        f(x, y)
      }
    }

}

case class Left[+E](get: E) extends Either[E, Nothing]

case class Right[+A](get: A) extends Either[Nothing, A]

object Either {

  //EX 4.7 - from answers (did not managed to accomplish)
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
  es match {
    case Nil => Right(Nil)
    case h::t => (f(h) map2 traverse(t)(f))(_ :: _)
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }



}