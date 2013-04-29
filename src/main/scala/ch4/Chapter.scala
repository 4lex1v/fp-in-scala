package fp.ch4

import scala.util.{ Try, Success, Failure }
import java.util.regex._

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(value) => value
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this map f getOrElse(None)


  def orElse[B >: A](or: => Option[B]): Option[B] =
    this.map(Some(_)) getOrElse or

  def filter(f: A => Boolean): Option[A] = {
    val bool = this map f getOrElse false
    if (bool) this else None
  }

  def filter2(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) this else None)
}

case class Some[A](value: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  // can't use toOption method, cause we r
  // using custom Option trait implementation
  def pattern(s: String): Option[Pattern] =
    Try(Pattern.compile(s)) match {
      case Success(v) => Some(v)
      case Failure(e) => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)

  def bothMatch(pat1: String, pat2: String, s: String): Option[Boolean] =
    mkMatcher(pat1) flatMap { f =>
      mkMatcher(pat2) map { g =>
        f(s) && g(s)
      }
    }

  //Exercise_2
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  //Exercise_3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a.flatMap { x => b.map { y => f(x, y) } }

  //Exercise_4
  def bothMatch2(pat1: String, pat2: String, s: String): Option[Boolean] =
    map2(mkMatcher(pat1), mkMatcher(pat2))((f, g) => f(s) && g(s))

  //Exercise_5
  def sequence[A](seq: List[Option[A]]): Option[List[A]] = {
    @annotation.tailrec
    def inner(cur: List[Option[A]], acc: Option[List[A]]): Option[List[A]] = {
      cur match {
        case Nil => acc
        case x :: xs =>
          inner(xs, map2(x, acc)(_ :: _))
      }
    }
    inner(seq, Some(Nil))
  }

  def sequence2[A](seq: List[Option[A]]): Option[List[A]] =
    seq.foldRight[Option[List[A]]](Some(Nil)){ (elem, acc) =>
      map2(elem, acc)(_ :: _)
    }

  def sequence3[A](seq: List[Option[A]]): Option[List[A]] = seq match {
    case Nil => None
    case x :: xs =>
      map2(x, sequence3(xs))(_ :: _)
  }

  def parsePatterns(xs: List[String]): Option[List[Pattern]] =
    sequence(xs.map(pattern))

  //Exercise_6
  def traverse[A,B](seq: List[A])(f: A => Option[B]): Option[List[B]] = seq match {
    case Nil => Some(Nil)
    case x :: xs =>
      map2(f(x), traverse(xs)(f))(_ :: _)
  }

  def sequence5[A](seq: List[Option[A]]): Option[List[A]] = traverse(seq)(identity)
}

sealed trait Either[+E, +A] {

  //Exercise_7
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(v) => Right(f(v))
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(v) => this
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this.map(f).flatMap(identity)

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap{ x => b.map{ y => f(x, y) } }
}
case class Left[E](value: E) extends Either[E, Nothing]
case class Right[A](value: A) extends Either[Nothing, A]

object Either {
  //Exercise_8
  def sequence[E, A](x: List[Either[E, A]]): Either[E, List[A]] =
    x.foldRight[Either[E, List[A]]](Right(Nil)){
      (elem, acc) => elem.map2(acc)(_ :: _)
    }

  def traverse[E, A, B](x: List[A])(f: A => Either[E,B]): Either[E, List[B]] =
    x.foldRight[Either[E, List[B]]](Right(Nil)){
      (elem, acc) => f(elem).map2(acc)(_ :: _)
    }
}