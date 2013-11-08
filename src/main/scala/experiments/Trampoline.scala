package experiments

import scala.annotation.tailrec

trait Trampoline[+A] {
  @tailrec
  final def runT: A = resume match {
    case Right(res) => res
    case Left(next) => next().runT

  }

  def resume: Either[() => Trampoline[A], A] = this match {
    case Done(res) => Right(res)
    case More(next) => Left(next)
    case FlatMap(a, f) => a match {
      case Done(v) => f(v).resume
      case More(k) => Left (() => k() flatMap f)
      case FlatMap (b ,g ) =>
        b.flatMap((x: Any) => g(x) flatMap f).resume
    }
  }

  def map[B](f: A => B) = More(() => Done(f(runT)))

  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = this match {
    case FlatMap(trmp, bind) => FlatMap(trmp, (x: Any) => bind(x).flatMap(f))
    case x => FlatMap(x, f)
  }

  def zip[B](trmp: Trampoline[B]): Trampoline[(A, B)] = {
    (this.resume, trmp.resume) match {
      case (Right(a), Right(b)) => Done((a, b))
      case (Left(a), Left(b)) =>
        More(() => a() zip b())
      case (Left(a), Right(b)) =>
        More(() => a() zip Done(b))
      case (Right(a), Left(b)) =>
        More(() => Done(a).zip(b()))
    }
  }

}

case class More[+A](f: () => Trampoline[A]) extends Trampoline[A]
case class Done[+A](result: A) extends Trampoline[A]
case class FlatMap[A, B](trmp: Trampoline[A], f: A => Trampoline[B]) extends Trampoline[B]



object TrampTest {

  def even[A](xs: List[A]): Boolean = xs match {
    case Nil => true
    case h :: t => odd(t)
  }

  def odd[A](xs: List[A]): Boolean = xs match {
    case Nil => false
    case h :: t => even(t)
  }


  implicit def step[A](a: => A): Trampoline[A] = More(() => Done(a))
}