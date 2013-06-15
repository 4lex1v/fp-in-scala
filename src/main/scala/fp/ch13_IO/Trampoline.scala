package fp.ch13_IO

import fp.ch11_Monads.Monad

trait Trampoline[+A]

object Trampoline extends Monad[Trampoline] {
  case class Done[+A](get: A) extends Trampoline[A]
  case class More[+A](force: () => Trampoline[A]) extends Trampoline[A]
  case class Bind[A,+B](force: () => Trampoline[A],
                        f:     A  => Trampoline[B]) extends Trampoline[B]


  def run[A](a: Trampoline[A]): A = a match {
    case Done(a) => a
    case More(f) => run(f())
    case Bind(fo, f) => run(flatMap(fo())(f))
  }

  def point[A](a: => A): Trampoline[A] = Done(a)
  def flatMap[A, B](a: Trampoline[A])(f: (A) => Trampoline[B]): Trampoline[B] = a match {
    case Done(a) => f(a)
    case More(fo) => Bind(fo, f)
    case Bind(fo, ff) => More(() => Bind(fo, ff andThen (flatMap(_)(f))))
  }

}

