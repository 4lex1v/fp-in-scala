package io

import scalaz.{Monad, State}

sealed trait Free[F[_], A] {
  def map[B](f: A => B): Free[F, B] = FlatMap(this, (a: A) => Return(f(a)))
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = this match {
    case FlatMap(sub, ff) => FlatMap(sub, (a: Any) => FlatMap(ff(a), f))
    case x => FlatMap(x, f)
  }
}

case class Return[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](f: F[Free[F, A]]) extends Free[F, A]
case class FlatMap[F[_], A, B](sub: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

object Free {
  implicit def freeMonad[F[_]] = new Monad[({ type λ[α] = Free[F, α]})#λ] {
    def point[A](a: => A): Free[F, A] = Return[F, A](a)
    def bind[A, B](fa: Free[F, A])(f: (A) => Free[F, B]): Free[F, B] = fa.flatMap(f)
  }
}

///////////
sealed trait Console[R]
case class ReadLine[R](k: Option[String] => R) extends Console[R]
case class PrintLine[R](s: String, k: () => R) extends Console[R]

object Console {
  type ConsoleIO[A] = Free[Console, A]

  def readLn: ConsoleIO[Option[String]] = Suspend {
    ReadLine { (s: Option[String]) => Return(s) }
  }
  def printLn(s: String): ConsoleIO[Unit] = Suspend {
    PrintLine(s, () => Return(println(s)))
  }

  def runConsole[A](console: ConsoleIO[A]): A = console match {
    case Return(res) => res
    case Suspend(ReadLine(k)) => runConsole(k(Some(readLine())))
    case Suspend(PrintLine(_, k)) => runConsole(k())
    case FlatMap(sub, f) => sub match {
      case Return(res) => runConsole(f(res))
      case Suspend(ReadLine(k)) => runConsole { k(Some(readLine())) flatMap f }
      case Suspend(PrintLine(s, k)) => runConsole { k() flatMap f }
      case FlatMap(y, g) => runConsole(y flatMap ((a: Any) => g(a) flatMap f))
    }
  }
}