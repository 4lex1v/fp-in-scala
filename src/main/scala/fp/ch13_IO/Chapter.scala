package fp
package ch13_IO

import ch11_Monads.Monad
import scala.util.Try

object Part1 {

  trait IO {
    self =>
    def run: Unit

    def ++(io: IO) = new IO {
      def run {
        self.run; io.run
      }
    }
  }

  object IO {
    def empty = new IO {
      def run = ()
    }
  }

  def PrintLine[T](msg: T): IO =
    new IO {
      def run = println(msg)
    }

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  def convert = {
    PrintLine("Enter a temp in fahrenheit")
    val d = readLine().toDouble
    PrintLine(fahrenheitToCelsius(d))
  }
}

object Part2 {

  trait IO[+A] {
    self =>
    def run: A

    def map[B](f: A => B): IO[B] =
      new IO[B] {
        def run: B = f(self.run)
      }

    def flatMap[B](f: A => IO[B]): IO[B] =
      new IO[B] {
        def run = f(self.run).run
      }

    def ++ [B >: A] (io: IO[B]) = new IO[B] {
      def run = {
        self.run; io.run
      }
    }

    def **[B](io2: IO[B]): IO[(A, B)] =
      self flatMap {
        x => io2 map {
          y => (x, y)
        }
      }
  }

  object IO extends Monad[IO] {
    def point[A](a: => A): IO[A] = new IO[A] {
      def run = a
    }

    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f

    def apply[A](a: => A) = point(a)
  }

  def ReadLine: IO[String] = IO {
    readLine()
  }

  def PrintLine[T](msg: T) = IO {
    println(msg)
  }

  val echo = ReadLine flatMap PrintLine
  val readInt: IO[Int] = ReadLine map (_.toInt)
  val readInts = readInt ** readInt
  def replicateM_[A](times: Int)(action: IO[A]) = List.range(1, times)
    .foldLeft(action) { (acc, elem) => acc ++ action }

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter temp in fahrenheit")
    d <- ReadLine map (_.toDouble)
    _ <- PrintLine(Part1.fahrenheitToCelsius(d).toString)
  } yield ()
}

object Part3 {

  type Id[A] = A

  trait IO[F[_], +A]
  case class Pure[F[_], +A](get: A) extends IO[F, A]
  case class Request[F[_], I, +A](expr: F[I], receive: I => IO[F, A]) extends IO[F, A]

  trait Runnable[+A] { def run: A }
  object Delay { def apply[A](a: => A) = new Runnable[A] { def run: A = a } }

  trait Console[A]
  case object ReadLine extends Console[Option[String]]
  case class PrintLine(value: String) extends Console[Unit]

  trait Run[F[_]] {
    def apply[A](expr: F[A]): (A, Run[F])
  }

  object Run {
    def run[F[_], A](R: Run[F])(io: IO[F, A]): A = io match {
      case Pure(value) => value
      case Request(expr, rec) =>
        R(expr) match { case (e, r2) => run(r2)(rec(e)) }
    }
  }

  import scalaz._
  import Scalaz._

  object ConsoleRunner extends Run[Console] {
    override def apply[A](expr: Console[A]) = expr match {
      case ReadLine => (Some(readLine()), ConsoleRunner)
      case PrintLine(value) => (println(value), ConsoleRunner)
    }
  }


  object IO {

//    def ioMonad[F[_]] = new Monad[({ type λ[x] = IO[F, x]})#λ] {
//      def point[A](a: => A): IO[F, A] = Pure(a)
//      def flatMap[A, B](a: IO[F, A])(f: (A) => IO[F, B]): IO[F, B] = a match {
//        case Pure(value) => f(value)
//        case Request(expr, rec) => Request(expr, rec andThen (_ flatMap f))
//      }
//    }

    def console(lines: List[String]): Run[Console] = new Run[Console] {
      def apply[A](expr: Console[A]) = expr match {
        case ReadLine =>
          if (lines.isEmpty) (None, console(lines))
          else (lines.headOption, console(lines.tail))
        case PrintLine(_) => ((), console(lines))
      }
    }

    def run[F[_],A](F: Monad[F])(io: IO[F,A]): F[A] = io match {
      case Pure(value) => F.point(value)
      case Request(expr, recv) =>
        F.flatMap(expr)(e => run(F)(recv(e)))
    }
  }






































//  trait Runnable[A] {
//    def run: A
//  }
//
//  object Delay {
//    def apply[A](a: => A) = new Runnable[A] {
//      def run = a
//    }
//  }
//
//  trait Console[A]
//
//  case object ReadLine extends Console[Option[String]]
//
//  case class PrintLine(s: String) extends Console[Unit]
//
//  trait Run[F[_]] {
//    def apply[A](expr: F[A]): (A, Run[F])
//  }
//
//  object IO {
//    @annotation.tailrec
//    def run[F[_], A](R: Run[F])(io: IO[F, A]): A = io match {
//      case Pure(a) => a
//      case Request(expr, recv) =>
//        R(expr) match {
//          case (e, r2) => run(r2)(recv(e))
//        }
//    }
//  }
//
//  object RunConsole extends Run[Console] {
//    def apply[A](c: Console[A]) = c match {
//      case ReadLine =>
//        val r = Try(readLine()).toOption
//        (r, RunConsole)
//      case PrintLine(s) => (println(s), RunConsole)
//    }
//  }
//
//  def monad[F[_]] = new Monad[({type f[a] = IO[F, a]})#f] {
//    def point[A](a: => A): IO[F, A] = Pure(a)
//
//    def flatMap[A, B](a: IO[F, A])(f: A => IO[F, B]): IO[F, B] =
//      a match {
//        case Pure(a) => f(a)
//        case Request(expr, recv) => Request(expr, recv.andThen(flatMap(_)(f)))
//      }
//  }
//
//  def console(l: List[String]): Run[Console] = new Run[Console] {
//    def apply[A](expr: Console[A]): (A, Run[Console]) = expr match {
//      case ReadLine =>
//        if (l.isEmpty) (None, console(l))
//        else (Some(l.head), console(l.tail))
//      case PrintLine(_) => ((), console(l))
//    }
//  }
//
//  def run[F[_], A](M: Monad[F])(io: IO[F, A]): F[A] = io match {
//    case Pure(a) => M.point(a)
//    case Request(expr, reqv) =>
//      M.flatMap(expr)(e => run(M)(reqv(e)))
//  }
}
