//package io
//
//import scalaz.Monad
//
//trait External[A]
//
//sealed trait IO[F[_], +A]
//
//case class Pure[F[_], +A](run: A) extends IO[F, A]
//case class Request[F[_], I, +A](expr: F[I], cb: I => IO[F, A]) extends IO[F, A]
//
//////////////////////////
//trait Console[A]
//case object ReadLine extends Console[Option[String]]
//case class PrintLine(s: String) extends Console[Unit]
//
//trait Run[F[_]] {
//  def apply[A](expr: F[A]): (A, Run[F])
//}
//
//trait Runnable[+A] { def run: A }
//object Delay {
//  def apply[A](a: => A) = new Runnable[A] {
//    def run: A = a
//  }
//}
//
//object IO {
//  def apply[A](a: => A): IO[Runnable, A] =
//    Request(Delay(a), (a: A) => Pure(a))
//
//  def ioMonad[F[_]] = new Monad[({ type λ[α] = IO[F, α]})#λ] {
//    def point[A](a: => A): IO[F, A] = Pure(a)
//    def bind[A, B](fa: IO[F, A])(f: (A) => IO[F, B]): IO[F, B] = {
//      fa match {
//        case Pure(value) => f(value)
//        case Request(expr, callback) =>
//          Request(expr, (x: Any) => bind(callback(x))(f))
//      }
//    }
//  }
//
//  def runIo[F[_], A](run: Run[F])(io: IO[F, A]): A = io match {
//    case Pure(value) => value
//    case Request(ext, callback) =>
//      run(ext) match { case (a, nr) => runIo(nr)(callback(a)) }
//  }
//
//  def run[F[_], A](M: Monad[F])(io: IO[F, A]): F[A] = io match {
//    case Pure(value) => M.point(value)
//    case Request(expr, callback) => run(M) {
//      M.map(expr)((x: Any) => run(M)(callback(x)))
//    }
//  }
//
//}
//
//object ConsoleRunner extends Run[Console] {
//  def apply[A](expr: Console[A]) = expr match {
//    case ReadLine => (Some(readLine()), ConsoleRunner)
//    case PrintLine(str) => (println(str), ConsoleRunner)
//  }
//}
//
//
//object IoTest extends App {
//  import IO._
//
//
//  def console(xs: List[String]): Run[Console] = new Run[Console] {
//    def apply[A](expr: Console[A]) = expr match {
//      case PrintLine(str) => (println(str), this)
//      case ReadLine => (xs.headOption, console(xs.tail))
//    }
//  }
//
//  val action: Console[Unit] = PrintLine("Hello, ")
//  val callback = (_: Unit) => Request(PrintLine("World"), (_: Unit) => Pure[Console, Unit]("Done"))
//
//  val req = Request(action, callback)
//
//  run(console("Hello" :: ", " :: "World" :: Nil))(Request(ReadLine,
//    (o1: Option[String]) => Request(ReadLine,
//      (o2: Option[String]) => Request(ReadLine,
//        (o3: Option[String]) => Request(PrintLine("" + o1.get + o2.get + o3.get),
//          (_: Unit) => Pure("Done"))))))
//
//}
//
