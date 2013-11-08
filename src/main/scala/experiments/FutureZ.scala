//package experiments
//
//import scalaz.{Monad, Trampoline}
//
//trait Future[+A] {
//  def listen(f: A => Trampoline[Unit]): Unit = ???
//  def runAsync(onFinish: A => Unit) = ???
//  def run: A = this match {
//    case Future.Now(result) => result
//    case Future.More(thunk) => thunk().run
//    case Future.Bind(thunk, bind) => bind(thunk().run).run
//  }
//}
//
//object Future {
//  case class Now[+A](value: A) extends Future[A]
//  case class More[+A](thunk: () => Future[A]) extends Future[A]
//  case class Bind[+A, +B](value: () => Future[A], bind: A => Future[B]) extends Future[B]
//
//  case class Async[+A](onFinish: (A => Trampoline[A]) => Unit) extends Future[A]
//  case class BindAsync[A, B](onFinish: (A => Trampoline[A]) => Unit,
//                             bind: A => Future[B]) extends Future[B]
//
//  val futureM = new Monad[Future] {
//    def point[A](a: => A): Future[A] = Now(a)
//    def bind[A, B](fa: Future[A])(f: (A) => Future[B]): Future[B] = fa match {
//      case Now(a) => f(a)
//      case More(thunk) => Bind(thunk, f)
//      case Bind(value, bind) =>
//
//      case Async(onFinish) => BindAsync(onFinish, f)
//      case BindAsync(onFinish, bind) =>
//    }
//  }
//
//
//}
//
//
//object FutureZ extends App {
//
//
//
//}
