//package experiments
//
//trait Process[I,O] {
//  import Process._
//
//  def apply(s: Stream[I]): Stream[O] = this match {
//    case Halt() => Stream()
//    case Await(recv, fin) => s match {
//      case h #:: t => recv(h)(t)
//      case _ => fin(s)
//    }
//    case Emit(h, t) => h.toStream append t(s)
//  }
//
//  def map[O2](f: O => O2): Process[I, O2] = this match {
//    case Halt() => Halt()
//    case Emit(head, tail) => Emit(head map f, tail.map(f))
//    case Await(recv, fin) => Await(recv andThen(_ map f), fin map f)
//  }
//
//  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
//    case Halt() => Halt()
//    case Emit(seq, tail) =>
//      if (seq.isEmpty) tail flatMap f
//      else f(seq.head) ++ emitAll(seq.tail, tail).flatMap(f)
//  }
//
//  def ++(p: => Process[I, O]): Process[I, O] = this match {
//    case Halt() => p
//    case Emit(head, tail) => emitAll(head, tail ++ p)
//    case Await(recv, fin) => Await(recv andThen (_ ++ p), fin ++ p)
//  }
//}
//
//object Process {
//  def halt[I, O] = Halt[I, O]()
//  def emit[I, O](obj: O, proc: Process[I, O] = Halt()): Process[I, O] = Emit(Stream(obj), halt)
//  def point[O](obj: O): Process[Nothing, O] = emit(obj)
//
//
//  def emitAll[I, O](head: Seq[O], tail: Process[I, O]): Process[I, O] = tail match {
//    case Emit(h2, t) => Emit(head ++ h2, t)
//    case _ => Emit(head, halt)
//  }
//}
//
//case class Emit[I,O](head: Seq[O], tail: Process[I, O] = Halt[I,O]()) extends Process[I,O]
//case class Await[I,O](recv: I => Process[I, O], finalizer: Process[I, O] = Halt[I,O]()) extends Process[I,O]
//case class Halt[I,O]() extends Process[I,O]
//
//
//object ProcessTest extends App {
//
//  val simpleStream = Emit(List("hello"))
//
//
//
//
//}