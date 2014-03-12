import scalaz.effect.IO
sealed trait Process[I, O] {
  import Process._

  def apply(input: List[I]): List[O] = this match {
    case Halt()           => List.empty[O]
    case Emit(head, tail) => head.toList ::: tail(input)
    case Await(recv, fallback) => input match {
      case h :: t => recv(h)(t)
      case _ => fallback(input)
    }
  }

  def ++(p: => Process[I, O]): Process[I, O] = this match {
    case Halt() => p
    case Emit(head, tail) => emitAll(head, tail ++ p)
    case Await(recv, fb) => Await(recv andThen (_ ++ p), fb ++ p)
  }

  def |>[O2](p: => Process[O, O2]): Process[I, O2] = p match {
    case Halt() => Halt()
    case Emit(head, tail) => Emit(head, this |> tail)
    case Await(recv, fb)  => this match {
      case Halt() => Halt() |> fb
      case Emit(head, tail) => tail |> p.feed(head)
      case Await(req, fb2)  =>
        Await[I, O2]((i: I) => req(i) |> p , fb2 |> fb)
    }
  }

  def feed(seq: Seq[I]): Process[I, O] = this match {
    case Halt()           => Halt()
    case Emit(head, tail) => Emit(head, tail.feed(seq))
    case Await(recv, fb)  => if (seq.isEmpty) fb else recv(seq.head) ++ feed(seq.tail)
  }

  def map[O2](f: O => O2): Process[I, O2] = this match {
    case Halt() => Halt()
    case Emit(head, tail) => Emit(head map f, tail map f)
    case Await(recv, fb) => Await(recv(_) map f, fb map f)
  }

  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
    case Halt() => Halt()
    case Emit(out, tail) =>
      if (out.isEmpty) tail flatMap f
      else f(out.head) ++ emitAll(out.tail, tail).flatMap(f)
    case Await(recv, fb) => Await(recv andThen (_ flatMap f), fb flatMap f)
  }

  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => go(this)
      case Emit(head, tail) => Emit(head, go(tail))
      case Await(recv, fb) => Await(recv andThen go, fb)
    }
    go(this)
  }

  def zip[O2](p: Process[I, O2]): Process[I, (O, O2)] = Process.zip(this, p)

  def zipWithIndex: Process[I, (O, Int)] = this zip (cnt map (1-))

}

case class Emit[I, O](
  head: Seq[O],
  tail: Process[I, O] = Halt[I, O]()
) extends Process[I, O]

case class Await[I, O](
  recv: I => Process[I, O],
  fallback: Process[I, O] = Halt[I, O]()
) extends Process[I, O]

case class Halt[I, O]() extends Process[I, O]

trait Source[O] {
  def |>[O2](p: Process[O, O2]): Source[O2]
  def filter(f: O => Boolean) = this |> Process.filter(f)
  def map[O2](f: O => O2) = this |> Process.lift(f)
}

case class ResourceR[R,I,O](
  acquire: IO[R],
  release: R => IO[Unit],
  step: R => IO[Option[I]],
  trans: Process[I,O]) extends Source[O] {

  def |>[O2](p: Process[O,O2]) = ResourceR(acquire, release, step, trans |> p)

}





object Process {
  import scalaz.Monad

  def monad[I] = new Monad[({ type λ[α] = Process[I, α]})#λ] {
    override def point[A](a: => A): Process[I, A] = emit(a)
    override def bind[A, B](fa: Process[I, A])(f: (A) => Process[I, B]): Process[I, B] = {
      fa.flatMap(f)
    }
  }

  def emitAll[I, O](head: Seq[O], tail: Process[I, O] = Halt[I, O]()): Process[I, O] = {
    tail match {
      case Emit(h1, t1) => Emit(head ++ h1, t1)
      case _            => Emit(head, tail)
    }
  }

  def filter[I](f: I => Boolean): Process[I, I] =
    Await[I, I]((i: I) => if (f(i)) emit(i) else Halt()).repeat

  def countF: Process[Int, Int] = {
    def go(p: Int): Process[Int, Int] = {
      Await[Int, Int](_ => go(p + 1), emit(p))
    }
    go(0)
  }

  def count0: Process[Int, Int] = {
    def go(p: Int): Process[Int, Int] = Await { _ =>
      emit(p, go(p + 1))
    }
    go(0)
  }

  def count1: Process[Int, Int] = {
    def go(p: Int): Process[Int, Int] = Await { _ =>
      emit(p + 1, go(p + 1))
    }
    go(0)
  }

  def mean: Process[Double,Double] = {
    def go(sum: Double, count: Double): Process[Double,Double] =
      Await((d: Double) => emit((sum+d) / (count+1), go(sum+d,count+1)))
    go(0.0, 0.0)
  }

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] = Await { elem =>
    f(elem, z) match { case (elem2, s) => emit(elem2, loop(s)(f)) }
  }

  def sum2: Process[Int, Int] = loop[Int, Int, Int](0) { case (cur, acc) =>
    (acc + cur, acc + cur)
  }

  def cnt[I]: Process[I, Int] = loop[Int, I, Int](0) { case (_, acc) =>
    (acc + 1, acc + 1)
  }

  def zip[A,B,C](p1: Process[A,B], p2: Process[A,C]): Process[A,(B,C)] = (p1, p2) match {
    case (Halt(), _) => Halt()
    case (_, Halt()) => Halt()
    case (Emit(bs, t1), Emit(cs, t2)) =>
      val (z, a, b) = (bs.zip(cs), bs.drop(cs.length), cs.drop(bs.length))
      Emit(z, zip(
        if (a.isEmpty) t1 else Emit(a, t1),
        if (b.isEmpty) t2 else Emit(b, t2)
      ))
    case (Await(recv1, fb1), _) =>
      Await(recv1 andThen (p1Next => zip(p1Next, p2)), zip (fb1, p2))
    case (_, Await(recv2, fb2)) =>
      Await(recv2 andThen (p2Next => zip(p1, p2Next)), zip (p1, fb2))
  }

  def sum: Process[Int, Int] = {
    def go(acc: Int): Process[Int, Int] = {
      Await[Int, Int](i => go(i + acc), emit(acc))
    }
    go(0)
  }

  def take[I](n: Int): Process[I, I] = Await { elem =>
    if (n <= 0) Halt() else emit(elem, take(n - 1))
  }

  def drop[I](n: Int): Process[I, I] = Await { elem =>
    if (n >= 0) drop(n - 1) else emit(elem, drop(n))
  }

  def takeWhile[I](p: I => Boolean): Process[I, I] = Await { elem =>
    if (p(elem)) emit(elem, takeWhile(p)) else Halt()
  }

  def dropWhile[I](p: I => Boolean): Process[I, I] = Await { elem =>
    if (p(elem)) dropWhile(p) else emit(elem, dropWhile(p))
  }

  def emit[I, O](input: O, tail: Process[I, O] = Halt[I,O]()): Process[I, O] = {
    emitAll(Stream(input), tail)
  }



  def lift[I, O](f: I => O): Process[I, O] = {
    Await(f andThen (emit(_, lift(f))))
  }

}
