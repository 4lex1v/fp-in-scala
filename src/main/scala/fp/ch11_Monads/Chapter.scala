package fp.ch11_Monads

trait Functor[F[_]] {
  def map[A, B](a: F[A])(f: A => B): F[B]
}

trait Monad[M[_]] extends Functor[M] { self =>
  def point[A](a: => A): M[A]

  def flatMap[A, B](a: M[A])(f: A => M[B]): M[B]

  def map[A, B](a: M[A])(f: A => B): M[B] =
    flatMap(a)(x => point(f(x)))

  def map2[A, B, C](a: M[A], b: M[B])(f: (A, B) => C): M[C] =
    flatMap(a)(x => map(b)(f(x, _)))

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] = {
    la.foldRight(point(List.empty[B])) { (elem, acc) =>
      map2(f(elem), acc)(_ :: _)
    }
  }

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    traverse(lma)(identity)

  def replicateM[A](m: Int, ma: M[A]): M[List[A]] =
    sequence(List.fill(m)(ma))

  def factor[A,B](ma: M[A], mb: M[B]): M[(A, B)] = map2(ma, mb)((_, _))

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)

  def flatMapC[A, B](a: M[A])(f: A => M[B]): M[B] =
    compose(identity[M[A]], f)(a)

  def join[A](m: M[M[A]]): M[A] =
    flatMap(m)(identity)

  def joinC[A](m: M[M[A]]): M[A] =
    compose(identity[M[M[A]]], identity[M[A]])(m)

  def flatMapJ[A, B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))
}

case class Id[A](value: A)

case class State[S, A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      (f(a), s1)
    })
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
}

case class Reader[R, A](run: R => A)

object Chapter {

  val idM = new Monad[Id] {
    def point[A](a: => A): Id[A] = Id(a)
    def flatMap[A, B](a: Id[A])(f: (A) => Id[B]): Id[B] =
      f(a.value)
  }

  val listM = new Monad[List] {
    def point[A](a: => A): List[A] = List(a)
    def flatMap[A, B](a: List[A])(f: (A) => List[B]): List[B] =
      a.flatMap(f)
  }

  val optM = new Monad[Option] {
    def point[A](a: => A): Option[A] = Option(a)
    def flatMap[A, B](a: Option[A])(f: (A) => Option[B]): Option[B] =
      a.flatMap(f)
  }

  val streamM = new Monad[Stream] {
    def point[A](a: => A): Stream[A] = Stream(a)
    def flatMap[A, B](a: Stream[A])(f: (A) => Stream[B]): Stream[B] =
      a.flatMap(f)
  }

  def stateMonad[S] = new Monad[({type lambda[x] = State[S, x]})#lambda] {
    def point[A](a: => A): State[S, A] = State(s => (a, s))
    def flatMap[A, B](a: State[S, A])(f: (A) => State[S, B]): State[S, B] =
      State { s =>
        val (o, n) = a.run(s)
        f(o).run(n)
      }
  }

  def readerMonad[R] = new Monad[({type l[x] = Reader[R, x]})#l] {
    def point[A](a: => A): Reader[R, A] = Reader(r => a)
    def flatMap[A, B](a: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader { r => f(a.run(r)).run(r) }
  }
}
