package fp.ch6_State

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
    }
  }

  val rng = simple(42)

  def randomPair: ((Int, Int), RNG) = {
    val (i1, r1) = rng.nextInt
    val (i2, r2) = r1.nextInt
    ((i1, i2), r2)
  }

  def positiveInt(rng: RNG): (Int, RNG) = {
    val (i1, r1) = rng.nextInt
    if (i1 == Int.MinValue) positiveInt(r1) else (i1.abs, r1)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i1, r1) = positiveInt(rng)
    (i1 / (Int.MaxValue.toDouble + 1), r1)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i1, r1) = positiveInt(rng)
    val (d1, r2) = double(r1)
    ((i1, d1), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (t, r) = intDouble(rng)
    ((t._2, t._1), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(cnt: Int)(rnd: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def inner(acc: List[Int], rand: RNG, cnt: Int): (List[Int], RNG) = {
      if (cnt == 0) (acc, rand)
      else {
        val (i, r) = positiveInt(rand)
        inner(i :: acc, r, cnt - 1)
      }
    }
    inner(Nil, rnd, cnt)
  }

  def int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rnd => (a, rnd)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rnd => {
      val (a, rng2) = s(rnd)
      (f(a), rng2)
    }

  def positiveMax(n: Int): Rand[Int] =
    map(positiveInt)(v => v / (Int.MaxValue / n))

  def doub: Rand[Double] =
    map(positiveInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](a: Rand[A], b: Rand[B])(f: (A, B) => C): Rand[C] =
    rnd => {
      val (i1, r1) = a(rnd)
      val (i2, r2) = b(r1)
      (f(i1, i2), r2)
    }

  val indDoub: Rand[(Int, Double)] =
    map2(positiveInt, double)((a, b) => (a, b))

  val doubInt: Rand[(Int, Double)] =
    map2(double, positiveInt)((i, d) => (d, i))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft(unit(List.empty[A])) { (acc, elem) =>
      map2(elem, acc)(_ :: _)
    }

  def flatMap[A, B](a: Rand[A])(f: A => Rand[B]): Rand[B] =
    rnd => {
      val (i1, r1) = a(rnd)
      f(i1)(r1)
    }

  def ints2(cnt: Int): Rand[List[Int]] =
    flatMap(positiveInt)(a => unit(List(a)))

  def map2F[A,B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2O[A,B,C](a: Rand[A], b: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(a){ x =>
      map(b) { y =>
        f(x, y)
      }
    }
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State(x => { val (a, s) = run(x); (f(a), s) })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a1, s1) = run(s)
      f(a1).run(s1)
    })

  def map2[B,C](s: State[S,B])(f: (A, B) => C): State[S, C] =
    flatMap{ x => s.map{ y => f(x, y) } }
}

object State {
  def unit[S,A](a: A) = new State[S,A](s => (a, s))
  def sequence[S,A](s: List[State[S, A]]): State[S, List[A]] =
    s.foldLeft[State[S, List[A]]](unit(Nil)){ (acc, elem) =>
      elem.map2(acc)(_ :: _)
    }

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
  for {
    s <- get
    _ <- set(f(s))
  } yield ()
}