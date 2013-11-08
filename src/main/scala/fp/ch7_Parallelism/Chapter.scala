package fp.ch7_Parallelism

import java.util.concurrent.{Callable, ExecutorService, TimeUnit, Future}

case class SimpleFuture[A](a: A) extends Future[A] {
  def cancel(mayInterruptIfRunning: Boolean) = false
  def isCancelled = false
  def isDone = true
  def get = a
  def get(timeout: Long, unit: TimeUnit) = get
}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = es => SimpleFuture(a)

  def fork[A](p: => Par[A]): Par[A] = es => es.submit(new Callable[A]() {
    def call(): A = p(es).get()
  })

  def async[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = f andThen unit

  def run[A](a: Par[A])(implicit es: ExecutorService) = a(es)

  def map2[A, B, C](fa: Par[A], fb: Par[B])(f: (A, B) => C): Par[C] =
    implicit es => {
    val a = run(fa)
    val b = run(fb)
    SimpleFuture(f(a.get, b.get))
  }

  def map[A,B](fa: Par[A])(f: A => B): Par[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def product[A,B](fa: Par[A], fb: Par[B]): Par[(A,B)] =
    map2(fa, fb)((a, b) => (a, b))

  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](es => SimpleFuture(Nil)){ (elem, acc) =>
      map2(elem, acc)(_ :: _)
    }

  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = {
    val lp: List[Par[B]] = l.map(asyncF(f))
    sequence(lp)
  }

  def parEqual[A](p1: Par[A], p2: Par[A]): Par[Boolean] = es =>
    unit(p1(es).get == p2(es).get)(es)

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val lp: List[Par[List[A]]] = l.map(asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(lp))(_.flatten)
  }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => choices(n(es).get)(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(if (_) 0 else 1))(t :: f :: Nil)

  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] = { es =>
    val k = key(es).get
    run(choices.apply(k))(es)
  }

  def chooser[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = es => run(f(run(pa)(es).get))(es)

  def choiceWithChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = chooser(cond)(if (_) t else f)

  def choiceNWithChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = chooser(n)(choices)

  def bind[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = es => run(f(run(pa)(es).get))(es)

  def join[A](ppar: Par[Par[A]]): Par[A] = bind(ppar)(x => x)

  def map2Bind[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    bind(pa) { a => map(pb) { b => f(a, b) } }

}



object Chapter {
  import Par._
  def sum(as: IndexedSeq[Int]): Par[Int] =
    if (as.size <= 1) Par.unit(as.headOption getOrElse 0)
    else {
      val (l,r) = as.splitAt(as.length/2)
      Par.map2(sum(l), sum(r))(_ + _)
    }
}