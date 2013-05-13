package ch7

import java.util.concurrent.{ExecutorService, TimeUnit, Future}


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

  def fork[A](p: => Par[A]): Par[A] = p

  def asynch[A](a: => A): Par[A] = fork(unit(a))

  def asynchF[A, B](f: A => B): A => Par[B] = a => es => SimpleFuture(f(a))

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

  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] =
    es => SimpleFuture(l.map(f))

  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](es => SimpleFuture(Nil)){ (elem, acc) =>
      map2(elem, acc)(_ :: _)
    }

  def parMap2[A, B](l: List[A])(f: A => B): Par[List[B]] = {
    val lp: List[Par[B]] = l.map(asynchF(f))
    sequence(lp)
  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val lp: List[Par[List[A]]] = l.map(asynchF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(lp))(_.flatten)
  }
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