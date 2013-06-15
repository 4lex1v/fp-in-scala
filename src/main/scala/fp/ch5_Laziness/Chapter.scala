package fp.ch5_Laziness

trait Stream[+A] {
  import Stream._
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty
  def toListO: List[A] = uncons match {
    case Some((h, t)) => h :: t.toListO
    case _ => Nil
  }

  //Exercise_1
  def toList: List[A] = foldRight(List.empty[A])(_ :: _)

  //Exercise_2
  def take(i: Int): Stream[A] = uncons match {
    case Some((h, t)) if i != 0 => cons(h, t.take(i - 1))
    case _ => empty
  }

  //Exercise_3
  def takeWhile(f: A => Boolean) = foldLeft(empty[A]){ (acc, elem) =>
    if (f(elem)) cons(elem, acc) else empty
  }

  def takeWhileO(f: A => Boolean): Stream[A] = uncons match {
    case Some((h, t)) if f(h) => cons(h, t.takeWhileO(f))
    case _ => empty
  }

  def foldRight[B](acc: => B)(f: (A, => B) => B): B = uncons match {
    case Some((h, t)) => f(h, t.foldRight(acc)(f))
    case _ => acc
  }

  def foldLeft[B](acc: => B)(f: (=> B, A) => B): B = this.uncons match {
    case Some((h, t)) => f(t.foldLeft(acc)(f), h)
    case _ => acc
  }

  def exists(f: A => Boolean): Boolean =
    foldRight(false)((elem, acc) => f(elem) || acc)

  //Exercise_4
  def forall(f: A => Boolean): Boolean = uncons match {
    case Some((h, t)) if (!f(h)) => false
    case Some((h, t)) if (f(h)) => t.forall(f)
    case _ => true
  }

  def forAll(f: A => Boolean): Boolean =
    foldRight(true){ (elem, acc) => f(elem) && acc }

  //Exercise_6
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B]){ (elem, acc) =>
      cons(f(elem), acc)
    }

  def filter(f: A => Boolean) =
    foldRight(empty[A]){ (elem, acc) =>
      if (f(elem)) cons(elem, acc)
      else acc
    }

  def append[B >: A](s1: Stream[B]): Stream[B] =
    foldRight(s1)((e, a) => cons(e, a))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldLeft(empty[B])((a, e) => a.append(f(e)))


  def mapU[B](f: A => B): Stream[B] = unfold(this){
    _.uncons match {
      case Some((h, t)) => Some((f(h), t))
      case None => None
    }
  }

  def takeU(n: Int) = unfold((this, n)){
    case (s, n) if n > 0 =>
      s.uncons.map { case (h, t) => (h, (t, n - 1)) }
    case _ => None
  }

  def takeWhileU(f: A => Boolean) = unfold(this) {
    _.uncons match {
      case Some((h, t)) if (f(h)) => Some((h, t))
      case _ => None
    }
  }

  def zip[B](st: Stream[B]): Stream[(A, B)] = unfold((this, st)) { case (a, b) =>
    (a.uncons, b.uncons) match {
      case (Some((h1, t1)), Some((h2, t2))) =>
        Some(((h1, h2), (t1, t2)))
      case _ => None
    }
  }

  def zipAll[B](st: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, st)) {
    case (a, b) =>
      (a.uncons, b.uncons) match {
        case (Some((h1, t1)), Some((h2, t2))) =>
          Some(((Option(h1), Option(h2)), (t1, t2)))
        case _ => None
      }
  }

  //Exercise_14
  def tails: Stream[Stream[A]] = cons(this, empty).append(unfold(this){
    _.uncons match {
      case Some((h, t)) => Some((t, t))
      case _ => None
    }
  })

  def tails2: Stream[Stream[A]] = unfold(this){ s =>
    s.uncons match {
      case Some((_, t)) => Some((s, t))
      case _ => None
    }
  }.append(Stream(empty))

  def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean =
    s1.tails exists (startsWith(_, s2))

  //Exercise_15
  def scanRight[B](acc: => B)(f: (A, => B) => B): Stream[B] =
    this.tails.map { stream =>
      stream.foldRight(acc) { (acc, elem) =>
        f(acc, elem)
      }
    }
}

object Stream {
  def empty[A]: Stream[A] =
    new Stream[A] { def uncons = None }

  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] =
    new Stream[A] { lazy val uncons = Some((hd, t1)) }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def ones: Stream[Int] = cons(1, ones)

  //Exercise_7
  def constant[A](value: A): Stream[A] = cons(value, constant(value))

  //Exercise_8
  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def build(upto: Int): Stream[Int] = from(1).take(upto)

  //Exercise_9
  def fib: Stream[BigInt] = {
    def inner(prev: BigInt, cur: BigInt): Stream[BigInt] =
      cons(prev, inner(cur, prev + cur))
    inner(0, 1)
  }

  //Exercise_10
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, t)) => cons(h, unfold(t)(f))
      case None => empty
    }

  //Exercise_11
  def constantU[A](v: A): Stream[A] = unfold(v)(s => Some((s, s)))
  def fromU(num: Int): Stream[Int] = unfold(num)(s => Some((s, s + num)))
  def onesU: Stream[Int] = constantU(1)
  def fibU: Stream[Int] = unfold((0, 1)){
    case (a, b) => Some((a, (b, a+b)))
  }

  //Exercise_13
  def startsWith[A](s1: Stream[A], s2: Stream[A]): Boolean =
    s1.zipAll(s2).takeWhileU(t => !t._1.isEmpty).forAll {
      _ match {
        case (Some(a), Some(b)) => a == b
        case _ => false
      }
    }
}

