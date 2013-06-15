package fp.ch10_Monoids

trait Monoid[A] {
	val zero: A
	val op: (A, A) => A
}

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
  def toList[A](as: F[A]): List[A] = foldRight(as)(List.empty[A])(_ :: _)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Chapter {
	implicit val stringMonoid = new Monoid[String] {
		val zero = ""
		val op = (_: String) + (_: String)
	}

	implicit val intAddition = new Monoid[Int] {
		val zero = 0
		val op = (_: Int) + (_: Int)
	}

	val intMultiplication: Monoid[Int] = new Monoid[Int] {
		val zero = 1
		val op = (_: Int) * (_: Int)
	}

	implicit val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
		val zero = true
		val op = (_: Boolean) || (_: Boolean)
	}

	val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
		val zero = true
		val op = (_: Boolean) && (_: Boolean)
	}

	def optionMonoid[A: Monoid] = new Monoid[Option[A]] {
		val am = implicitly[Monoid[A]]
		val zero = None
		val op = (a: Option[A], b: Option[A]) => 
			a.flatMap(av => b.map(bv => am.op(av, bv)))
	}

  val tscm = new Monoid[String] {
    val zero: String = ""
    val op: (String, String) => String =
      (a: String, b: String) => (a + " " + b).trim
  }

  def concat[A: Monoid](xs: List[A]) = {
    val lm = implicitly[Monoid[A]]
    xs.foldLeft(lm.zero)(lm.op)
  }

  def foldMap[A, B](xs: List[A], m: Monoid[B])(f: A => B): B = xs match {
    case Nil => m.zero
    case h :: t => m.op(f(h), foldMap(t, m)(f))
  }

  def foldMap[A, B: Monoid](xs: List[A])(f: A => B): B = {
    val bm = implicitly[Monoid[B]]
    xs.foldLeft(bm.zero)((a, b) => bm.op(a, f(b)))
  }

	def EndoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
		val zero: A => A = identity
		val op = (f: A => A, g: A => A) => g compose f 
	}

  val listF = new Foldable[List] {
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B =
      as.foldLeft(mb.zero)((b, a) => mb.op(f(a), b))
  }

  val indSeqF = new Foldable[IndexedSeq] {
    def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
    def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(mb: Monoid[B]): B =
      as.foldLeft(mb.zero)((b, a) => mb.op(f(a), b))
  }

  val treeF = new Foldable[Tree] {
    def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
      case Leaf(v) => f(v, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }

    def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
      foldRight(as)(z)((a, b) => f(b, a))

    def foldMap[A, B](as: Tree[A])(f: (A) => B)(mb: Monoid[B]): B = as match {
      case Leaf(v) => mb.op(f(v), mb.zero)
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }
  }

  val optionF = new Foldable[Option] {
    def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
      case Some(v) => f(v, z)
      case None => z
    }

    def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      foldRight(as)(z)((a, b) => f(b, a))

    def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B = as match {
      case Some(v) => f(v)
      case None => mb.zero
    }
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A,B)] = new Monoid[(A, B)] {
    val zero: (A, B) = (A.zero, B.zero)
    val op: ((A, B), (A, B)) => (A, B) =
      (a, b) => (A.op(a._1, b._1), B.op(a._2, b._2))
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    val zero: Map[K, V] = Map.empty
    val op: (Map[K, V], Map[K, V]) => Map[K, V] =
      (ma, mb) => ma.map {
        case (k, v) => (k, V.op(v, mb.getOrElse(k, V.zero)))
      }
  }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[(A) => B] {
    val op: ((A) => B, (A) => B) => (A) => B =
      (f1, f2) => a => B.op(f1(a), f2(a))
    val zero: (A) => B = _ => B.zero
  }

  def freqMap(is: IndexedSeq[String]): Map[String, Int] =
    indSeqF.foldMap(is)((s: String) => Map(s -> 1))(mapMergeMonoid[String, Int](intAddition))

}