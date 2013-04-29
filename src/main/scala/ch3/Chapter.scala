package fp.ch3

import annotation._

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
	
  // Exercise_2
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(y, ys) => ys
  }

  // Exercise_3
  def drop[A](xs: List[A], n: Int): List[A] = 
    if (n <= 0) xs
    else xs match {
      case Nil => Nil
      case Cons(y, ys) =>
        drop(ys, n - 1)
    }

  // Exercise_4
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  // Exercise_5
  def setHead[A](h: A, xs: List[A]): List[A] = xs match {
    case Nil => Cons(h, Nil)
    case Cons(y, ys) => Cons(h, ys)
  }

  //Exercise_6    List(1)
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }
	
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }
	
  //Exercise_9
  def length[A](l: List[A]) = foldRight(l, 0)((elem, acc) => acc + 1)

  //Exercise_10
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum2(l: List[Int]) = foldRight(l, 0.0)(_ + _)
  def product2(l: List[Double]) = foldRight(l, 1.0)(_ * _)

  //Exercise_11
  def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)
  def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)
  def length2[A](l: List[A]) = foldLeft(l, 0)((acc, elem) => acc + 1)

  //Exercise_12
  def rReverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, elem) => Cons(elem, acc))

  // Exercise_13
  def foldl[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(rReverse(l), z)((acc, elem) => f(elem, acc))

  def foldr[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(rReverse(l), z)((acc, elem) => f(elem, acc))

  //Exercise_14
  def append[A](l: List[A], r: List[A]) =
    foldRight(l, r)((acc, elem) => Cons(acc, elem))

  // Exercise_15
  def flat[A](l: List[List[A]]): List[A] =
    foldLeft(l, Nil: List[A]){ (acc, elem) => append(acc, elem) }

  //Exercise_16
  def incByOne(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, incByOne(xs))
  }

  //Exercise_17
  def ld2ls(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, ld2ls(xs))
  }

  //Exercise_18
  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  //Exercise_19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x)) Cons(x, filter(xs)(f))
      else filter(xs)(f)
  }

  //Exercise_20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => append(f(x), flatMap(xs)(f))
  }

  //Exercise_21
  def filter2[A](l: List[A])(f: A => Boolean) =
    flatMap(l) { elem => if (f(elem)) List(elem) else Nil }

  //Exercise_22
  def sumLists(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, sumLists(xs, ys))
  }

  //Exercise_23
  def genAdd[A](a: List[A], b: List[A])(op: (A, A) => A): List[A] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(op(x, y), genAdd(xs, ys)(op))
  }

  def zip[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zip(t1, t2)(f))
  }

  def forall[A](l: List[A])(p: A => Boolean): Boolean = l match {
    case Nil => true
    case Cons(h, t) => p(h) && forall(t)(p)
  }

  //Exercise_24
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil => false
    case Cons(h, t) => forall(zip(l, sub)(_ == _))(identity) || hasSubsequence(t, sub)
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  
  //Exercise_25
  def size[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => size(l) + size(r)
    case Leaf(_) => 1
  }

  //Exercise_26
  def maximum(t: Tree[Int]): Int = t match {
    case Branch(l, r) => maximum(l) max maximum(r)
    case Leaf(v) => v
  }

  //Exercise_27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }
  
  //Exercise_28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  //Exercise_29

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(r)(f)(g), fold(l)(f)(g))
  }

  def fold2[A,B](t: Tree[A], z: B)(f: (A, B) => B): B = t match {
    case Leaf(v) => f(v, z)
    case Branch(l, r) => fold2(r, fold2(l, z)(f))(f)
  }

  def size2[A](t: Tree[A]): Int =
    fold(t)(a => 1)(_ + _)

  def size3[A](t: Tree[A]): Int =
    fold2(t, 0)((elem, acc) => acc + 1)

  def maximum2(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def maximum3(t: Tree[Int]): Int =
    fold2(t, 0)((elem, acc) => elem max acc)

  def depth2[A](t: Tree[A]): Int =
    fold(t)(a => 0)((elem1, elem2) => 1 + (elem1 max elem2))

  def map3[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  val tree = Branch(Branch(Leaf(1),Leaf(2)),Branch(Branch(Leaf(3),Leaf(4)),Leaf(5)))
}