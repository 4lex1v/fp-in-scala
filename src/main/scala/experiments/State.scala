package experiments

case class State[A, S](runS: S => (A, S)) {
  def map[B](f: A => B): State[B, S] = State[B, S] { s =>
    val (a, ns) = runS(s)
    (f(a), ns)
  }
  def flatMap[B](f: A => State[B, S]) = State[B, S] { s =>
    val (a, ns) = runS(s)
    f(a).runS(ns)
  }
}

object State {
  def getState[S] = State[S, S](s => (s, s))
  def setState[S](s: S) = State[Unit, S](_ => ((), s))
  def pureState[A, S](a: A) = State[A, S](s => (a, s))
}

object ListLength {
  import State._


  def zip[A](list: List[A]): List[(A, Int)] = {
    val initSt: State[List[(A, Int)], Int] = pureState(List())
    list.foldLeft(initSt) { (acc, elem) =>
      for {
        curSt <- acc
        x <- getState
        _ <- setState(x + 1)
      } yield (elem, x) :: curSt
    }.runS(0)._1.reverse
  }

  def main(args: Array[String]) {
    val list = List.fill(10000)(1)
    zip(list)
  }
}