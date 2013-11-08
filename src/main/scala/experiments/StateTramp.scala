package experiments

case class StateTramp[A, S](runS: S => Trampoline[(A, S)]) {
  def map[B](f: A => B): StateTramp[B, S] = StateTramp[B, S] { s =>
    runS(s).map { case (a, ns) => (f(a), ns) }
  }

  def flatMap[B](f: A => StateTramp[B, S]): StateTramp[B, S] = StateTramp { s =>
    More { () => runS(s) flatMap { case (a, ns) => More(() => f(a).runS(ns)) } }
  }
}
