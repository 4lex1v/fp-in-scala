package fp.ch14_ST

sealed trait ST[S, A] { self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    protected def run(s: S): (B, S) = {
      val (a, ss) = self.run(s)
      (f(a), ss)
    }
  }

  def flatMap[B](f: A => ST[S, B]) = new ST[S, B] {
    protected def run(s: S): (B, S) = {
      val (a, ss) = self.run(s)
      f(a).run(ss)
    }
  }
}

object ST {
  def apply[S, A](a: => A) = {
    lazy val memo = a
    new ST[S, A] {
      def run(s: S) = (memo, s)
    }
  }
}

sealed trait STRef[S, A] {
  protected var cell: A
  def read: ST[S, A] = ST(cell)
  def write(a: => A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (Unit, S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
    var cell: A = a
  })
}

