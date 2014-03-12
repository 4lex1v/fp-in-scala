package fp.ch8_PropertyTesting

import fp.ch6_State.{RNG, State}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))
}

object Gen {

  def choose(start: Int, exclusive: Int): Gen[Int] = Gen {
    State(RNG.positiveInt) map { num =>
      start + num % (exclusive - start)
    }
  }

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen {
    State(RNG.int) map { num => if (num % 2 == 0) true else false }
  }

  def listOfN[A](n: Int, gen: Gen[A]): Gen[List[A]] = Gen {
    State.sequence(List.fill(n)(gen.sample))
  }

  def intPair: Gen[(Int, Int)] = Gen(State(_ => RNG.randomPair))

}

trait Props { self =>
  def check: Boolean
  def && (other: Props) = new Props {
    def check: Boolean = other.check || self.check
  }
}

trait Prop {
  import Prop._
  def check: Either[(Failed, Succeed), Succeed] = ???
}
object Prop {
  type Failed = String
  type Succeed = Int


  def forAll[A](gen: Gen[A])(p: A => Boolean): Prop = ???
}
