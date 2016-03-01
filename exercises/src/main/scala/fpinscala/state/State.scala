package fpinscala.state

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (int, nextRng) = rng.nextInt
    if (int < 0) (-int, nextRng) else (int, nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (int, nextRng) = nonNegativeInt(rng)
    (int.toDouble / Int.MaxValue, nextRng)
  }
  val _double: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (int, rng1) = rng.nextInt
    val (double, rng2) = RNG.double(rng1)
    ((int, double), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((int, double), rng1) = intDouble(rng)
    ((double, int), rng1)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (double1, rng1) = RNG.double(rng)
    val (double2, rng2) = RNG.double(rng1)
    val (double3, rng3) = RNG.double(rng2)
    ((double1, double2, double3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def newInt(remaining:Int, list:List[Int], rng:RNG): (List[Int], RNG) = {
      if (remaining > 0) {
        val (int, newRng) = rng.nextInt
        newInt(remaining - 1, int::list, newRng)
      } else {
        (list, rng)
      }

    }
    newInt(count, List(), rng)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      @tailrec
      def go(rng: RNG, transitions: List[Rand[A]], values: List[A]): (List[A], RNG) = transitions match {
        case ::(h, t) =>
          val (a, nextRng) = h(rng)
          go(nextRng, t, a::values)
        case Nil => (values, rng)
      }
      go(rng, fs, List())
    }
  }

  def intsSeq(count: Int)(rng:RNG): (List[Int], RNG) = {
    sequence[Int](List.fill(count)(RNG.int))(rng)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }
  }
  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  def nonNegativeLessThan2(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        unit(mod)
      else nonNegativeLessThan(n)
    })
  }

  def mapFM[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2FM[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }
}

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = {
    new State({
      s: S => {
        val (a, s1) = run(s)
        (f(a), s1)
      }
    })
  }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    new State(s => {
      val (a, s1) = run(s)
      val (b, s2) = sb.run(s1)
      (f(a, b), s2)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    new State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
  }


}

sealed trait Input
case object Coin extends Input
case object Turn extends Input


case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
