package fpinscala.state

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

  def nonNegativeIntFirstAttempt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (Int.MinValue, nextRNG) => nonNegativeIntFirstAttempt(nextRNG)
      case (i, nextRNG) => (i.abs, nextRNG)
    }
  }
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (i, nextRNG) if i < 0 => ((i+1).abs, nextRNG)
      case (i, nextRNG) => (i, nextRNG)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = rng.nextInt
    (i / Int.MaxValue.toDouble+1, r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r2) = rng.nextInt
    val (d, r3) = double(r2)
    ((i,d), r3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d), r) = intDouble(rng)
    ((d,i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r2) = double(rng)
    val (d2, r3) = double(r2)
    val (d3, r4) = double(r3)
    ((d1,d2,d3), r4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count < 1) {
      (List(), rng)
    } else {
      val (i, newRNG) = rng.nextInt
      val (rest, finalRNG) = ints(count - 1)(newRNG)
//      (List(i) ++ rest, finalRNG)
      (i :: rest, finalRNG)
    }

  def doubleViaMap: Rand[Double] =
    map(int)(_ / Int.MaxValue.toDouble+1)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    r1 => {
      val (a, r2) = ra(r1)
      val (b, r3) = rb(r2)
      (f(a,b), r3)
    }

  // this one is like reimplementing foldLeft and map2 myself (strict, unlike discussed below)
  def sequenceFirstAttempt[A](fs: List[Rand[A]]): Rand[List[A]] =
    r1 => {
      fs match {
        case List() => unit(List())(r1)
        case ra :: rest =>
          val (a, r2) = ra(r1)
          val (aRest, rLast) = sequence(rest)(r2)
          (a :: aRest, rLast)
      }
    }

  // as in the answers.
  // foldLeft then reverse could be better, since a non-strict implementation is possible
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((ra, rla) => map2(ra, rla)(_ :: _))

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    r1 => {
      val (a, r2) = f(r1)
      g(a)(r2) // == (b, r3)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMapFirstAttempt[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

  // from answers
  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

}

case class State[S,+A](run: S => (A, S)) {

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def result = (coins, candies)
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S,A](a: A): State[S,A] =
    State(s => (a, s))

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.foldRight(State.unit[S,List[A]](List[A]()))((sa, sla) => sa.map2(sla)((h,t) => h :: t))


  def myGet[S]: State[S, S] = State(s => (s, s))
  def mySet[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- myGet
    _ <- mySet(f(s))
  } yield ()

  def transition(i: Input): (Machine => Machine) =
    m => (i,m) match {
      case (_, Machine(_, 0, _)) => m
      case (Coin, Machine(true, _, _)) => Machine(locked = false, m.candies, m.coins + 1)
      case (Turn, Machine(false, _, _)) => Machine(locked = true, m.candies - 1, m.coins)
      case _ => m
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(i => modify[Machine](transition(i))))
    m <- myGet
  } yield m.result

}
