package fpinscala.state

import fpinscala.state.State.{get, modify, sequence, set}


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
    val (a, rng2) = rng.nextInt
    val b = if (a < 0) -(a + 1) else a
    (b, rng2)
  }

  def doubleViaMap(rng:RNG): Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def double(rng: RNG): (Double, RNG) = {
    val (a, rng2) = nonNegativeInt(rng)
    val b = a / (Int.MaxValue.toDouble + 1)
    (b, rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt
    ((d, i), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) (Nil, rng)
    else {
      val (a, rngNext) = rng.nextInt
      val (partList, lastRng) = ints(count - 1)(rngNext)
      (a :: partList, lastRng)
    }
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      fs.foldRight((List[A](), rng))((rx, z) => {
        val (acc, curRng) = z
        val (a, nextRng) = rx(curRng)
        ((a :: acc), nextRng)
      })
    }
  }

  def ints2(count: Int)(rng: RNG): Rand[List[Int]] = sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, nextRng) = f(rng)
    g(a)(nextRng)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(a => {
      val mod = a % n
      if (a + (n - 1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    })

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => (rng => (f(a), rng)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a =>
    flatMap(rb)(b => (rng => {(f(a, b), rng)})))
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, nextState) = run(s)
    (f(a), nextState)
  })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(s => {
    val (a, nextState) = run(s)
    val (b, finalState) = sb.run(nextState)
    (f(a, b), finalState)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, nextState) = run(s)
    f(a).run(nextState)
  })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = State[S, List[A]](s =>
    l.foldRight((List[A](), s))((cur, acc) => {
      val (accList, accState) = acc
      val (newA, newState) = cur.run(accState)
      (newA :: accList, newState)
    })
  )

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

object Candy {

  def applyInputToMachine(input: Input)(machine: Machine): Machine = input match {
    case _ if machine.candies <= 0 => machine
    case Coin if machine.locked => Machine(locked = false, machine.candies, machine.coins + 1)
    case Turn if !machine.locked => Machine(locked = true, machine.candies - 1, machine.coins)
    case _ => machine
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (input => modify[Machine](applyInputToMachine(input))))
    s <- get
  } yield (s.coins, s.candies)
}
