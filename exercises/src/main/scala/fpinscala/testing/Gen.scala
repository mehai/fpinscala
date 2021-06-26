package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.{Par, equal}
import Gen._
import Prop._

import java.util.concurrent.{ExecutorService, Executors}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {(ms, n, rng) =>
    run(ms, n, rng) match {
      case Passed | Proved => p.run(ms, n, rng)
      case x => x
    }
  }

  def ||(p: Prop): Prop = Prop {(ms, n, rng) =>
    run(ms, n, rng) match {
      case Falsified(msg, _) => p.tag(msg).run(ms, n, rng)
      case x => x
    }
  }

  def tag(msg: String): Prop = Prop {(ms, n, rng) =>
    run(ms, n, rng) match {
      case Falsified(e, c) => Falsified(s"$msg\n$e", c)
      case x => x
    }
  }
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified: Boolean = true
  }

  case object Proved extends Result {
    def isFalsified: Boolean = false
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props.map(p => Prop {
        (max, _, rng) => p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (ms, n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p,p2)(_ == _)

  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = forAll(S ** g){case (s, a) => f(a)(s).get}

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  val p2 = checkPar {
    equal (Par.map(Par.unit(1))(_ + 1), Par.unit(2))
  }

  val pint = Gen.choose(0, 10) map (Par.unit(_))
  val p4 = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))

  lazy val pint2: Gen[Par[Int]] = choose(-100,100).listOfN(choose(0,20)).map(l =>
    l.foldLeft(Par.unit(0))((p,i) =>
      Par.fork { Par.map2(p, Par.unit(i))(_ + _) }))

  val forkPar = forAllPar(pint2)(n => equal(Par.fork(n), n)) tag "fork"

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property.")
    }
}

case class Gen[+A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def map[B](f: A => B): Gen[B] = Gen(this.sample.map(f))

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap (Gen.listOfN(_, this))

  def unsized: SGen[A] = SGen(s => this)

  def get(r: RNG): A = this.sample.run(r)._1

  def **[B](g: Gen[B]): Gen[(A, B)] = (this map2 g)((_, _))
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))

  def boolean: Gen[Boolean] = Gen(State(rng => {
    val (nextI, nextRng) = rng.nextInt
    (nextI % 2 == 0, nextRng)
  }))

  def double: Gen[Double] = Gen(State(RNG.double))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => listOfN(n, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => listOfN((1 min n), g))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeLessThan(stopExclusive)).map(_ + start))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean flatMap(if (_) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = double flatMap(decider => {
    val (gen1, w1) = g1
    val (gen2, w2) = g2
    if (decider * (w1 + w2) <= w1) gen1 else gen2
  })

  def map2[A, B, C](gA: Gen[A], gB: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(State(rng => {
      val (a, r1) = gA.sample.run(rng)
      val (b, r2) = gB.sample.run(r1)
      (f(a, b), r2)
    }))
}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] = SGen(n => this(n).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(n => this(n).flatMap(a => f(a)(n)))

  def map2[B, C](g2: SGen[B])(f: (A, B) => C): SGen[C] = SGen(n => this(n).map2(g2(n))(f))
}

