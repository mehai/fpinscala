package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.
  
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }
  
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es) 
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
    def partF(a: A, b: B)(c: C): D = f(a, b, c)
    val partPar: Par[C => D] = map2(a, b)((aVal, bVal) => partF(aVal, bVal))
    map2(partPar, c)((f2, cVal) => f2(cVal))
  }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] { 
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val l = ps.map(asyncF(f))
    sequence(l)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case Nil => unit(Nil)
    case h :: t => map2(h, fork(sequence(t)))(_ :: _)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = as map (asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => 
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  def choiceN[A](cond: Par[Int])(l: List[Par[A]]): Par[A] =
    es =>
      l(run(es)(cond).get)(es)

  def choice2[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN( map(cond)(if (_) 0 else 1) )(List(t, f))

  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
    es => {
      val k = run(es)(key).get
      run(es)(choices(k))
    }

  def choiceAny[K, V](cond: Par[K])(choices: K => Par[V]): Par[V] =
    es => {
      val key = run(es)(cond).get
      run(es)(choices(key))
    }

  def choiceViaChoiceAny[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceAny(cond)(if (_) t else f)

  def choiceNViaChoiceAny[A](cond: Par[Int])(l: List[Par[A]]): Par[A] =
    choiceAny(cond)(l(_))

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    es => {
      val key = run(es)(a).get
      run(es)(f(key))
    }

  def join[A](a: Par[Par[A]]): Par[A] = es => run(es)(run(es)(a).get())

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(x => x)

  def flatMapViaJoin[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    join(map(a)(f))

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

  def parProc[A](arr: IndexedSeq[A], baseCase: A, f: (A, A) => A): Par[A] =
    if (arr.size <= 1)
      Par.unit[A](arr.headOption getOrElse baseCase)
    else {
      val (l, r) = arr.splitAt(arr.length / 2)
      Par.map2(parProc(l, baseCase, f), parProc(r, baseCase, f))(f(_, _))
    }

  def max(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit[Int](ints.headOption getOrElse Int.MinValue)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(max(l), max(r))(_ max _)
    }

  def countWords(paragraphs: List[String]): Par[Int] = {
    val wordCountList: Par[List[Int]] = Par.parMap(paragraphs)(_.split(" ").length)
    map(wordCountList)(lst => lst.sum)
  }
}
