package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => Cons(h, xs)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => if (n == 0) l else drop (xs, n - 1)
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(_, xs) => 1 + length(xs)
  }

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def prod3(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def len3[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    def f2(z: B, x: A): B = f(x, z)
    def reverse(l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc: List[A], x: A) => Cons(x, acc))
    foldLeft(reverse(l), z)(f2)
  }

  def append2[A](l1: List[A], l2: List[A]): List[A] = foldRight2(l1, l2)(Cons(_, _))

  def flatten[A](l: List[List[A]]): List[A] = foldRight2(l, Nil: List[A])(append2)

  def increment(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, increment(xs))
  }

  def doubleToString(l: List[Double]): List[String] = foldRight2(l, Nil: List[String])((x, res) => Cons(x.toString, res))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight2(l, Nil:List[B])((x, acc) => Cons(f(x), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight2(as, Nil: List[A])((x, res) =>
    if (f(x)) Cons(x, res)
    else res
  )

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)((x) => if (f(x)) List(x) else Nil)

  def zipWithSum(l1: List[Int], l2: List[Int]): List[Int] = {
    if (length(l1) != length(l2))
      throw new RuntimeException("diff len")
    else
      l1 match {
        case Nil => Nil
        case Cons(x, xs) => l2 match {
          case Nil => Nil
          case Cons(y, ys) => Cons(x + y, zipWithSum(xs, ys))
        }
      }
  }

  def zipWith[A, B](l1: List[A], l2: List[A])(f: (A, A) => B): List[B] = {
    if (length(l1) != length(l2))
      throw new RuntimeException("diff len")
    else
      l1 match {
        case Nil => Nil
        case Cons(x, xs) => l2 match {
          case Nil => Nil
          case Cons(y, ys) => Cons(f(x, y), zipWith(xs, ys)(f))
        }
      }
  }

  @tailrec
  def partialEquality[A](sup: List[A], sub: List[A]): Boolean = sub match {
    case Nil => true
    case Cons(x, xs) => sup match {
      case Nil => false
      case Cons(y, ys) => if (x == y) partialEquality(ys, xs) else false
    }
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    sub match {
      case Nil => true
      case Cons(x, xs) => sup match {
        case Nil => false
        case Cons(y, ys) =>
          if (partialEquality(sup, sub))
              true
          else
            hasSubsequence(ys, sub)
      }
    }
  }
}
