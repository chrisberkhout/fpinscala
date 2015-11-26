package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => List.empty
    case Cons(h, t) => h() :: t().toList
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n-1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)
//  Cons[Int](() => {println("1");1}, () => Cons(() => {println("2");2}, () => Cons(() => {println("3");3}, () => Cons(() => {println("4");4}, () => {println("Empty");Empty})))).forAll(_ < 3)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => if (p(a)) Cons[A](() => a, () => b) else Empty)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => Cons[B](() => f(a), () => b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => if (f(a)) Cons[A](() => a, () => b) else b)

  def append[B >: A](xs: => Stream[B]): Stream[B] =
    foldRight(xs)((a, b) => Cons[B](() => a, () => b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
   foldRight(Stream.empty[B])((a, b) => f(a).append(b))


  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}

object StreamTest {
  def main(args: Array[String]): Unit = {
    println(s"test")
  }
}