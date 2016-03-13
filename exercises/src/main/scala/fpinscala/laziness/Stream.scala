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

  // 5.13 - map, take, takeWhile, zipWidth and zipAll via unfold

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case Empty => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this,n)) {
    case (Cons(h,t), z) if z > 0 => Some(h(), (t(),z-1))
    case _ => None
  }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h,t) if f(h()) => Some(h(), t())
    case _ => None
  }

  def zipWithViaUnfold[B,C](rs: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, rs)) {
    case (Cons(lh,lt), Cons(rh,rt)) => Some(( f(lh(),rh()), (lt(),rt()) ))
    case _ => None
  }

  def zipAllViaUnfold[B](rs: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, rs)) {
    case (Cons(lh,lt), Cons(rh,rt)) => Some( (Some(lh()),Some(rh())), (lt(),rt()) )
    case (Cons(lh,lt), Empty) => Some( (Some(lh()),None), (lt(),Empty) )
    case (Empty, Cons(rh,rt)) => Some( (None,Some(rh())), (Empty,rt()) )
    case _ => None
  }

  // 5.14 - startsWith using functions you've written

  def startsWith[B](s: Stream[B]): Boolean =
    this.zipAllViaUnfold[B](s).takeWhile(!_._2.isEmpty).forAll { case (a,b) => a == b }

  // 5.15 - tails using unfold

  def tails: Stream[Stream[A]] = unfold(this) {
    case Cons(h,t) => Some(Cons(h,t), t())
    case _ => None
  }.append(Stream(empty))

  // 5.16 - generalize tails to the function scanRight

  def scanRightNotLinearInN[B](z: => B)(f: (A, => B) => B): Stream[B] =
      this.tails.map(i => i.foldRight(z)(f))

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    this.foldRight((z, Stream(z)))((newItem, p0) => {
      lazy val p1 = p0 // lazy to ensure one evaluation
      val lastIntermediate = p1._1
      val lastAcc = p1._2

      val newIntermediate = f(newItem, lastIntermediate)
      val newAcc = cons(newIntermediate, lastAcc)

      (newIntermediate, newAcc)
    })._2

  // this adds a head evaluation of the accumulator stream to each iteration, but it's still linear in n
  def scanRightWithOnlyAcc[B](z: => B)(f: (A, => B) => B): Stream[B] =
    this.foldRight(Stream(z))((newItem, acc) => {
      val newResult = f(newItem, acc.headOption.get)
      cons(newResult, acc)
    })

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

  def constant[A](a: A): Stream[A] = {
    lazy val result: Stream[A] = Stream.cons(a, result)
    result
  }

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def fibsBody(ll: Int, l: Int): Stream[Int] = Stream.cons(ll+l, fibsBody(l, ll+l))
    Stream.cons(0, Stream.cons(1, fibsBody(0, 1)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  def fibsViaUnfold: Stream[Int] = Stream.cons(0, Stream.cons(1, unfold((0,1))(z => Some((z._2+z._1, (z._1, z._2+z._1))))))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(z => Some((z, z+1)))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(z => Some(z, z))

  def onesViaUnfold: Stream[Int] = unfold(1)(z => Some(1, 1))

}

object StreamTest {
  def main(args: Array[String]): Unit = {
    println(s"test")
  }
}
