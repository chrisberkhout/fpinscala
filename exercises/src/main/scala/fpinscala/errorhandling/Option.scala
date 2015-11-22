package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    if (this.map(f).getOrElse(false)) this
    else None
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
  //    mean(xs).map(m => xs.map(x => math.pow(x - m, 2))).flatMap(mean) // mine
  //    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2)))) // answer
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2)))) // mine rewritten == answer

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(aa), Some(bb)) => Some(f(aa, bb))
    case _ => None
  }

  def map2viaFlatMap[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa,bb)))


  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))

  // bad: this reverses the list
  def sequenceViaFoldLeft[A](a: List[Option[A]]): Option[List[A]] =
    a.foldLeft[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)((xx,yy) => yy :: xx))

  def sequenceViaMatch[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case None :: _ => None
    case Some(h) :: t => sequenceViaMatch(t).map((tt) => h :: tt)
  }

  // from answer: combine two last cases by using flatMap
  def sequenceViaMatchFlatMap[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap(hh => sequenceViaMatchFlatMap(t).map((tt) => hh :: tt))
  }


  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => f(h).flatMap(hh => traverse(t)(f).map((tt) => hh :: tt))
  }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)
}

object Test {
  def main(args: Array[String]): Unit = {
    println(s"map2 given one None returns ${Option.map2[Int,Int,Int](Some(1), None)(_+_)}")
    println(s"sequence ${Option.sequence(List(Some(1), Some(2), Some(3)))}")
    println(s"sequenceviaFoldLEft ${Option.sequenceViaFoldLeft(List(Some(1), Some(2), Some(3)))}")
    println(s"sequence... ${Option.sequenceViaTraverse(List(Some(1), Some(2), Some(3)))}")
    println(s"sequence... ${Option.sequenceViaTraverse(List(None, Some(2), None))}")
    println(s"sequence... ${Option.sequenceViaTraverse(List(Some(1), None, Some(3)))}")
    println(s"sequence... ${Option.sequenceViaTraverse(List(Some(1), Some(2), None))}")
  }
}