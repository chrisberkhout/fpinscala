package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object Test {
  def main(args: Array[String]): Unit = {
    println("%s".format(List(1,2,3)))
    println("x == 3 == %s".format(List.x))

    def variadic[Int](as: Int*): Unit = println("as == %s".format(as))
    variadic(1,2,3)
    variadic(Seq(1,2,3))
    variadic(Seq(1,2,3): _*)

    println("ex 3.8 foldRight Nil Cons... %s".format(List.foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_))))
    // data constructors of List can be considered functions.

    println("length(List(1,2,3,4) == 5 == %s".format(List.length(List(1,2,3,4,5))))

    println("ex 3.10 foldLeft Nil + == 6 == %s".format(List.foldLeft(List(1,2,3), 0)(_+_)))

    println("llength(List(1,2,3,4) == 5 == %s".format(List.llength(List(1,2,3,4,5))))

    println("reverse(List(1,2,3,4) == List(4,3,2,1) == %s".format(List.reverse(List(1,2,3,4))))

    println("foldLeft2 llength List(1,2,3,4) == 4 == %s".format(List.foldLeft2(List(1,2,3,4), 0)((a,b) => a + 1)))

    println("foldRight2 (1-(2-(3-(0))) == 2 == %s".format(List.foldRight2(List(1,2,3), 0)(_-_)))

    println("append2(List(1,2), List(3,4)) == List(1,2,3,4) == %s".format(List.append2(List(1,2),List(3,4))))

    println("concat(List(List(1,2), List(3,4))) == List(1,2,3,4) == %s".format(List.concat(List(List(1,2),List(3,4)))))

    println("filter(List(1,2,3,4,5)) == List(2,4) == %s".format(List.filter(List(1,2,3,4,5))(_ % 2 == 0)))

    println("flatMap(List(1,2,3))(i => List(i,i)) == List(1,1,2,2,3,3) == %s".format(List.flatMap(List(1,2,3))(i => List(i,i))))

    println("filter2(List(1,2,3,4,5)) == List(2,4) == %s".format(List.filter2(List(1,2,3,4,5))(_ % 2 == 0)))

    println("zipSum(List(1,2,3),List(4,5,6)) == List(5,7,9) == %s".format(List.zipSum(List(1,2,3),List(4,5,6))))
    println("zipSum(List(1,2),List(4,5,6)) == List(5,7,6) == %s".format(List.zipSum(List(1,2),List(4,5,6))))
    println("zipSum(List(1,2,3),List(4,5)) == List(5,7,3) == %s".format(List.zipSum(List(1,2,3),List(4,5))))

    println("zipWith(List(1,2),List(3,4))(_*_) == List(3,8) == %s".format(List.zipWith(List(1,2),List(3,4))(_*_)))
    println("zipWith(List(2),List(3,4))(_*_) == List(6) == %s".format(List.zipWith(List(2),List(3,4))(_*_)))
    println("zipWith(List(3,4),List(2))(_*_) == List(6) == %s".format(List.zipWith(List(3,4),List(2))(_*_)))

    println("hasSubsequence(List(1,2,3,4),List(2,3)) == true == %s".format(List.hasSubsequence(List(1,2,3,4),List(2,3))))
    println("hasSubsequence(List(1,2,3,4),List(2,4)) == false == %s".format(List.hasSubsequence(List(1,2,3,4),List(2,4))))
  }
}

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
    case Cons(_, xs) => xs
    case _ => Nil // could be None
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, xs) => Cons(h, xs)
    case _ => Cons(h, Nil)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n > 0) drop(tail(l), n-1)
    else l

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => xs
    case _ => l
  }

  // ex 3.6
  def init[A](l: List[A]): List[A] = l match {
    // isn't O(1) because it needs to find the end, and all preceding items needs to be recreated
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((a,b) => 1 + b)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, z)((b,a) => f(a,b))

  def foldRight2[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, z)((a,b) => f(b,a))

  def lsum(l: List[Int]) = foldLeft(l, 0)(_+_)

  def lproduct(l: List[Int]) = foldLeft(l, 1)(_*_)

  def llength[A](l: List[A]) = foldLeft(l, 0)((a,b) => a + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((a,b) => Cons(b, a))

  def append2[A](ll: List[A], lr: List[A]): List[A] = foldRight(ll, lr)(Cons(_,_))

  def concat[A](ol: List[List[A]]): List[A] = foldRight(ol, Nil: List[A])(append2)

  def plus1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((a,b) => Cons(a+1, b))

  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((a,b) => Cons(a.toString, b))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((h,t) => Cons(f(h), t))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, Nil: List[A])((h,t) => if (f(h)) Cons(h, t) else t)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil: List[B])((h,t) => append2(f(h), t))

  def filter2[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(x => if (f(x)) List(x) else List())

  def zip[A](l: List[A], r: List[A]): List[List[A]] = {
    l match {
      case Nil => r match {
        case Nil => Nil
        case Cons(rh,rt) => Cons(List(rh), zip(Nil,rt))
      }
      case Cons(lh,lt) => r match {
        case Nil => Cons(List(lh), zip(lt,Nil))
        case Cons(rh,rt) => Cons(List(lh,rh), zip(lt,rt))
      }
    }
    // these would be better combined into `(l,r) match { ... }`
  }

  def zipSum(l: List[Int], r: List[Int]): List[Int] = map(zip(l, r))(sum)

  def zipWith[A](l: List[A], r: List[A])(f: (A,A) => A): List[A] = {
    def applyF(as: List[A]): List[A] = as match {
      case Cons(a,Cons(b, Nil)) => Cons(f(a,b), Nil)
      case _ => Nil
    }
    flatMap(zip(l, r))(applyF)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def equalPair[A](l: List[A]): Boolean = l match {
      case Cons(a, Cons(b, Nil)) => a == b
      case _ => false
    }
    length(sup) > 0 && (
      sum(map(filter(zip(sup, sub))(length(_) == 2))((pair) => if (equalPair(pair)) 1 else 0)) == length(sub) ||
        hasSubsequence(tail(sup), sub)
    )
  }

}
