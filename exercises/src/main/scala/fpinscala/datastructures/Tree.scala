package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

//  def size[A](t: Tree[A]): Int = t match {
//    case Leaf => 1
//    case Branch(left, right) => size(left) + size(right) + 1
//  }
//
//  def maximum(t: Tree[Int]): Int = t match {
//    case Leaf(value) => value
//    case Branch(left, right) => maximum(left) max maximum(right)
//  }
//
//  def depth[A](t: Tree[A]): Int = t match {
//    case Leaf => 0
//    case Branch(left, right) => depth(left).max(depth(right)) + 1
//  }
//
//  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
//    case Leaf(value) => Leaf(f(value))
//    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
//  }

  def fold[A,B](t: Tree[A])(f: (A) => B)(ff: (B,B) => B): B = t match {
    case Leaf(value) => f(value)
    case Branch(left, right) => ff(fold(left)(f)(ff), fold(right)(f)(ff))
  }

  def size[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)

  def maximum(t: Tree[Int]): Int = fold(t)(v => v)(_ max _)

  def depth[A](t: Tree[A]): Int = fold(t)(_ => 0)(_.max(_) + 1)

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_,_))

}