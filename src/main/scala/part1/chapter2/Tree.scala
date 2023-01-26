package part1.chapter2

import scala.annotation.tailrec
import Tree._

sealed trait Tree[+A] {

  final def foldTail[B](init: B)(f: (B, A) => B): B = {

    @tailrec
    def drown(path: List[Path[A]]): List[Path[A]] = path match {
      case ToLeft(l @ Branch(_, _), _) :: _  => drown(ToLeft(l) :: path)
      case ToLeft(Leaf(v), _) :: _           => Destination(v) :: path
      case ToRight(_, r @ Branch(_, _)) :: _ => drown(ToLeft(r) :: path)
      case ToRight(_, Leaf(v)) :: _          => Destination(v) :: path
      case _                                 => path
    }

    @tailrec
    def bubble(path: List[Path[A]]): List[Path[A]] = path match {
      case Nil                        => path
      case (l @ ToLeft(_, _)) :: tail => ToRight(l.branch) :: tail
      case _ :: tail                  => bubble(tail)
    }

    @tailrec
    def rec(acc: B, path: List[Path[A]]): B = path match {
      case Nil => acc
      case Destination(v) :: _ =>
        val newAcc = f(acc, v)
        val newSplittingPoint = bubble(path)
        rec(newAcc, newSplittingPoint)
      case path =>
        val p = drown(path)
        rec(acc, p)
    }

    this match {
      case b @ Branch(_, _) =>
        val path = drown(ToLeft(b) :: Nil)
        rec(init, path)
      case Leaf(v) => f(init, v)
    }
  }

  final def fold[B](init: B)(f: (B, A) => B): B =
    this match {
      case Leaf(value)         => f(init, value)
      case Branch(left, right) => right.fold(left.fold(init)(f))(f)
    }

}

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case class Leaf[A](value: A) extends Tree[A]


object Tree {
  sealed trait Path[A]

  case class ToLeft[A](branch: Branch[A]) extends Path[A]

  case object ToLeft {
    def unapply[A](l: ToLeft[A]): Some[(Tree[A], Tree[A])] =
      Some(l.branch.left, l.branch.right)
  }

  case class ToRight[A](branch: Branch[A]) extends Path[A]

  case object ToRight {
    def unapply[A](r: ToRight[A]): Some[(Tree[A], Tree[A])] =
      Some(r.branch.left, r.branch.right)
  }

  case class Destination[A](value: A) extends Path[A]
}


