package part1.chapter2

import scala.annotation.tailrec

sealed trait Lizt[+A]
case object Nill extends Lizt[Nothing]
case class Conz[A](head: A, tail: Lizt[A]) extends Lizt[A]

object Lizt {

  def apply[A](as: A*): Lizt[A] = {
    if (as.isEmpty) Nill
    else Conz(as.head, apply(as.tail: _*))
  }

  // 3.2
  def tail[A](l: Lizt[A]): Lizt[A] =
    l match {
      case Nill => Nill
      case Conz(_, tail) => tail
    }

  // 3.3
  def setHead[A](a: A, l: Lizt[A]): Lizt[A] = l match {
    case Nill => Conz(a, Nill)
    case Conz(_, tail) => Conz(a, tail)
  }

  // 3.4
  @tailrec
  def drop[A](n: Int, l: Lizt[A]): Lizt[A] =
    l match {
      case Nill => Nill
      case Conz(_,tail) if n > 0 => drop(n - 1, tail)
      case _ => l
    }

  // 3.5
  @tailrec
  def dropWhile[A](l: Lizt[A])(f: A => Boolean): Lizt[A] = l match {
    case Nill => Nill
    case Conz(head, tail) if f(head) => dropWhile(tail)(f)
    case _ => l
  }

  // 3.6
  def reverse[A](l: Lizt[A]): Lizt[A] = {
    @tailrec def loop(remaining: Lizt[A], acc: Lizt[A]): Lizt[A] =
      remaining match {
        case Nill => acc
        case Conz(head, tail) => loop(tail, Conz(head, acc))
    }
    loop(l, Nill)
  }


  def init[A](l: Lizt[A]): Lizt[A] =
    reverse(l) match {
      case Nill => Nill
      case Conz(_, tail) => tail
    }

  val l = Lizt(1, 2, 3)
  dropWhile(l)(x => x < 7)
}
