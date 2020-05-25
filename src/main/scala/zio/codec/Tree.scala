package zio.codec

import scala.annotation.tailrec
import scala.collection.mutable

object RangeDecomposition {
  sealed trait Tree[+A] extends Product with Serializable {
    @tailrec final def rightmost: A = this match {
      case Leaf(a) => a
      case Node(_, r) => r.rightmost
    }
  }
  final case class Leaf[+A](value: A) extends Tree[A]
  final case class Node[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  sealed trait Consecutive[+A] extends Product with Serializable {
    def left: A
    def right: A
  }
  final case class Point[A](value: A) extends Consecutive[A] {
    def left: A = value
    def right: A = value
  }
  final case class Range[A](left: A, right: A) extends Consecutive[A]

  def splitWhere[A](list: List[A])(f: (A, A) => Boolean): List[List[A]] = {
    def go
    (list: List[A], current: mutable.Builder[A, List[A]],
     result: mutable.Builder[List[A], List[List[A]]]
    ): List[List[A]] =
      list match {
        case Nil =>
          sys.error("impossible")
        case x :: Nil =>
          current += x
          result += current.result()
          result.result()
        case x :: y :: xs =>
          if (f(x, y)) {
            current += x
            result += current.result()
            current.clear()
            go(y :: xs, current, result)
          } else
            go(y :: xs, current += x, result)
      }

    list match {
      case Nil => Nil
      case _ => go(list, List.newBuilder, List.newBuilder)
    }
  }

  // println(splitWhere(List(1, 1, 1, 2, 2, 3, 3, 4))(_ != _))
  // println(splitWhere(List(1, 2, 3, 4, 3, 2, 1, 5, 6, 7, 4, 3))(_ >= _))

  def balance[A](list: List[A]): Tree[A] =
    list match {
      case Nil => sys.error("impossible")
      case x :: Nil => Leaf(x)
      case x :: y :: Nil => Node(Leaf(x), Leaf(y))
      case xs =>
        val n = xs.length
        Node(
          balance(xs.take(n / 2)),
          balance(xs.drop(n / 2))
        )
    }

  // println(balance(List(1, 2, 3)))
  // println(balance(List(1, 2, 3, 4)))
  // println(balance(List(1, 2, 3, 4, 5)))
  // println(balance(List(1, 2, 3, 4, 5, 6)))
  // println(balance(List(1, 2, 3, 4, 5, 6, 7)))

  def decompose(set: Set[Char]): Option[Tree[Consecutive[Char]]] = {
    if (set.isEmpty) None
    else {
      val ranges = splitWhere(set.toList.sorted)((x, y) => y != x + 1)
      val consecutives = ranges.map { r =>
        if (r.head == r.last) Point(r.head)
        else Range(r.head, r.last)
      }
      Some(balance(consecutives))
    }
  }

  // println(decompose("abc01234".toSet))
}

