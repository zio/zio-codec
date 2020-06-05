package zio.codec

import zio.Chunk

sealed trait Equiv[A, B]

object Equiv {
  final case class First[A, B](b: B)  extends Equiv[(A, B), A]
  final case class Second[A, B](a: A) extends Equiv[(A, B), B]

  final case class Merge[A]() extends Equiv[Either[A, A], A]

  final case class Ignore[A](a: A)      extends Equiv[A, Unit]
  final case class As[A, B](b: B, a: A) extends Equiv[A, B]

  // todo: remove
  final case class ForTesting[A, B](f: A => B) extends Equiv[A, B]

  object Bits {
    final case object UInt16 extends Equiv[Chunk[Boolean], Int]
  }

  object Chars {
    final case object String extends Equiv[Chunk[Char], String]
  }

  private[codec] def merge[A](arg: Either[A, A]): A =
    arg.merge
}
