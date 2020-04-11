package zio.codec

import zio.Chunk

sealed trait Equiv[A, B]

object Equiv {
  final case class Left[A, B](b: B)  extends Equiv[(A, B), A]
  final case class Right[A, B](a: A) extends Equiv[(A, B), B]

  final case class Ignore[A, Unit](a: A) extends Equiv[A, Unit]

  object Bits {
    final case object UInt16 extends Equiv[Chunk[Boolean], Int]
  }

  object Chars {
    final case object String extends Equiv[Chunk[Char], String]
  }
}
