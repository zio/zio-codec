package zio.codec

import zio.Chunk

sealed trait Equiv[A, B]
object Equiv {
  final case class Left[A, B](b: B)  extends Equiv[(A, B), A]
  final case class Right[A, B](a: A) extends Equiv[(A, B), B]

  final case object UInt16 extends Equiv[Chunk[Boolean], Int]
}
