package zio.codec

import zio.Chunk

trait CodecModule {
  import Codec._

  type Input

  sealed trait Codec[A] { self =>

    final def map[B](equiv: Equiv[A, B]): Codec[B] = Map(equiv, () => self)

    final def ignore(a: A): Codec[Unit]   = map(Equiv.Ignore(a))
    final def as[B](b: B, a: A): Codec[B] = map(Equiv.As(b, a))

    final def oneOf(s: A*): Codec[A]                                       = filter(s.toSet)
    final def filter(set: Set[A]): Codec[A]                                = Filter(() => self, set, FilterMode.Inside)
    final def filterNot(set: Set[A]): Codec[A]                             = Filter(() => self, set, FilterMode.Outside)
    final def filter(f: A => Boolean)(implicit e: Enumerable[A]): Codec[A] = filter(e.enumerate.filter(f).toSet)

    final def zip[B](that: => Codec[B]): Codec[(A, B)] = Zip(() => self, () => that)
    final def ~[B](that: => Codec[B]): Codec[(A, B)]   = zip(that)

    final def zipLeft[B](that: => Codec[B], b: B): Codec[A] = zip(that).map(Equiv.Left(b))
    final def <~[B](b: B, that: => Codec[B]): Codec[A]      = zipLeft(that, b)

    final def zipRight[B](a: A, that: => Codec[B]): Codec[B] = zip(that).map(Equiv.Right(a))

    final def orElseEither[B](that: => Codec[B]): Codec[Either[A, B]] = Alt(() => self, () => that)
    final def |[B](that: => Codec[B]): Codec[Either[A, B]]            = orElseEither(that)

    final def option: Codec[Option[A]] = Opt(() => self)

    final def recover(default: A): Codec[A] = orElseEither(succeed(default)).map(Equiv.Merge())

    // todo: N should be natural
    final def repRange(range: Range): Codec[Chunk[A]] = Rep(() => self, Some(range.start), Some(range.end))
    final def repN(n: Int): Codec[Chunk[A]]           = Rep(() => self, Some(n), Some(n))
    final def rep: Codec[Chunk[A]]                    = Rep(() => self, None, None)
    final def * : Codec[Chunk[A]]                     = rep
  }

  implicit final class ToZipOps1[A](self: (Codec[A], A)) {
    def ~>[B](that: => Codec[B]): Codec[B] =
      self._1.zipRight(self._2, that)
  }

  implicit final class ToChunkOps1[A](self: Codec[Chunk[A]]) {
    def orEmpty: Codec[Chunk[A]] =
      self.recover(Chunk.empty)
  }

  final val unit: Codec[Unit] = Produce(())

  final def succeed[A](a: A): Codec[A] = Produce(a)

  final def fail[A](error: String): Codec[A] = Fail(error)

  final val consume: Codec[Input] = Consume

  def decoder[A](codec: Codec[A]): Chunk[Input] => Either[DecodeError, (Int, A)]
  def encoder[A](codec: Codec[A]): A => Either[EncodeError, Chunk[Input]]

  def printer[A](codec: Codec[A]): List[String]

  // format: off
  object Codec {
    private[zio] sealed case class Produce[A](a: A)                                                  extends Codec[A]
    private[zio] sealed case class Fail[A](error: String)                                            extends Codec[A]
    private[zio]       case object Consume                                                           extends Codec[Input]
    private[zio] sealed case class Map[A, B](equiv: Equiv[A, B], value: () => Codec[A])              extends Codec[B]
    private[zio] sealed case class Filter[A](value: () => Codec[A], filter: Set[A], mod: FilterMode) extends Codec[A]
    private[zio] sealed case class Zip[A, B](left: () => Codec[A], right: () => Codec[B])            extends Codec[(A, B)]
    private[zio] sealed case class Opt[A](value: () => Codec[A])                                     extends Codec[Option[A]]
    private[zio] sealed case class Alt[A, B](left: () => Codec[A], right: () => Codec[B])            extends Codec[Either[A, B]]
    private[zio] sealed case class Rep[A](value: () => Codec[A], min: Option[Int], max: Option[Int]) extends Codec[Chunk[A]]

    private[zio] sealed trait FilterMode
    private[zio] object FilterMode {
      case object Inside  extends FilterMode
      case object Outside extends FilterMode
    }
  }
  // format: on
}
