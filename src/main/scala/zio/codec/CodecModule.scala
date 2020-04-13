package zio.codec

import zio.Chunk

trait CodecModule {
  import Codec._

  type Input

  sealed trait Codec[A] { self =>

    def map[B](equiv: Equiv[A, B]): Codec[B] = Map(equiv, () => self)

    def ignore(a: A): Codec[Unit] = map(Equiv.Ignore(a))

    def filter(set: Set[A]): Codec[A]                                = Filter(() => self, set, FilterMode.Inside)
    def filterNot(set: Set[A]): Codec[A]                             = Filter(() => self, set, FilterMode.Outside)
    def filter(f: A => Boolean)(implicit e: Enumerable[A]): Codec[A] = filter(e.enumerate.filter(f).toSet)

    def zip[B](that: => Codec[B]): Codec[(A, B)] = Zip(() => self, () => that)
    def ~[B](that: => Codec[B]): Codec[(A, B)]   = zip(that)

    def zipLeft[B](that: => Codec[B], b: B): Codec[A] = zip(that).map(Equiv.Left(b))
    def <~[B](b: B, that: => Codec[B]): Codec[A]      = zipLeft(that, b)

    def zipRight[B](a: A, that: => Codec[B]): Codec[B] = zip(that).map(Equiv.Right(a))

    def orElseEither[B](that: => Codec[B]): Codec[Either[A, B]] = Alt(() => self, () => that)
    def |[B](that: => Codec[B]): Codec[Either[A, B]]            = orElseEither(that)

    def repRange(range: Range): Codec[Chunk[A]] = Rep(() => self, Some(range.start), Some(range.end))
    def repN(n: Int): Codec[Chunk[A]]           = Rep(() => self, Some(n), Some(n))
    def rep: Codec[Chunk[A]]                    = Rep(() => self, None, None)
    def * : Codec[Chunk[A]]                     = rep

  }

  implicit final class ToZipOps1[A](self: (Codec[A], A)) {

    def ~>[B](that: => Codec[B]): Codec[B] =
      self._1.zipRight(self._2, that)
  }

  val unit: Codec[Unit] = Produce(())

  def succeed[A](a: A): Codec[A] = Produce(a)

  def fail[A](error: String): Codec[A] = Fail(error)

  val consume: Codec[Input] = Consume

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
