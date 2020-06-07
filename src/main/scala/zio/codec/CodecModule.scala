package zio.codec

import com.github.ghik.silencer.silent
import zio.Chunk

trait CodecModule {
  import Codec._

  type Input

  sealed trait Codec[A] { self =>

    final def map[B](equiv: Equiv[A, B]): Codec[B] = Codec.map(self, equiv)

    final def ignore(a: A): Codec[Unit]   = map(Equiv.Ignore(a))
    final def as[B](b: B, a: A): Codec[B] = map(Equiv.As(b, a))

    final def filter(set: Set[A]): Codec[A]                                = Codec.filter(self, set, FilterMode.Inside)
    final def filterNot(set: Set[A]): Codec[A]                             = Codec.filter(self, set, FilterMode.Outside)
    final def oneOf(values: A*): Codec[A]                                  = filter(values.toSet)
    final def notOneOf(values: A*): Codec[A]                               = filterNot(values.toSet)
    final def filter(f: A => Boolean)(implicit e: Enumerable[A]): Codec[A] = filter(e.enumerate.filter(f).toSet)

    final def zip[B](that: => Codec[B]): Codec[(A, B)] = Codec.zip(self, that)
    final def ~[B](that: => Codec[B]): Codec[(A, B)]   = zip(that)

    final def zipLeft[B](that: => Codec[B], b: B): Codec[A] = zip(that).map(Equiv.First(b))
    final def <~[B](b: B, that: => Codec[B]): Codec[A]      = zipLeft(that, b)

    final def zipRight[B](a: A, that: => Codec[B]): Codec[B] = zip(that).map(Equiv.Second(a))

    final def orElseEither[B](that: => Codec[B]): Codec[Either[A, B]] = Codec.alt(self, that)
    final def |[B](that: => Codec[B]): Codec[Either[A, B]]            = orElseEither(that)

    final def option: Codec[Option[A]] = Codec.opt(self)

    final def recover(default: A): Codec[A] = orElseEither(succeed(default)).map(Equiv.Merge())

    // todo: N should be natural
    final def repRange(range: Range): Codec[Chunk[A]] = Codec.rep(self, Some(range.start), Some(range.end))
    final def repN(n: Int): Codec[Chunk[A]]           = Codec.rep(self, Some(n), Some(n))
    final def rep: Codec[Chunk[A]]                    = Codec.rep(self, None, None)
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

  private[zio] sealed trait Codec0    extends Codec[Input]
  private[zio] sealed trait Codec1[A] extends Codec[A]

  object Codec {
    import scala.collection.JavaConverters._

    // format: off
    private[zio]       case object Consume                                                            extends Codec0
    private[zio] sealed case class Filter0(value: () => Codec0, filter: Set[Input], mod: FilterMode)  extends Codec0
                                   { @silent val hs: java.util.HashSet[Any] = new java.util.HashSet[Any](filter.asJava) }

    private[zio] sealed case class Box(codec: Codec0)                                                 extends Codec1[Input]
    private[zio] sealed case class Produce[A](a: A)                                                   extends Codec1[A]
    private[zio] sealed case class Fail[A](error: String)                                             extends Codec1[A]
    private[zio] sealed case class Map[A, B](equiv: Equiv[A, B], value: () => Codec1[A])              extends Codec1[B]
    private[zio] sealed case class Filter[A](value: () => Codec1[A], filter: Set[A], mod: FilterMode) extends Codec1[A]
                                   { @silent val hs: java.util.HashSet[Any] = new java.util.HashSet[Any](filter.asJava) }
    private[zio] sealed case class Zip[A, B](left: () => Codec1[A], right: () => Codec1[B])           extends Codec1[(A, B)]
    private[zio] sealed case class Opt[A](value: () => Codec1[A])                                     extends Codec1[Option[A]]
    private[zio] sealed case class Alt[A, B](left: () => Codec1[A], right: () => Codec1[B])           extends Codec1[Either[A, B]]
    private[zio] sealed case class Rep[A](value: () => Codec1[A], min: Option[Int], max: Option[Int]) extends Codec1[Chunk[A]]
    // format: on

    private[zio] sealed trait FilterMode
    private[zio] object FilterMode {
      case object Inside  extends FilterMode
      case object Outside extends FilterMode
    }

    private[zio] def boxed[A](c: Codec[A]): Codec1[A] =
      c match {
        case c: Codec0    => Box(c)
        case c: Codec1[_] => c
      }

    private def map[A, B](c: Codec[A], equiv: Equiv[A, B]): Codec[B] =
      c match {
        case c0: Codec0    => Map(equiv, () => boxed(c0))
        case c1: Codec1[_] => Map(equiv, () => c1)
      }

    private def filter[A](c: Codec[A], set: Set[A], mode: FilterMode): Codec[A] =
      c match {
        case c0: Codec0    => Filter0(() => c0, set, mode)
        case c1: Codec1[_] => Filter(() => c1, set, mode)
      }

    private def zip[A, B](c: Codec[A], that: => Codec[B]): Codec[(A, B)] =
      c match {
        case c0: Codec0    => Zip(() => Box(c0), () => boxed(that))
        case c1: Codec1[_] => Zip(() => c1, () => boxed(that))
      }

    private def alt[A, B](c: Codec[A], that: => Codec[B]): Codec[Either[A, B]] =
      c match {
        case c0: Codec0    => Alt(() => Box(c0), () => boxed(that))
        case c1: Codec1[_] => Alt(() => c1, () => boxed(that))
      }

    private def opt[A](c: Codec[A]): Codec[Option[A]] =
      c match {
        case c0: Codec0    => Opt(() => Box(c0))
        case c1: Codec1[_] => Opt(() => c1)
      }

    private def rep[A](c: Codec[A], min: Option[Int], max: Option[Int]): Codec[Chunk[A]] =
      c match {
        case c0: Codec0    => Rep(() => Box(c0), min, max)
        case c1: Codec1[_] => Rep(() => c1, min, max)
      }
  }
}
