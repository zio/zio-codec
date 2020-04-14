package zio.codec.examples

import zio.Chunk
import zio.codec.{ CharCodecModule, DecodeError, Equiv }

object CodecJsonTest {

  object Js {
    sealed trait Val extends Any {
      def value: Any
    }
    case class Str(value: String)         extends AnyVal with Val
    case class Obj(value: (String, Val)*) extends AnyVal with Val
    case class Arr(value: Val*)           extends AnyVal with Val
    case class Num(value: Double)         extends AnyVal with Val
    case object False                     extends Val { def value: Any = false }
    case object True                      extends Val { def value: Any = true }
    case object Null                      extends Val { def value: Any = null }
  }

  object JsonCodec extends CharCodecModule

  import Js._
  import Equiv._
  import JsonCodec._

  def char(c: Char): Codec[Char] =
    consume.filter(Set(c))

  val colon: Codec[Char] = char(':')
  val quote: Codec[Char] = char('"')

  val spacing: Codec[Unit] =
    consume.filter(Set(' ', '\n', '\r')).rep.ignore(Chunk.empty)

  val `null`: Codec[Null.type] = tokenAs("null", Null)

  val string: Codec[String] =
    ((spacing ~ quote, ((), '"')) ~> consume.filterNot(Set('\"', '\\')).rep <~ ('"', quote)).map(Chars.String)

  // todo: val field: Codec[(String, Val)] = (string <~ (':', colon)) ~ js
  val field: Codec[(String, String)] = (string <~ (':', colon)) ~ string

  val js: Codec[Either[(String, String), Js.Null.type]] =
    (spacing, ()) ~> (field | `null`) <~ ((), spacing)

  val dec: Chunk[Char] => Either[DecodeError, (Int, Either[(String, String), Js.Null.type])] = decoder(js)

  def main(args: Array[String]): Unit = {
    println(dec(Chunk.fromArray("""kkk""".toCharArray)))
    println(dec(Chunk.fromArray(""""pro""".toCharArray)))

    println(dec(Chunk.fromArray(""" null """.toCharArray)))
    println(dec(Chunk.fromArray(""" "prop1": "val1" """.toCharArray)))
  }
}
