package zio.codec.examples

import zio.Chunk
import zio.codec.{ CharCodecModule, Equiv }

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

  val dot: Codec[Char]      = char('.')
  val comma: Codec[Char]    = char(',')
  val colon: Codec[Char]    = char(':')
  val quote: Codec[Char]    = char('"')
  val bracket1: Codec[Char] = char('[')
  val bracket2: Codec[Char] = char(']')
  val brace1: Codec[Char]   = char('{')
  val brace2: Codec[Char]   = char('}')

  val spacing: Codec[Unit] =
    consume.filter(Set(' ', '\n', '\r')).rep.ignore(Chunk.empty)

  val digits: Codec[Chunk[Char]]   = consume.filter(Set('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')).rep
  val sign: Codec[Option[Char]]    = consume.filter(Set('-', '+')).option
  val integral: Codec[Chunk[Char]] = digits

  val num: Codec[(Option[Char], Chunk[Char])] = sign ~ integral

  val `null`: Codec[Null.type]   = tokenAs("null", Null)
  val `true`: Codec[True.type]   = tokenAs("true", True)
  val `false`: Codec[False.type] = tokenAs("false", False)

  val string: Codec[String] =
    ((spacing ~ quote, ((), '"')) ~> consume.filterNot(Set('\"', '\\')).rep <~ ('"', quote)).map(Chars.String)

  // todo: val field: Codec[(String, Val)] = (string <~ (':', colon)) ~ js
  val field: Codec[(String, String)] = (string <~ (':', colon)) ~ string

  val js = (spacing, ()) ~> (field | `true` | `false` | `null` | num) <~ ((), spacing)

  val dec = decoder(js)

  def main(args: Array[String]): Unit = {
    println(dec(Chunk.fromArray("""kkk""".toCharArray)))
    println(dec(Chunk.fromArray(""""pro""".toCharArray)))

    println(dec(Chunk.fromArray(""" "prop1": "val1" """.toCharArray)))
    println(dec(Chunk.fromArray(""" true """.toCharArray)))
    println(dec(Chunk.fromArray(""" false """.toCharArray)))
    println(dec(Chunk.fromArray(""" null """.toCharArray)))
    println(dec(Chunk.fromArray(""" 5 """.toCharArray)))
    println(dec(Chunk.fromArray(""" -3 """.toCharArray)))
  }
}
