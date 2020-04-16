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

  val dot: Codec[Char]      = consume.oneOf('.')
  val comma: Codec[Char]    = consume.oneOf(',')
  val colon: Codec[Char]    = consume.oneOf(':')
  val quote: Codec[Char]    = consume.oneOf('"')
  val bracket1: Codec[Char] = consume.oneOf('[')
  val bracket2: Codec[Char] = consume.oneOf(']')
  val brace1: Codec[Char]   = consume.oneOf('{')
  val brace2: Codec[Char]   = consume.oneOf('}')

  val spacing: Codec[Unit] =
    consume.oneOf(' ', '\n', '\r').rep.ignore(Chunk.empty)

  val digit: Codec[Char]             = betweenChar('0', '9')
  val sign: Codec[Option[Char]]      = consume.oneOf('-', '+').option
  val fractional: Codec[Chunk[Char]] = (dot, '.') ~> digit.rep
  val exponent: Codec[Chunk[Char]]   = (consume.oneOf('e', 'E') ~ sign, ('E', Some('+'))) ~> digit.rep
  // todo: repRange(1 to 100)
  val integral: Codec[(Char, Chunk[Char])] = digit ~ digit.rep

  // todo: map to case class
  val num: Codec[(((Option[Char], (Char, Chunk[Char])), Chunk[Char]), Chunk[Char])] =
    sign ~ integral ~ fractional.orEmpty ~ exponent.orEmpty

  val `null`: Codec[Null.type]   = tokenAs("null", Null)
  val `true`: Codec[True.type]   = tokenAs("true", True)
  val `false`: Codec[False.type] = tokenAs("false", False)

  // todo: map to case class
  val string: Codec[String] =
    ((spacing ~ quote, ((), '"')) ~> consume.filterNot(Set('\"', '\\')).rep <~ ('"', quote)).map(Chars.String)

  type ValToBe = Either[
    Either[Either[Either[String, Js.True.type], Js.False.type], Js.Null.type],
    (((Option[Char], (Char, Chunk[Char])), Chunk[Char]), Chunk[Char])
  ]

  val arr: JsonCodec.Codec[(ValToBe, Chunk[ValToBe])] =
    (bracket1, '[') ~> (js ~ ((comma, ',') ~> js).rep) <~ (((), ']'), spacing ~ bracket2)

  val field: Codec[(String, ValToBe)] = (string <~ (':', colon)) ~ js

  lazy val js: Codec[ValToBe] = (spacing, ()) ~> (string | `true` | `false` | `null` | num) <~ ((), spacing)

  val dec = decoder(js)

  def main(args: Array[String]): Unit = {
    println(dec(Chunk.fromArray("""kkk""".toCharArray)))
    println(dec(Chunk.fromArray(""""pro""".toCharArray)))
    println()

    println(dec(Chunk.fromArray(""" "prop1": "val1" """.toCharArray)))
    println()

    println(dec(Chunk.fromArray(""" true """.toCharArray)))
    println(dec(Chunk.fromArray(""" false """.toCharArray)))
    println(dec(Chunk.fromArray(""" null """.toCharArray)))
    println()

    println(dec(Chunk.fromArray(""" 5 """.toCharArray)))
    println(dec(Chunk.fromArray(""" 5.5 """.toCharArray)))
    println(dec(Chunk.fromArray(""" -33.777e+99 """.toCharArray)))
    println()

    println(decoder(arr)(Chunk.fromArray("""[ "ss", true, false, null, -12 ]""".toCharArray)))
  }
}
