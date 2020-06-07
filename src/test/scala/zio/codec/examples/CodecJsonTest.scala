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

  val digit: Codec[Char]             = charBetween('0', '9')
  val sign: Codec[Option[Char]]      = consume.oneOf('-', '+').option
  val fractional: Codec[Chunk[Char]] = (dot, '.') ~> digit.rep
  val exponent: Codec[Chunk[Char]]   = (consume.oneOf('e', 'E') ~ sign, ('E', Some('+'))) ~> digit.rep
  // todo: repRange(1 to 100)
  val integral: Codec[(Char, Chunk[Char])] = digit ~ digit.rep

  // todo: map to case class
  val num: Codec[Num] =
    (sign ~ integral ~ fractional.orEmpty ~ exponent.orEmpty)
      .map(Equiv.ForTesting({
        case (((s, (d1, ds)), l2), l3) =>
          Num((s.mkString + (d1 :: ds.toList).mkString + l2.mkString + l3.mkString).toDouble)
      }))

  val `null`: Codec[Null.type]   = tokenAs("null", Null)
  val `true`: Codec[True.type]   = tokenAs("true", True)
  val `false`: Codec[False.type] = tokenAs("false", False)

  // todo: map to case class
  val string: Codec[String] =
    ((spacing ~ quote, ((), '"')) ~> consume.filterNot(Set('\"', '\\')).rep <~ ('"', quote)).map(Equiv.Chars.String)
  val str: Codec[Str] = string
    .map(Equiv.ForTesting(Str.apply))

  // todo: map to list, then to case class
  val arr: Codec[Arr] =
    ((bracket1, '[') ~> (js ~ ((comma, ',') ~> js).rep) <~ (((), ']'), spacing ~ bracket2))
      .map(Equiv.ForTesting({
        case (head, rest) => Arr(head :: rest.toList: _*)
      }))

  val field: Codec[(String, Val)] = (string <~ (':', colon)) ~ js

  // todo: map to list, then to case class
  val obj: Codec[Obj] =
    ((brace1, '{') ~> (field ~ ((comma, ',') ~> field).rep <~ (((), '}'), spacing ~ brace2)))
      .map(Equiv.ForTesting({
        case (head, rest) => Obj(head :: rest.toList: _*)
      }))

  // todo: map to coproduct
  lazy val js: Codec[Val] = ((spacing, ()) ~> (obj | arr | str | `true` | `false` | `null` | num) <~ ((), spacing))
    .map(Equiv.ForTesting({
      case Left(Left(Left(Left(Left(Left(v))))))  => v
      case Left(Left(Left(Left(Left(Right(v)))))) => v
      case Left(Left(Left(Left(Right(v)))))       => v
      case Left(Left(Left(Right(v))))             => v
      case Left(Left(Right(v)))                   => v
      case Left(Right(v))                         => v
      case Right(v)                               => v
    }))

  val dec: Chunk[Char] => Either[DecodeError, (Int, Val)] = decoder(js)

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

    println(dec(Chunk.fromArray("""[ [ 111, 222, 333 ], "ss", true, false, null, -12 ]""".toCharArray)))
    println()

    // Right((373, Obj(List((firstName,Str(John)), (lastName,Str(Smith)), (age,Num(25.0)), (address,Obj(List((streetAddress,Str(21 2nd Street)), (city,Str(New York)), (state,Str(NY)), (postalCode,Num(10021.0))))), (phoneNumbers,Arr(List(Obj(List((type,Str(home)), (number,Str(212 555-1234)))), Obj(List((type,Str(fax)), (number,Str(646 555-4567)))))))))))
    println(dec(value))

    val t1: Long = System.nanoTime()
    (1 to 100000).foreach { _ =>
      dec(value)
    }
    val t2: Long = System.nanoTime() - t1
    println(
      s"\n\n Execution time = ${(t2 / 1000).toString.reverse.grouped(3).map(_.reverse).toList.reverse.mkString(",")} μs"
    )
  }

  // parserz 0.1.4
  //    10,000  in    - sec
  //   100,000  in  3.3 sec
  // 1,000,000  in 24.9 sec

  // parserz 0.2.0-α
  //    10,000  in    - sec
  //   100,000  in  1.9 sec
  // 1,000,000  in 14.1 sec

  // zio-codec 0.0.1
  //    10,000  in  0.8 sec
  //   100,000  in  4.1 sec
  // 1,000,000  in 35.8 sec

  val value: Chunk[Char] =
    Chunk.fromArray("""{
                      |  "firstName": "John",
                      |  "lastName": "Smith",
                      |  "age": 25,
                      |  "address": {
                      |      "streetAddress": "21 2nd Street",
                      |      "city": "New York",
                      |      "state": "NY",
                      |      "postalCode": 10021
                      |  },
                      |  "phoneNumbers": [
                      |      {
                      |          "type": "home",
                      |          "number": "212 555-1234"
                      |      },
                      |      {
                      |          "type": "fax",
                      |          "number": "646 555-4567"
                      |      }
                      |  ]
                      |}""".stripMargin.toCharArray)
}
