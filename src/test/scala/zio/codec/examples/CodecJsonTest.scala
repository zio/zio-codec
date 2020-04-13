package zio.codec.examples

import zio.Chunk
import zio.codec.{ CharCodecModule, DecodeError, Equiv }

object CodecJsonTest {

  object JsonCodec extends CharCodecModule

  import Equiv._
  import JsonCodec._

  def char(c: Char): Codec[Char] =
    consume.filter(Set(c))

  val colon: Codec[Char] = char(':')
  val quote: Codec[Char] = char('"')

  val spacing: Codec[Unit] =
    consume.filter(Set(' ', '\n', '\r')).rep.ignore(Chunk.empty)

  val string: Codec[String] =
    ((spacing ~ quote, ((), '"')) ~> consume.filterNot(Set('\"', '\\')).rep <~ ('"', quote)).map(Chars.String)

  // todo: val field: Codec[(String, Val)] = (string <~ (':', colon)) ~ js
  val field: Codec[(String, String)] = (string <~ (':', colon)) ~ string

//  val dec: Chunk[Char] => Either[DecodeError, (Int, Unit)] = decoder(spacing)
  val dec: Chunk[Char] => Either[DecodeError, (Int, (String, String))] = decoder(field)

  def main(args: Array[String]): Unit =
    println(dec(Chunk.fromArray(""" "prop1": "val1" """.toCharArray)))
}
