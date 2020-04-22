package zio.codec.examples

import zio.Chunk
import zio.test._
import zio.test.Assertion._
import zio.codec.{ CharCodecModule }

object CharCodecSpec extends DefaultRunnableSpec {
  object CharTest extends CharCodecModule
  import CharTest._

  def toChunk[A](a: Iterable[A]) = Chunk.fromIterable(a)

  def decodeSuccess[A](c: Codec[A], input: Chunk[Char])(expected: A) =
    assert(decoder(c)(input))(isRight(equalTo((input.size, expected))))

  val genString1 = Gen.string1(Gen.alphaNumericChar)

  val spec = suite("CharCodecSpec")(
    testM("token")(
      check(genString1)(str => decodeSuccess(token(str), toChunk(str))(toChunk(str)))
    ),
    testM("token")(
      check(genString1)(str => decodeSuccess(token(str), toChunk(str))(toChunk(str)))
    ),
    testM("tokenAs")(
      check(Gen.anyInt)(a => decodeSuccess(tokenAs(a.toString, a), toChunk(a.toString))(a))
    ),
    testM("~")(
      check(genString1)(str =>
        decodeSuccess(token(str) ~ token(str.reverse), toChunk(str + str.reverse))((toChunk(str), toChunk(str.reverse)))
      )
    ),
    testM("|")(
      check(genString1)(str => decodeSuccess(token(str) | token(str.reverse), toChunk(str))(Left(toChunk(str))))
    ),
    testM("rep")(
      check(Gen.anyChar, Gen.small(Gen.int(1, _), 1)) { (c, i) =>
        val chars = toChunk(List.fill(i)(c))
        decodeSuccess(consume.oneOf(c).rep, chars)(chars)
      }
    ),
    testM("repN")(
      check(Gen.anyChar, Gen.small(Gen.int(1, _), 1)) { (c, i) =>
        val chars = toChunk(List.fill(i)(c))
        decodeSuccess(consume.oneOf(c).repN(i), chars)(chars)
      }
    ),
    testM("opt")(
      check(genString1)(str => assert(decoder(token("extra").option)(toChunk(str)))(isRight(equalTo((0, None)))))
    )
  )
}
