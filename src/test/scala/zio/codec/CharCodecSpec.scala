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
    assert(decoder(c)(input))(isRight(equalTo((input.size - 1, expected))))

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
      check(genString1)(str =>
        decodeSuccess(token(str) | token(str.reverse), toChunk(str))(Left(toChunk(str))) &&
          decodeSuccess(token(str) | token("noise" + str.reverse), toChunk("noise" + str.reverse))(
            Right(toChunk("noise" + str.reverse))
          )
      )
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
    testM("opt string")( //this fails index -1 != 0
      check(genString1) { str =>
        decodeSuccess((token(str) ~ token("extra").ignore(Chunk.empty)).option, toChunk(str))(
          Some((toChunk(str), ()))
        )
      }
    ),
    suite("opt char")( //index is == 0
      testM("rep")(
        check(genString1)(str =>
          decodeSuccess((token(str) ~ consume.oneOf('e').rep.ignore(Chunk.empty)).option, toChunk(str))(
            Some((toChunk(str), ()))
          )
        )
      ),
      testM("repN == 1")( //this fails index -1 != 0
        check(genString1)(str =>
          decodeSuccess((token(str) ~ consume.oneOf('e').repN(3).ignore(Chunk.empty)).option, toChunk(str))(
            Some((toChunk(str), ()))
          )
        )
      )
    )
  )
}
