package zio.codec.examples

import zio.Chunk
import zio.test._
import zio.test.Assertion._
import zio.codec.{ CharCodecModule, Equiv }

object CharCodecSpec extends DefaultRunnableSpec {
  object CharTest extends CharCodecModule
  import CharTest._

  def toChunk[A](a: Iterable[A]) = Chunk.fromIterable(a)

  def decodeSuccess[A](c: Codec[A], input: Chunk[Char])(expected: A) =
    assert(decoder(c)(input))(isRight(equalTo((input.size - 1, expected))))

  val genString1 = Gen.string1(Gen.anyChar)

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
      check(genString1, Gen.listOf1(Gen.string1(Gen.elements(' ')))) { (str, w) =>
        val pattern = (token(str) ~ consume.oneOf(' ').rep).rep
          .map(
            Equiv.ForTesting(
              _.fold[(Chunk[Input], Chunk[Input])]((Chunk.empty, Chunk.empty))((a, b) => (a._1 ++ b._1, a._2 ++ b._2))
            )
          )
        decodeSuccess(pattern, toChunk(w.map(str + _).mkString))((toChunk(str * w.length), toChunk(w.mkString)))
      }
    ),
    testM("repN") {
      check(genString1, Gen.small(i => Gen.stringBounded(1, i)(Gen.elements(' ')), 1))((str, w) =>
        decodeSuccess(token(str) ~ consume.oneOf(' ').repN(w.size), toChunk(str + w))(
          (toChunk(str), toChunk(w.mkString))
        )
      )
    }
  )
}
