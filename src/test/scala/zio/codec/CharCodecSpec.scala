package zio.codec.examples

import zio.Chunk
import zio.codec.CharCodecModule
import zio.test._
import zio.test.Assertion._

object CharCodecSpec extends DefaultRunnableSpec {
  object CharTest extends CharCodecModule
  import CharTest._

  def toChunk[A](a: Iterable[A]) = Chunk.fromIterable(a)

  def decodeSuccess[A](c: Codec[A], input: Chunk[Char])(expected: A) =
    assert(decoder(c)(input))(isRight(equalTo((input.size - 1, expected))))

  def tokenAsSuccess[A](a: A) =
    decodeSuccess(tokenAs(a.toString, a), toChunk(a.toString))(a)

  case class Test(s: String, i: Int)

  val genCase = for {
    str <- Gen.string1(Gen.anyChar)
    i   <- Gen.anyInt
  } yield Test(str, i)

  val spec = suite("CharCodecSpec")(
    testM("token")(
      check(Gen.string1(Gen.anyChar))(str => decodeSuccess(token(str), toChunk(str))(toChunk(str)))
    ),
    suite("tokenAs")(
      testM("int")(check(Gen.anyInt)(tokenAsSuccess)),
      testM("byte")(check(Gen.anyByte)(tokenAsSuccess)),
      testM("string")(check(Gen.string1(Gen.anyChar))(tokenAsSuccess)),
      testM("case class")(check(genCase)(tokenAsSuccess)),
      testM("list")(check(Gen.listOf(Gen.anyInt))(tokenAsSuccess))
    )
  )
}
