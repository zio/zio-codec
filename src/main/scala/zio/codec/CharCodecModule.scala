package zio.codec

import zio.Chunk

trait CharCodecModule extends CodecModule {
  type Input = Char

  def decoder[A](codec: Codec[A]): Chunk[Input] => Either[DecodeError, (Int, A)] = {
    val compiled = compileCodec(codec)
    interpret(_, compiled).asInstanceOf[Either[DecodeError, (Int, A)]]
  }

  def encoder[A](codec: Codec[A]): A => Either[EncodeError, Chunk[Input]] = ???

  def printer[A](codec: Codec[A]): List[String] = ???

  private def compileCodec[A](codec: Codec[A]): Array[CodecVM] = ???

  private def interpret(input: Chunk[Input], codec: Array[CodecVM]): Either[DecodeError, (Int, Any)] = ???
}
