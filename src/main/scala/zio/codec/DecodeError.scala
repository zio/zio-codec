package zio.codec

final case class DecodeError(message: String, pos: Int)
