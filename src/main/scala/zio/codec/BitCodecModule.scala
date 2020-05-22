package zio.codec

import com.github.ghik.silencer.silent
import zio.Chunk
import zio.internal.Stack

trait BitCodecModule extends CodecModule {
  type Input = Boolean

  val uint16: Codec[Int] = consume.repN(16).map(Equiv.Bits.UInt16)

  @silent
  private def compileCodec[A](codec: Codec[A]): Array[CodecVM] = ???

  @silent
  private def interpret(input: Chunk[Input], codec: Array[CodecVM]): Either[DecodeError, Any] = {
    val stack: Stack[AnyRef] = Stack()

    var i: Int          = 0
    var inputIndex: Int = 0
    var r0: AnyRef      = null.asInstanceOf[AnyRef]

    while (i < codec.length) {
      val instr: CodecVM = codec(i)
      instr match {
        case CodecVM.Push(value) =>
          stack.push(value)

        case CodecVM.InputRead(min, max) =>
//          if (inputIndex + min < input.length

        case CodecVM.CheckSet(s) =>
          stack.push(s.set.contains(stack.pop()).asInstanceOf[AnyRef])

        case CodecVM.ACmpEq(address) =>
          if (stack.pop().eq(stack.pop())) i = address.address else i += 1

        case CodecVM.Call1(f) =>
          stack.push(f(stack.pop()))

        case CodecVM.Fail(err) =>
          return Left(DecodeError(err, i))

        case CodecVM.StoreRegister0 =>
          r0 = stack.pop()
      }
    }

    stack.pop() match {
      case null => Left(DecodeError("bug in our implementation, please report to us", 0))
      case v    => Right(v)
    }
  }
}
