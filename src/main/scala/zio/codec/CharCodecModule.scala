package zio.codec

import zio.Chunk
import zio.internal.Stack

trait CharCodecModule extends CodecModule {
  type Input = Char

  def decoder[A](codec: Codec[A]): Chunk[Input] => Either[DecodeError, (Int, A)] = {
    val compiled = compileCodec(codec, 0)
//    println(compiled.zipWithIndex.map { case (o, i) => i.toString.reverse.padTo(3, '0').reverse + ": " + o }.mkString("", "\n", "\n"))
    interpret(_, compiled).asInstanceOf[Either[DecodeError, (Int, A)]]
  }

  def encoder[A](codec: Codec[A]): A => Either[EncodeError, Chunk[Input]] = ???

  def printer[A](codec: Codec[A]): List[String] = ???

  private case object NoValue

  private def compileCodec[A](codec: Codec[A], offset: Int): Array[CodecVM] = {
    import Codec._
    import Codec.FilterMode._

    val VM = CodecVM

    codec match {
      case Produce(a) =>
        Array(
          VM.Push(a.asInstanceOf[AnyRef])
        )

      case Fail(error) =>
        Array(
          VM.Fail(error)
        )

      case Consume =>
        Array(
          VM.Read(None, None)
        )

      case map: Map[_, _] =>
        val program = compileCodec(map.value(), offset) ++ Array(
          VM.Duplicate,
          VM.Push(NoValue)
        )

        val mapProgram: Array[CodecVM] = map.equiv match {
          case Equiv.Left(_)      => Array(VM.Construct1(_.asInstanceOf[(AnyRef, _)]._1))
          case Equiv.Right(_)     => Array(VM.Construct1(_.asInstanceOf[(_, AnyRef)]._2))
          case Equiv.Ignore(_)    => Array(VM.Pop, VM.Push(().asInstanceOf[AnyRef]))
          case Equiv.Chars.String => Array(VM.Construct1(_.asInstanceOf[Chunk[Char]].mkString))
          case Equiv.Bits.UInt16  => ???
        }

        val lMap  = offset + program.length + 1
        val lBail = offset + program.length + 1 + mapProgram.length

        program ++ Array(
          VM.JumpCond(lBail, lMap)
        ) ++ mapProgram

      case Filter(value, in, mod) =>
        val program = compileCodec(value(), offset) ++ Array(
          VM.Duplicate,
          VM.Push(NoValue)
        )

        val lFilter   = offset + program.length + 1
        val lMismatch = offset + program.length + 5
        val lMatch    = offset + program.length + 7
        val lBail     = offset + program.length + 7

        program ++ Array(
          VM.JumpCond(lBail, lFilter),
          VM.Duplicate,
          VM.CheckSet(in.map(_.asInstanceOf[AnyRef])),
          VM.Push((mod match {
            case Inside  => true
            case Outside => false
          }).asInstanceOf[AnyRef]),
          VM.JumpCond(lMatch, lMismatch),
          VM.Pop,
          VM.Push(NoValue)
        )

      case Zip(left, right) =>
        // todo: check result
        val program = compileCodec(left(), offset)
        program ++ compileCodec(right(), offset + program.length) ++ Array(
          VM.Construct2(Tuple2.apply)
        )

      case Alt(_, _) =>
        // todo
        ???
//        println(right)
//        compileCodec(left(), offset)

      case Rep(value, None, None) =>
        val program = Array(
          VM.Push(Chunk.empty),
          VM.FramePush
        ) ++ compileCodec(value(), offset + 2) ++ Array(
          VM.Duplicate,
          VM.Push(NoValue)
        )

        val lRepeat = offset + 1
        val lAccum  = offset + program.length + 1
        val lFinish = offset + program.length + 4

        program ++ Array(
          VM.JumpCond(lFinish, lAccum),
          VM.FramePop,
          VM.Construct2((l, a) => l.asInstanceOf[Chunk[AnyRef]] + a),
          VM.Jump(lRepeat),
          VM.FrameLoad,
          VM.Pop
        )

      case Rep(_, _, _) =>
        ???
    }
  }

  private def interpret(input: Chunk[Input], codec: Array[CodecVM]): Either[DecodeError, (Int, Any)] = {
    import CodecVM._

    val stack: Stack[AnyRef] = Stack()
    var i: Int               = 0

    val inputStack: Stack[AnyRef] = Stack()
    var inputIndex: Int           = -1
    val inputLength: Int          = input.length

    var r0: AnyRef = null.asInstanceOf[AnyRef]

    while (i < codec.length) {
      val instr: CodecVM = codec(i)
      instr match {
        case Push(value) =>
          stack.push(value)
          i += 1

        case Read(None, None) =>
          inputIndex += 1
          if (inputIndex < inputLength) {
            stack.push(input(inputIndex).asInstanceOf[AnyRef])
            i += 1
          } else {
            return Left(DecodeError("EOI", inputIndex))
          }

        case Read(_, _) =>
          ???

        case CheckSet(s) =>
          val v1 = stack.pop()
          println(v1.toString + " in " + s) // todo
          stack.push(s.contains(v1).asInstanceOf[AnyRef])
          i += 1

        case Jump(to) =>
          i = to

        case JumpCond(ifEqual, otherwise) =>
          if (stack.pop().eq(stack.pop())) i = ifEqual else i = otherwise

        case Construct1(f) =>
          stack.push(f(stack.pop()))
          i += 1

        case Construct2(f) =>
          val arg2 = stack.pop()
          val arg1 = stack.pop()
          stack.push(f(arg1, arg2))
          i += 1

        case Fail(err) =>
          return Left(DecodeError(err, inputIndex))

        case Pop =>
          stack.pop()
          i += 1

        case Noop =>
          i += 1

        case Duplicate =>
          stack.push(stack.peek())
          i += 1

        case StoreRegister0 =>
          r0 = stack.pop()
          i += 1

        case LoadRegister0 =>
          stack.push(r0)
          i += 1

        case FramePush =>
          inputStack.push(inputIndex.asInstanceOf[AnyRef])
          i += 1

        case FramePop =>
          inputStack.pop()
          i += 1

        case FrameLoad =>
          inputIndex = inputStack.pop().asInstanceOf[Int]
          i += 1
      }

//      println(instr.toString.padTo(20, ' ') + ": " + stack.peekOrElse("EMPTY"))
    }

    stack.pop() match {
      case null => Left(DecodeError("bug in our implementation, please report to us", 0))
      case v    => Right((inputIndex, v))
    }
  }
}
