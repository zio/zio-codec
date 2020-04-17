package zio.codec

import zio.Chunk
import zio.internal.Stack

trait CharCodecModule extends CodecModule {
  type Input = Char

  // todo: consumeN
  def token(t: String): Codec[Chunk[Char]] =
    consume.repN(t.length).filter(Set(Chunk.fromIterable(t)))

  def tokenAs[A](t: String, v: A): Codec[A] =
    token(t).as(v, Chunk.fromIterable(t))

  def decoder[A](codec: Codec[A]): Chunk[Input] => Either[DecodeError, (Int, A)] = {
    val compiled = compileCodec(codec)
//    println(compiled.zipWithIndex.map { case (o, i) => i.toString.reverse.padTo(3, '0').reverse + ": " + o }.mkString("", "\n", "\n"))
    interpret(_, compiled).asInstanceOf[Either[DecodeError, (Int, A)]]
  }

  def encoder[A](codec: Codec[A]): A => Either[EncodeError, Chunk[Input]] = ???

  def printer[A](codec: Codec[A]): List[String] = ???

  private case object NoValue

  private def compileCodec[A](codec: Codec[A]): Array[CodecVM] = {
    import Codec.FilterMode._

    val VM = CodecVM

    // todo: keep only needed return statements

    var nameCount = 0

    def compile[A0](codec: Codec[A0], offset: Int, chain: Map[AnyRef, (Int, Int)]): Array[CodecVM] = {
      chain
        .get(codec)
        .fold[Array[CodecVM]] {
          val chain1 = chain + (codec -> ((nameCount, offset)))
          val name   = nameCount
          nameCount += 1

          codec match {
            case Codec.Produce(a) =>
              Array(
                VM.Push(a.asInstanceOf[AnyRef])
              )

            case Codec.Fail(error) =>
              Array(
                VM.Fail(error)
              )

            case Codec.Consume =>
              Array(
                VM.Read(None, None)
              )

            case map: Codec.Map[_, _] =>
              val program = compile(map.value(), offset, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(NoValue)
              )

              val mapProgram: Array[CodecVM] = map.equiv match {
                case Equiv.Left(_)      => Array(VM.Construct1(_.asInstanceOf[(AnyRef, _)]._1))
                case Equiv.Right(_)     => Array(VM.Construct1(_.asInstanceOf[(_, AnyRef)]._2))
                case Equiv.Merge()      => Array(VM.Construct1(_.asInstanceOf[Either[AnyRef, AnyRef]].merge))
                case Equiv.Ignore(_)    => Array(VM.Pop, VM.Push(().asInstanceOf[AnyRef]))
                case Equiv.As(b, _)     => Array(VM.Pop, VM.Push(b.asInstanceOf[AnyRef]))
                case Equiv.Chars.String => Array(VM.Construct1(_.asInstanceOf[Chunk[Char]].mkString))
                case Equiv.Bits.UInt16  => ???

                case Equiv.ForTesting(f) => Array(VM.Construct1(f.asInstanceOf[AnyRef => AnyRef]))
              }

              val lMap  = offset + program.length + 1
              val lBail = offset + program.length + 1 + mapProgram.length

              program ++ Array(
                VM.JumpEq(lBail, lMap)
              ) ++ mapProgram ++ Array(
                VM.Return(name)
              )

            case Codec.Filter(value, in, mod) =>
              val program = compile(value(), offset, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(NoValue)
              )

              val lFilter   = offset + program.length + 1
              val lMismatch = offset + program.length + 5
              val lMatch    = offset + program.length + 7
              val lExit     = offset + program.length + 7

              // format: off
              program ++ Array(
                VM.JumpEq(lExit, lFilter),

                VM.Duplicate,                    // filter
                VM.CheckSet(in.map(_.asInstanceOf[AnyRef])),
                VM.Push((mod match { case Inside => true; case Outside => false }).asInstanceOf[AnyRef]),
                VM.JumpEq(lMatch, lMismatch),

                VM.Pop,                          // mismatch
                VM.Push(NoValue),
                VM.Return(name)                  // match, exit
              )
              // format: on

            case Codec.Zip(left, right) =>
              val programL = compile(left(), offset, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(NoValue)
              )

              val programR = compile(right(), offset + programL.length + 1, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(NoValue)
              )

              val lCont1 = offset + programL.length + 1
              val lCont2 = offset + programL.length + 1 + programR.length + 1
              val lClean = offset + programL.length + 1 + programR.length + 3
              val lExit  = offset + programL.length + 1 + programR.length + 6

              // format: off
              programL ++
                Array(
                  VM.JumpEq(lExit, lCont1)
                ) ++ programR ++                 // continue to second codec
                Array(
                  VM.JumpEq(lClean, lCont2),

                  VM.Construct2(Tuple2.apply),   // continue to construct a tuple
                  VM.Jump(lExit),

                  VM.Pop,                        // clean up
                  VM.Pop,
                  VM.Push(NoValue),
                  VM.Return(name)                // exit
                )
              // format: on

            case Codec.Opt(value) =>
              val program = Array(
                VM.FramePush
              ) ++ compile(value(), offset + 1, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(NoValue)
              )

              val lSome = offset + program.length + 1
              val lNone = offset + program.length + 4
              val lExit = offset + program.length + 7

              // format: off
              program ++ Array(
                VM.JumpEq(lNone, lSome),

                VM.FramePop,                     // some
                VM.Construct1(Some.apply),
                VM.Jump(lExit),

                VM.FrameLoad,                    // none
                VM.Pop,
                VM.Push(None),
                VM.Return(name)                  // exit
              )
              // format: on

            case Codec.Alt(left, right) =>
              val programL = Array(
                VM.FramePush
              ) ++ compile(left(), offset + 1, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(NoValue)
              )

              val programR = compile(right(), offset + programL.length + 3, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(NoValue)
              )

              val lAlt     = offset + programL.length + 1
              val lAcceptL = offset + programL.length + 3 + programR.length + 3
              val lAcceptR = offset + programL.length + 3 + programR.length + 1
              val lExit    = offset + programL.length + 3 + programR.length + 5

              // format: off
              programL ++
                Array(
                  VM.JumpEq(lAlt, lAcceptL),
                  VM.FrameLoad,                  // try another program
                  VM.Pop
                ) ++ programR ++
                Array(
                  VM.JumpEq(lExit, lAcceptR),
                  VM.Construct1(Right.apply),    // accept right
                  VM.Jump(lExit),
                  VM.FramePop,                   // accept left
                  VM.Construct1(Left.apply),
                  VM.Return(name)                // exit
                )
              // format: on

            case Codec.Rep(value, None, None) =>
              val program = Array(
                VM.Push(Chunk.empty),
                VM.FramePush // repeat
              ) ++ compile(value(), offset + 2, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(NoValue)
              )

              val lRepeat = offset + 1
              val lAccum  = offset + program.length + 1
              val lFinish = offset + program.length + 4

              // format: off
              program ++ Array(
                VM.JumpEq(lFinish, lAccum),

                VM.FramePop,                     // accumulate
                VM.Construct2((l, a) => l.asInstanceOf[Chunk[AnyRef]] + a),
                VM.Jump(lRepeat),

                VM.FrameLoad,                    // finish
                VM.Pop,
                VM.Return(name)
              )
              // format: on

            // todo: remove guard! bug is Scala prevents exhaustive matching
            case Codec.Rep(value, Some(min), Some(max)) if min == max =>
              val program = Array(
                VM.Push(0.asInstanceOf[AnyRef]),
                VM.Push(Chunk.empty),
                VM.FramePush // repeat
              ) ++ compile(value(), offset + 3, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(NoValue)
              )

              val lRepeat  = offset + 2
              val lAccum   = offset + program.length + 1
              val lNoValue = offset + program.length + 14
              val lMore    = offset + program.length + 9
              val lEnough  = offset + program.length + 11
              val lExit    = offset + program.length + 19

              // format: off
              program ++ Array(
                VM.JumpEq(lNoValue, lAccum),

                VM.FramePop,                     // accumulate element
                VM.Construct2((l, a) => l.asInstanceOf[Chunk[AnyRef]] + a),
                VM.StoreRegister0,
                VM.Push(1.asInstanceOf[AnyRef]),
                VM.IAdd,
                VM.Duplicate,
                VM.Push(math.max(1, max).asInstanceOf[AnyRef]), // todo: it's always 1 or more, is it correct?
                VM.JumpEq(lEnough, lMore),

                VM.LoadRegister0,                // more
                VM.Jump(lRepeat),

                VM.Pop,                          // enough
                VM.LoadRegister0,
                VM.Jump(lExit),

                VM.FrameLoad,                    // no more elements
                VM.Pop,
                VM.Pop,
                VM.Pop,
                VM.Push(NoValue),
                VM.Return(name)                  // exit
              )
              // format: on

            case Codec.Rep(_, _, _) =>
              ???
          }
        } { case (name, address) => Array(VM.Invoke(name, address)) }
    }

    val ret = compile(codec, 0, Map.empty)
    ret
  }

  private def interpret(input: Chunk[Input], codec: Array[CodecVM]): Either[DecodeError, (Int, Any)] = {
    import CodecVM._

    val stack: Stack[AnyRef] = Stack()
    var i: Int               = 0

    val inputStack: Stack[AnyRef] = Stack()
    var inputIndex: Int           = -1
    val inputLength: Int          = input.length

    val frameStack: Stack[AnyRef] = Stack()

    var r0: AnyRef = null.asInstanceOf[AnyRef]

    while (i < codec.length) {
      val instr: CodecVM = codec(i)
      instr match {
        case Push(value) =>
          stack.push(value)
          i += 1

        case Read(None, None) =>
          inputIndex += 1
          if (inputIndex < inputLength) stack.push(input(inputIndex).asInstanceOf[AnyRef])
          else stack.push(NoValue)
          i += 1

        case Read(_, _) =>
          ???

        case CheckSet(s) =>
          val v1 = stack.pop()
//          println(s"($inputIndex) " + v1.toString + " in " + s)
          stack.push(s.contains(v1).asInstanceOf[AnyRef])
          i += 1

        case Jump(to) =>
          i = to

        case JumpEq(ifEqual, otherwise) =>
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

        case Invoke(name, address) =>
          frameStack.push((name, i))
          i = address

        case Return(name) =>
          val (n: Int, a: Int) = frameStack.peekOrElse((-1, -1)) // todo: should be optimized away
          if (name == n) {
            frameStack.pop(); i = a + 1
          } else i += 1

        case Pop =>
          stack.pop()
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

        case IAdd =>
          stack.push((stack.pop().asInstanceOf[Int] + stack.pop().asInstanceOf[Int]).asInstanceOf[AnyRef])
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
      case null    => Left(DecodeError("bug in our implementation, please report to us", 0))
      case NoValue => Left(DecodeError("no match", inputIndex))
      case v       => Right((inputIndex, v))
    }
  }
}
