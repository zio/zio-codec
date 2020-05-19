package zio.codec

import zio.Chunk
import zio.internal.Stack

trait CharCodecModule extends CodecModule {
  type Input = Char

  // todo: consumeN
  def token(t: String): Codec[Chunk[Input]] =
    consume.repN(t.length).filter(Set(Chunk.fromIterable(t)))

  def tokenAs[A](t: String, v: A): Codec[A] =
    token(t).as(v, Chunk.fromIterable(t))

  def charBetween(begin: Input, end: Input): Codec[Input] =
    consume.filter((begin to end).toSet)

  def decoder[A](codec: Codec[A]): Chunk[Input] => Either[DecodeError, (Int, A)] = {
    val (program, labels) = compileCodec(codec)
//    println(program.zipWithIndex.map { case (o, i) => i.toString.reverse.padTo(3, '0').reverse + ": " + o }.mkString("", "\n", "\n"))
    interpret(_, labels, program).asInstanceOf[Either[DecodeError, (Int, A)]]
  }

  def encoder[A](codec: Codec[A]): A => Either[EncodeError, Chunk[Input]] = ???

  def printer[A](codec: Codec[A]): List[String] = ???

  private case object NoValue

  private def compileCodec[A](codec: Codec[A]): (Array[CodecVM], Map[Int, Int]) = {
    import Codec.FilterMode._

    val VM = CodecVM

    var methodCount: Int  = 0
    var invoked: Set[Int] = Set.empty

    var labelCount: Int       = 0
    var labels: Map[Int, Int] = Map.empty
    def newLabel(address: Int): VM.ALabel = {
      val number = labelCount
      labels += (labelCount -> address)
      labelCount += 1
      VM.ALabel(number, address)
    }

    def compile[A0](codec: Codec[A0], offset: Int, chain: Map[AnyRef, (Int, Int)]): Array[CodecVM] = {
      chain
        .get(codec)
        .fold[Array[CodecVM]] {
          val chain1 = chain + (codec -> ((methodCount, offset)))
          val name   = methodCount

          methodCount += 1

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
              val program = Array(
                VM.BeginMethod(name)
              ) ++ compile(map.value(), offset + 1, chain1) ++ Array(
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

              val lBail = newLabel(offset + program.length + 1 + mapProgram.length)

              program ++ Array(
                VM.ACmpEq(lBail)
              ) ++ mapProgram ++ Array(
                VM.Return(name)
              )

            case Codec.Filter(value, in, mod) =>
              val program = Array(
                VM.BeginMethod(name)
              ) ++ compile(value(), offset + 1, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(NoValue)
              )

              val lMatch = newLabel(offset + program.length + 7)
              val lExit  = newLabel(offset + program.length + 7)

              // format: off
              program ++ Array(
                VM.ACmpEq(lExit),

                VM.Duplicate,                    // filter
                VM.CheckSet(in.asInstanceOf[Set[Any]]),
                VM.Push((mod match { case Inside => 1; case Outside => 0 }).asInstanceOf[AnyRef]),
                VM.ICmpEq(lMatch),

                VM.Pop,                          // mismatch
                VM.Push(NoValue),
                VM.Return(name)                  // match, exit
              )
              // format: on

            case Codec.Zip(left, right) =>
              val programL = Array(
                VM.BeginMethod(name)
              ) ++ compile(left(), offset + 1, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(NoValue)
              )

              val programR = compile(right(), offset + programL.length + 1, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(NoValue)
              )

              val lClean = newLabel(offset + programL.length + 1 + programR.length + 3)
              val lExit  = newLabel(offset + programL.length + 1 + programR.length + 6)

              // format: off
              programL ++
                Array(
                  VM.ACmpEq(lExit)
                ) ++ programR ++                 // continue to second codec
                Array(
                  VM.ACmpEq(lClean),

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
                VM.BeginMethod(name),
                VM.IIndexPush
              ) ++ compile(value(), offset + 2, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(NoValue)
              )

              val lNone = newLabel(offset + program.length + 4)
              val lExit = newLabel(offset + program.length + 7)

              // format: off
              program ++ Array(
                VM.ACmpEq(lNone),

                VM.IIndexPop,                    // some
                VM.Construct1(Some.apply),
                VM.Jump(lExit),

                VM.IIndexStore,                  // none
                VM.Pop,
                VM.Push(None),
                VM.Return(name)                  // exit
              )
              // format: on

            case Codec.Alt(left, right) =>
              val programL = Array(
                VM.BeginMethod(name),
                VM.IIndexPush
              ) ++ compile(left(), offset + 2, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(NoValue)
              )

              val programR = compile(right(), offset + programL.length + 3, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(NoValue)
              )

              val lAcceptL = newLabel(offset + programL.length + 3 + programR.length + 3)
              val lExit    = newLabel(offset + programL.length + 3 + programR.length + 5)

              // format: off
              programL ++
                Array(
                  VM.ACmpNe(lAcceptL),
                  VM.IIndexStore,                // try another program
                  VM.Pop
                ) ++ programR ++
                Array(
                  VM.ACmpEq(lExit),
                  VM.Construct1(Right.apply),    // accept right
                  VM.Jump(lExit),
                  VM.IIndexPop,                  // accept left
                  VM.Construct1(Left.apply),
                  VM.Return(name)                // exit
                )
              // format: on

            case Codec.Rep(value, None, None) =>
              val program = Array(
                VM.BeginMethod(name),
                VM.Push(Chunk.empty),
                VM.IIndexPush // repeat
              ) ++ compile(value(), offset + 3, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(NoValue)
              )

              val lRepeat = newLabel(offset + 2)
              val lFinish = newLabel(offset + program.length + 4)

              // format: off
              program ++ Array(
                VM.ACmpEq(lFinish),

                VM.IIndexPop,                    // accumulate
                VM.Construct2((l, a) => l.asInstanceOf[Chunk[AnyRef]] + a),
                VM.Jump(lRepeat),

                VM.IIndexStore,                  // finish
                VM.Pop,
                VM.Return(name)
              )
              // format: on

            // todo: remove guard! bug in Scala prevents exhaustive matching
            case Codec.Rep(value, Some(min), Some(max)) if min == max =>
              val program = Array(
                VM.BeginMethod(name),
                VM.Push(0.asInstanceOf[AnyRef]),
                VM.Push(Chunk.empty),
                VM.IIndexPush // repeat
              ) ++ compile(value(), offset + 4, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(NoValue)
              )

              val lRepeat  = newLabel(offset + 3)
              val lNoValue = newLabel(offset + program.length + 14)
              val lEnough  = newLabel(offset + program.length + 11)
              val lExit    = newLabel(offset + program.length + 19)

              // format: off
              program ++ Array(
                VM.ACmpEq(lNoValue),

                VM.IIndexPop,                    // accumulate element
                VM.Construct2((l, a) => l.asInstanceOf[Chunk[AnyRef]] + a),
                VM.StoreRegister0,
                VM.Push(1.asInstanceOf[AnyRef]),
                VM.IAdd,
                VM.Duplicate,
                VM.Push(math.max(1, max).asInstanceOf[AnyRef]), // todo: it's always 1 or more, is it correct?
                VM.ICmpEq(lEnough),

                VM.LoadRegister0,                // more
                VM.Jump(lRepeat),

                VM.Pop,                          // enough
                VM.LoadRegister0,
                VM.Jump(lExit),

                VM.IIndexStore,                  // no more elements
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
        } {
          case (name, address) =>
            invoked += name
            Array(VM.InvokeStatic(name, address + 1))
        }
    }

    val ret0 = compile(codec, 0, Map.empty)

    // optimization: inline methods
    val ret1 = ret0.map {
      case i @ VM.BeginMethod(name) => if (invoked.contains(name)) i else VM.Noop
      case i @ VM.Return(name)      => if (invoked.contains(name)) i else VM.Noop
      case i                        => i
    }

    (ret1, labels)
  }

  private def interpret(
    input: Chunk[Input],
    labels: Map[Int, Int],
    program: Array[CodecVM]
  ): Either[DecodeError, (Int, Any)] = {
    import CodecVM._

    val stack: Stack[AnyRef] = Stack()
    var i: Int               = 0

    val inputStack: Stack[AnyRef] = Stack()
    var inputIndex: Int           = -1
    val inputLength: Int          = input.length

    val frameStack: Stack[AnyRef] = Stack()

    var r0: AnyRef = null.asInstanceOf[AnyRef]

    while (i < program.length) {
      val instr: CodecVM = program(i)
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
          stack.push((if (s.contains(v1)) 1 else 0).asInstanceOf[AnyRef])
          i += 1

        case Jump(label) =>
          i = labels(label.num)

        case ICmpEq(label) =>
          if (stack.pop().asInstanceOf[Int] == stack.pop().asInstanceOf[Int]) i = labels(label.num) else i += 1

        case ACmpEq(label) =>
          if (stack.pop().eq(stack.pop())) i = labels(label.num) else i += 1

        case ACmpNe(label) =>
          if (stack.pop().eq(stack.pop())) i += 1 else i = labels(label.num)

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

        case BeginMethod(_) =>
          frameStack.push((-1).asInstanceOf[AnyRef])
          i += 1

        case Return(_) =>
          val address = frameStack.pop().asInstanceOf[Int]
          if (address == -1) i += 1
          else i = address + 1

        case InvokeStatic(_, address) =>
          frameStack.push(i.asInstanceOf[AnyRef])
          i = address

        case Noop =>
          i += 1

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

        case IIndexPush =>
          inputStack.push(inputIndex.asInstanceOf[AnyRef])
          i += 1

        case IIndexPop =>
          inputStack.pop()
          i += 1

        case IIndexStore =>
          inputIndex = inputStack.pop().asInstanceOf[Int]
          i += 1
      }

//      println(instr.toString.padTo(20, ' ') + ": " + stack.peekOrElse("EMPTY"))
    }

    stack.pop() match {
      case null    => Left(DecodeError("bug in our implementation, please report to us", 0))
      case NoValue => Left(DecodeError("no match", inputIndex + 1))
      case v       => Right((inputIndex + 1, v))
    }
  }
}
