package zio.codec

import java.lang.reflect.Method
import java.util.UUID

import org.objectweb.asm.{ ClassWriter, Label, MethodVisitor, Opcodes }
import zio.Chunk
import zio.internal.Stack

import scala.runtime.BoxedUnit

trait CharCodecModule extends CodecModule {
  type Input = Char

  // todo: consumeN
  def token(t: String): Codec[Chunk[Input]] =
    consume.repN(t.length).filter(Set(Chunk.fromIterable(t)))

  def tokenAs[A](t: String, v: A): Codec[A] =
    token(t).as(v, Chunk.fromIterable(t))

  def charBetween(begin: Input, end: Input): Codec[Input] =
    consume.filter((begin to end).toSet)

  def interpreterDecoder[A](codec: Codec[A]): Chunk[Input] => Either[DecodeError, (Int, A)] = {
    val CodecCompilationResult(program, _, _, _) = compileCodec(codec)
//    println(program.zipWithIndex.map { case (o, i) => i.toString.reverse.padTo(3, '0').reverse + ": " + o }.mkString("", "\n", "\n"))
    interpret(_, program).asInstanceOf[Either[DecodeError, (Int, A)]]
  }

  def encoder[A](codec: Codec[A]): A => Either[EncodeError, Chunk[Input]] = ???

  def printer[A](codec: Codec[A]): List[String] = ???

  def decoder[A](codec: Codec[A]): Chunk[Input] => Either[DecodeError, (Int, A)] = {
    val CodecCompilationResult(program, labels, values, lookups) = compileCodec(codec)
//    println(program.zipWithIndex.map { case (o, i) => i.toString.reverse.padTo(3, '0').reverse + ": " + o }.mkString("", "\n", "\n"))

    val (name, bytes) = compileCodecVM(program, labels)
    val method        = new ByteClassLoader().load(name, bytes)

//    println(bytes.length)
//    println(method)
//    println()

    val state: Array[Int] = Array(-1)
    s => {
      method.invoke(null, NoValue +: values, lookups, s.toArray, state) match {
        case null    => Left(DecodeError("bug in our implementation, please report to us", 0))
        case NoValue => Left(DecodeError("no match", state(0) + 1))
        case v       => Right((state(0) + 1, v.asInstanceOf[A]))
      }
    }
  }

  private case object NoValue

  private case class CodecCompilationResult(
    program: Array[CodecVM],
    addresses: Map[Int, Int],
    values: Array[AnyRef],
    lookups: Array[java.util.HashSet[Any]]
  )

  private def compileCodec[A](codec: Codec[A]): CodecCompilationResult = {
    import Codec.FilterMode._

    val VM = CodecVM

    var methodCount: Int     = 0
    var invoked: Set[String] = Set.empty

    var labelCount: Int       = 0
    var labels: Map[Int, Int] = Map.empty
    def newLabel(address: Int): VM.ALabel = {
      val idx = labelCount
      labels += (labelCount -> address)
      labelCount += 1
      VM.ALabel(idx, address)
    }

    var setCount: Int                       = 0
    var sets: Array[java.util.HashSet[Any]] = Array()
    def newSet[A0](filter: Codec.Filter[A0]): VM.ASet = {
      val idx = setCount
      sets = sets :+ filter.hs
      setCount += 1
      VM.ASet(idx, filter.filter.asInstanceOf[Set[Any]])
    }

    val v1                    = VM.AValue(1, BoxedUnit.UNIT)
    val v2                    = VM.AValue(2, None)
    val v3                    = VM.AValue(3, Chunk.empty)
    var valueCount: Int       = 4
    var values: Array[AnyRef] = Array(BoxedUnit.UNIT, None, Chunk.empty)
    def newValue(value: AnyRef): VM.AValue =
      value match {
        case BoxedUnit.UNIT => v1
        case None           => v2
        case Chunk.empty    => v3
        case v =>
          val idx = valueCount
          values = values :+ v
          valueCount += 1
          VM.AValue(idx, v)
      }

    def compile[A0](codec: Codec[A0], offset: Int, chain: Map[AnyRef, (String, Int)]): Array[CodecVM] = {
      chain
        .get(codec)
        .fold[Array[CodecVM]] {
          val name   = "m" + methodCount
          val chain1 = chain + (codec -> ((name, offset)))

          methodCount += 1

          codec match {
            case Codec.Produce(a) =>
              Array(
                VM.Push(newValue(a.asInstanceOf[AnyRef]))
              )

            case Codec.Fail(error) =>
              Array(
                VM.Fail(error)
              )

            case Codec.Consume =>
              Array(
                VM.InputRead(None, None)
              )

            case map: Codec.Map[_, _] =>
              val program = Array(
                VM.BeginMethod(name)
              ) ++ compile(map.value(), offset + 1, chain1) ++ Array(
                VM.Duplicate,
                VM.PushNoValue
              )

              val mapProgram: Array[CodecVM] = map.equiv match {
                case Equiv.Left(_) =>
                  Array(
                    VM.CheckCast("scala/Tuple2"),
                    VM.InvokeVirtual0("scala/Tuple2", "_1", "()Ljava/lang/Object;", _.asInstanceOf[(AnyRef, _)]._1)
                  )

                case Equiv.Right(_) =>
                  Array(
                    VM.CheckCast("scala/Tuple2"),
                    VM.InvokeVirtual0("scala/Tuple2", "_2", "()Ljava/lang/Object;", _.asInstanceOf[(_, AnyRef)]._2)
                  )

                case Equiv.Merge() =>
                  Array(
                    VM.GetStatic("zio/codec/Equiv$", "MODULE$", "Lzio/codec/Equiv$;", Equiv),
                    VM.Swap,
                    VM.CheckCast("scala/util/Either"),
                    VM.InvokeVirtual1(
                      "zio/codec/Equiv$",
                      "merge",
                      "(Lscala/util/Either;)Ljava/lang/Object;",
                      (equiv, v) => equiv.asInstanceOf[Equiv.type].merge(v.asInstanceOf[Either[AnyRef, AnyRef]])
                    )
                  )

                case Equiv.Ignore(_) =>
                  Array(
                    VM.Pop,
                    VM.Push(newValue(BoxedUnit.UNIT))
                  )

                case Equiv.As(b, _) =>
                  Array(
                    VM.Pop,
                    VM.Push(newValue(b.asInstanceOf[AnyRef]))
                  )

                case Equiv.Chars.String =>
                  Array(
                    VM.CheckCast("zio/Chunk"),
                    VM.InvokeInterface0(
                      "zio/Chunk",
                      "mkString",
                      "()Ljava/lang/String;",
                      _.asInstanceOf[Chunk[Char]].mkString
                    )
                  )

                case Equiv.Bits.UInt16 => ???

                case Equiv.ForTesting(f) =>
                  Array(
                    VM.Push(newValue(f)),
                    VM.CheckCast("scala/Function1"),
                    VM.Swap,
                    VM.InvokeInterface1(
                      "scala/Function1",
                      "apply",
                      "(Ljava/lang/Object;)Ljava/lang/Object;",
                      (f, a) => f.asInstanceOf[AnyRef => AnyRef](a)
                    )
                  )
              }

              val lBail = newLabel(offset + program.length + 1 + mapProgram.length)

              program ++ Array(
                VM.ACmpEq(lBail)
              ) ++ mapProgram ++ Array(
                VM.EndMethod(name)
              )

            case filter @ Codec.Filter(value, _, mod) =>
              val program = Array(
                VM.BeginMethod(name)
              ) ++ compile(value(), offset + 1, chain1) ++ Array(
                VM.Duplicate,
                VM.PushNoValue
              )

              val lMatch = newLabel(offset + program.length + 7)
              val lExit  = newLabel(offset + program.length + 7)

              // format: off
              program ++ Array(
                VM.ACmpEq(lExit),

                VM.Duplicate,                    // filter
                VM.CheckSet(newSet(filter)),
                VM.PushInt(mod match { case Inside => 1; case Outside => 0 }),
                VM.ICmpEq(lMatch),

                VM.Pop,                          // mismatch
                VM.PushNoValue,
                VM.EndMethod(name)               // match, exit
              )
              // format: on

            case Codec.Zip(left, right) =>
              val programL = Array(
                VM.BeginMethod(name),
                VM.New(VM.ANew(UUID.randomUUID(), "scala/Tuple2")),
                VM.Duplicate
              ) ++ compile(left(), offset + 3, chain1) ++ Array(
                VM.Duplicate,
                VM.PushNoValue
              )

              val programR = compile(right(), offset + programL.length + 1, chain1) ++ Array(
                VM.Duplicate,
                VM.PushNoValue
              )

              val lCleanA = newLabel(offset + programL.length + 1 + programR.length + 3)
              val lCleanL = newLabel(offset + programL.length + 1 + programR.length + 4)
              val lExit   = newLabel(offset + programL.length + 1 + programR.length + 8)

              // format: off
              programL ++
                Array(
                  VM.ACmpEq(lCleanL)
                ) ++ programR ++                 // continue to second codec
                Array(
                  VM.ACmpEq(lCleanA),
                                                 // continue to construct a tuple
                  VM.InvokeSpecial2("scala/Tuple2", "<init>", "(Ljava/lang/Object;Ljava/lang/Object;)V", Tuple2.apply),
                  VM.Jump(lExit),

                  VM.Pop,                        // clean up all
                  VM.Pop,                        // clean up left
                  VM.Pop,
                  VM.Pop,
                  VM.PushNoValue,
                  VM.EndMethod(name)             // exit
                )
              // format: on

            case Codec.Opt(value) =>
              val program = Array(
                VM.BeginMethod(name),
                VM.InputIdxLoad,
                VM.New(VM.ANew(UUID.randomUUID(), "scala/Some")),
                VM.Duplicate
              ) ++ compile(value(), offset + 4, chain1) ++ Array(
                VM.Duplicate,
                VM.PushNoValue
              )

              val lNone = newLabel(offset + program.length + 5)
              val lExit = newLabel(offset + program.length + 10)

              // format: off
              program ++ Array(
                VM.ACmpEq(lNone),
                                                 // some
                VM.InvokeSpecial1("scala/Some", "<init>", "(Ljava/lang/Object;)V", Some.apply),
                VM.Swap,
                VM.InputIdxPop,
                VM.Jump(lExit),

                VM.Pop,                          // none
                VM.Pop,
                VM.Pop,
                VM.InputIdxStore,
                VM.Push(newValue(None)),
                VM.EndMethod(name)               // exit
              )
              // format: on

            case Codec.Alt(left, right) =>
              val programL = Array(
                VM.BeginMethod(name),
                VM.InputIdxLoad
              ) ++ compile(left(), offset + 2, chain1) ++ Array(
                VM.Duplicate,
                VM.PushNoValue
              )

              val programR = compile(right(), offset + programL.length + 3, chain1) ++ Array(
                VM.Duplicate,
                VM.PushNoValue
              )

              val lAcceptL = newLabel(offset + programL.length + 3 + programR.length + 7)
              val lExit    = newLabel(offset + programL.length + 3 + programR.length + 13)

              // format: off
              programL ++
                Array(
                  VM.ACmpNe(lAcceptL),
                  VM.Pop,                        // try another program
                  VM.InputIdxStore
                ) ++ programR ++
                Array(
                  VM.ACmpEq(lExit),

                  VM.StoreRegister0,             // accept right
                  VM.New(VM.ANew(UUID.randomUUID(), "scala/util/Right")),
                  VM.Duplicate,
                  VM.LoadRegister0,
                  VM.InvokeSpecial1("scala/util/Right", "<init>", "(Ljava/lang/Object;)V", scala.util.Right.apply),
                  VM.Jump(lExit),

                  VM.StoreRegister0,             // accept left
                  VM.InputIdxPop,
                  VM.New(VM.ANew(UUID.randomUUID(), "scala/util/Left")),
                  VM.Duplicate,
                  VM.LoadRegister0,
                  VM.InvokeSpecial1("scala/util/Left", "<init>", "(Ljava/lang/Object;)V", scala.util.Left.apply),
                  VM.EndMethod(name)             // exit
                )
              // format: on

            case Codec.Rep(value, None, None) =>
              val program = Array(
                VM.BeginMethod(name),
                VM.Push(newValue(Chunk.empty)),
                VM.InputIdxLoad // repeat
              ) ++ compile(value(), offset + 3, chain1) ++ Array(
                VM.Duplicate,
                VM.PushNoValue
              )

              val lRepeat = newLabel(offset + 2)
              val lFinish = newLabel(offset + program.length + 5)

              // format: off
              program ++ Array(
                VM.ACmpEq(lFinish),

                VM.Swap,                         // accumulate
                VM.InputIdxPop,
                VM.InvokeInterface1("zio/Chunk", "$plus", "(Ljava/lang/Object;)Lzio/Chunk;", (l, a) => l.asInstanceOf[Chunk[AnyRef]] + a),
                VM.Jump(lRepeat),

                VM.Pop,                          // finish
                VM.InputIdxStore,
                VM.EndMethod(name)
              )
              // format: on

            // todo: remove guard! bug in Scala prevents exhaustive matching
            case Codec.Rep(value, Some(min), Some(max)) if min == max =>
              val program = Array(
                VM.BeginMethod(name),
                VM.PushInt(0),
                VM.Push(newValue(Chunk.empty)),
                VM.InputIdxLoad // repeat
              ) ++ compile(value(), offset + 4, chain1) ++ Array(
                VM.Duplicate,
                VM.PushNoValue
              )

              val lRepeat  = newLabel(offset + 3)
              val lNoValue = newLabel(offset + program.length + 15)
              val lEnough  = newLabel(offset + program.length + 12)
              val lExit    = newLabel(offset + program.length + 20)

              // format: off
              program ++ Array(
                VM.ACmpEq(lNoValue),

                VM.Swap,                         // accumulate element
                VM.InputIdxPop,
                VM.InvokeInterface1("zio/Chunk", "$plus", "(Ljava/lang/Object;)Lzio/Chunk;", (l, a) => l.asInstanceOf[Chunk[AnyRef]] + a),
                VM.StoreRegister0,
                VM.PushInt(1),
                VM.IAdd,
                VM.Duplicate,
                VM.PushInt(math.max(1, max)), // todo: it's always 1 or more, is it correct?
                VM.ICmpEq(lEnough),

                VM.LoadRegister0,                // more
                VM.Jump(lRepeat),

                VM.Pop,                          // enough
                VM.LoadRegister0,
                VM.Jump(lExit),

                VM.Pop,                          // no more elements
                VM.InputIdxStore,
                VM.Pop,
                VM.Pop,
                VM.PushNoValue,
                VM.EndMethod(name)               // exit
              )
              // format: on

            case Codec.Rep(_, _, _) =>
              ???
          }
        } {
          case (name, address) =>
            invoked += name
            Array(VM.CallMethod(name, address + 1))
        }
    }

    val ret0 = compile(codec, 0, Map.empty)

    // optimization: inline methods
    val ret1 = ret0.map {
      case i @ VM.BeginMethod(name) => if (invoked.contains(name)) i else VM.Noop
      case i @ VM.EndMethod(name)   => if (invoked.contains(name)) i else VM.Noop
      case i                        => i
    }

    CodecCompilationResult(ret1, labels, values, sets)
  }

  private def interpret(input: Chunk[Input], program: Array[CodecVM]): Either[DecodeError, (Int, Any)] = {
    import CodecVM._

    val stack: Stack[AnyRef] = Stack()
    var i: Int               = 0

    var createdInstances: Map[UUID, AnyRef] = Map.empty

    def stackPopResolve(): AnyRef =
      stack.pop() match {
        case ANew(id, _) => createdInstances(id)
        case value       => value
      }

    var inputIndex: Int  = -1
    val inputLength: Int = input.length

    val callStack: Stack[AnyRef] = Stack()

    var r0: AnyRef = null.asInstanceOf[AnyRef]

    while (i < program.length) {
      val instr: CodecVM = program(i)
      instr match {
        case BeginMethod(_) =>
          callStack.push((-1).asInstanceOf[AnyRef])
          i += 1

        case EndMethod(_) =>
          val address = callStack.pop().asInstanceOf[Int]
          if (address == -1) i += 1
          else i = address + 1

        case CallMethod(_, address) =>
          callStack.push(i.asInstanceOf[AnyRef])
          i = address

        case InputRead(None, None) =>
          inputIndex += 1
          if (inputIndex < inputLength) stack.push(input(inputIndex).asInstanceOf[AnyRef])
          else stack.push(NoValue)
          i += 1

        case InputRead(_, _) =>
          ???

        case InputIdxLoad =>
          stack.push(inputIndex.asInstanceOf[AnyRef])
          i += 1

        case InputIdxPop =>
          stack.pop()
          i += 1

        case InputIdxStore =>
          inputIndex = stack.pop().asInstanceOf[Int]
          i += 1

        case Push(AValue(_, value)) =>
          stack.push(value)
          i += 1

        case PushInt(int) =>
          stack.push(int.asInstanceOf[AnyRef])
          i += 1

        case PushNoValue =>
          stack.push(NoValue)
          i += 1

        case CheckSet(s) =>
          val v1 = stackPopResolve()
//          println(s"($inputIndex) " + v1.toString + " in " + s)
          stack.push((if (s.set.contains(v1)) 1 else 0).asInstanceOf[AnyRef])
          i += 1

        case Jump(ALabel(_, address)) =>
          i = address

        case ICmpEq(ALabel(_, address)) =>
          if (stack.pop().asInstanceOf[Int] == stack.pop().asInstanceOf[Int]) i = address else i += 1

        case ACmpEq(ALabel(_, address)) =>
          if (stackPopResolve().eq(stackPopResolve())) i = address else i += 1

        case ACmpNe(ALabel(_, address)) =>
          if (stackPopResolve().eq(stackPopResolve())) i += 1 else i = address

        case Fail(err) =>
          return Left(DecodeError(err, inputIndex))

        case New(ins @ ANew(id, _)) =>
          createdInstances += (id -> null.asInstanceOf[AnyRef])
          stack.push(ins)
          i += 1

        case CheckCast(_) =>
          i += 1

        case GetStatic(_, _, _, v) =>
          stack.push(v)
          i += 1

        case InvokeSpecial1(_, _, _, f) =>
          val arg1        = stackPopResolve()
          val ANew(id, _) = stack.pop()
          createdInstances += (id -> f(arg1))
          i += 1

        case InvokeSpecial2(_, _, _, f) =>
          val arg2        = stackPopResolve()
          val arg1        = stackPopResolve()
          val ANew(id, _) = stack.pop()
          createdInstances += (id -> f(arg1, arg2))
          i += 1

        case InvokeVirtual0(_, _, _, f) =>
          stack.push(f(stackPopResolve()))
          i += 1

        case InvokeVirtual1(_, _, _, f) =>
          val arg2 = stackPopResolve()
          val arg1 = stackPopResolve()
          stack.push(f(arg1, arg2))
          i += 1

        case InvokeInterface0(_, _, _, f) =>
          stack.push(f(stackPopResolve()))
          i += 1

        case InvokeInterface1(_, _, _, f) =>
          val arg2 = stackPopResolve()
          val arg1 = stackPopResolve()
          stack.push(f(arg1, arg2))
          i += 1

        case Noop =>
          i += 1

        case Pop =>
          stack.pop()
          i += 1

        case Duplicate =>
          stack.push(stack.peek())
          i += 1

        case Swap =>
          val arg2 = stack.pop()
          val arg1 = stack.pop()
          stack.push(arg2)
          stack.push(arg1)
          i += 1

        case StoreRegister0 =>
          r0 = stackPopResolve()
          i += 1

        case LoadRegister0 =>
          stack.push(r0)
          i += 1

        case IAdd =>
          stack.push((stack.pop().asInstanceOf[Int] + stack.pop().asInstanceOf[Int]).asInstanceOf[AnyRef])
          i += 1
      }

//      println(instr.toString.padTo(20, ' ') + ": " + stack.peekOrElse("EMPTY"))
    }

    stackPopResolve() match {
      case null    => Left(DecodeError("bug in our implementation, please report to us", 0))
      case NoValue => Left(DecodeError("no match", inputIndex + 1))
      case v       => Right((inputIndex + 1, v))
    }
  }

  private class ByteClassLoader extends ClassLoader {
    def load(name: String, bytes: Array[Byte]): Method = {
      val c = defineClass(name, bytes, 0, bytes.length)
      val m = c.getMethods.find(_.getName == "run").get
      m
    }
  }

  private def compileCodecVM(instructions: Array[CodecVM], addresses: Map[Int, Int]): (String, Array[Byte]) = {
    import CodecVM._
    import Opcodes._

    val labels: Map[Int, (Label, Int)] =
      addresses.map { case (key, address) => (key, (new Label(), address)) }
    val labelAddresses: Map[Int, List[Label]] =
      labels.groupBy(_._2._2).map { case (address, list) => (address, list.toList.map(_._2._1)) }

    def m_pushInt(m: MethodVisitor, i: Int): Unit =
      i match {
        case -1                             => m.visitInsn(ICONST_M1)
        case 0                              => m.visitInsn(ICONST_0)
        case 1                              => m.visitInsn(ICONST_1)
        case 2                              => m.visitInsn(ICONST_2)
        case 3                              => m.visitInsn(ICONST_3)
        case b if -128 <= b && b <= 127     => m.visitIntInsn(BIPUSH, b)
        case s if -32768 <= s && s <= 32767 => m.visitIntInsn(SIPUSH, s)
        case i                              => m.visitLdcInsn(i)
      }

    def m_pushNoValue(m: MethodVisitor): Unit = {
      m.visitVarInsn(ALOAD, 0)
      m.visitInsn(ICONST_0)
      m.visitInsn(AALOAD)
    }

    def m_callMethod(m: MethodVisitor, owner: String, name: String): Unit = {
      m.visitVarInsn(ALOAD, 0)
      m.visitVarInsn(ALOAD, 1)
      m.visitVarInsn(ALOAD, 2)
      m.visitVarInsn(ALOAD, 3)
      m.visitVarInsn(ILOAD, 4)

      m.visitMethodInsn(
        INVOKESTATIC,
        owner,
        name,
        "([Ljava/lang/Object;[Ljava/util/HashSet;[C[II)Ljava/lang/Object;",
        false
      )

      // continue from input index returned from the method
      m.visitVarInsn(ALOAD, 3)
      m.visitInsn(ICONST_0)
      m.visitInsn(IALOAD)
      m.visitVarInsn(ISTORE, 4)
    }

    val c: ClassWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES)
    val owner: String  = "Decoder" + UUID.randomUUID().toString.replace("-", "")

    c.visit(V1_8, ACC_PUBLIC | ACC_SUPER, owner, null, "java/lang/Object", null)

    val main: MethodVisitor =
      c.visitMethod(
        ACC_PUBLIC | ACC_STATIC,
        "run",
        "([Ljava/lang/Object;[Ljava/util/HashSet;[C[I)Ljava/lang/Object;",
        null,
        null
      )
    main.visitCode()

    // var0: Array[Object]  - external values
    // var1: Array[HashSet] - inclusion sets
    // var2: Array[char]    - input
    // var3: Array[int]     - size = 1, local state with input index
    // var4: int            - input index

    main.visitInsn(ICONST_M1)
    main.visitVarInsn(ISTORE, 4)

    def compile(m: MethodVisitor, offset: Int): Int = {
      var address: Int      = offset
      var finished: Boolean = false

      while (!finished && address < instructions.length) {
        val ins = instructions(address)

        labelAddresses
          .get(address)
          .foreach(_.foreach(m.visitLabel))

        address += 1

        ins match {
          case BeginMethod(name) =>
            m_callMethod(m, owner, name)

            val m2: MethodVisitor = c.visitMethod(
              ACC_PUBLIC | ACC_STATIC,
              name,
              "([Ljava/lang/Object;[Ljava/util/HashSet;[C[II)Ljava/lang/Object;",
              null,
              null
            )
            m2.visitCode()

            address = compile(m2, address)

            m2.visitMaxs(0, 0)
            m2.visitEnd()

          case EndMethod(_) =>
            finished = true

            // store input index to be returned from method
            m.visitVarInsn(ALOAD, 3)
            m.visitInsn(ICONST_0)
            m.visitVarInsn(ILOAD, 4)
            m.visitInsn(IASTORE)
            m.visitInsn(ARETURN)

          case CallMethod(name, _) =>
            m_callMethod(m, owner, name)

          case InputRead(None, None) =>
            val labelNoInput = new Label()
            val labelDone    = new Label()

            m.visitIincInsn(4, 1)

            m.visitVarInsn(ILOAD, 4)
            m.visitVarInsn(ALOAD, 2)
            m.visitInsn(ARRAYLENGTH)
            m.visitJumpInsn(IF_ICMPGE, labelNoInput)

            m.visitVarInsn(ALOAD, 2)
            m.visitVarInsn(ILOAD, 4)
            m.visitInsn(CALOAD)
            m.visitMethodInsn(INVOKESTATIC, "java/lang/Character", "valueOf", "(C)Ljava/lang/Character;", false)
            m.visitJumpInsn(GOTO, labelDone)

            m.visitLabel(labelNoInput)
            m_pushNoValue(m)

            m.visitLabel(labelDone)

          case InputRead(_, _) =>
            ???

          case InputIdxLoad =>
            m.visitVarInsn(ILOAD, 4)

          case InputIdxPop =>
            m.visitInsn(POP)

          case InputIdxStore =>
            m.visitVarInsn(ISTORE, 4)

          case Push(AValue(idx, _)) =>
            m.visitVarInsn(ALOAD, 0)
            m_pushInt(m, idx)
            m.visitInsn(AALOAD)

          case PushInt(int) =>
            m_pushInt(m, int)

          case PushNoValue =>
            m_pushNoValue(m)

          case CheckSet(ASet(idx, _)) =>
            m.visitVarInsn(ALOAD, 1)
            m_pushInt(m, idx)
            m.visitInsn(AALOAD)

            m.visitInsn(SWAP)
            m.visitMethodInsn(INVOKEVIRTUAL, "java/util/HashSet", "contains", "(Ljava/lang/Object;)Z", false)

          case Jump(ALabel(idx, _)) =>
            m.visitJumpInsn(GOTO, labels(idx)._1)

          case ICmpEq(ALabel(idx, _)) =>
            m.visitJumpInsn(IF_ICMPEQ, labels(idx)._1)

          case ACmpEq(ALabel(idx, _)) =>
            m.visitJumpInsn(IF_ACMPEQ, labels(idx)._1)

          case ACmpNe(ALabel(idx, _)) =>
            m.visitJumpInsn(IF_ACMPNE, labels(idx)._1)

          case New(ANew(_, owner)) =>
            m.visitTypeInsn(NEW, owner)

          case CheckCast(owner) =>
            m.visitTypeInsn(CHECKCAST, owner)

          case GetStatic(owner, name, desc, _) =>
            m.visitFieldInsn(GETSTATIC, owner, name, desc)

          case InvokeSpecial1(owner, name, args, _) =>
            m.visitMethodInsn(INVOKESPECIAL, owner, name, args, false)

          case InvokeSpecial2(owner, name, args, _) =>
            m.visitMethodInsn(INVOKESPECIAL, owner, name, args, false)

          case InvokeVirtual0(owner, name, args, _) =>
            m.visitMethodInsn(INVOKEVIRTUAL, owner, name, args, false)

          case InvokeVirtual1(owner, name, args, _) =>
            m.visitMethodInsn(INVOKEVIRTUAL, owner, name, args, false)

          case InvokeInterface0(owner, name, args, _) =>
            m.visitMethodInsn(INVOKEINTERFACE, owner, name, args, true)

          case InvokeInterface1(owner, name, args, _) =>
            m.visitMethodInsn(INVOKEINTERFACE, owner, name, args, true)

          case Fail(_) => ???

          case Noop =>
            m.visitInsn(NOP)

          case Pop =>
            m.visitInsn(POP)

          case Duplicate =>
            m.visitInsn(DUP)

          case Swap =>
            m.visitInsn(SWAP)

          case StoreRegister0 =>
            m.visitVarInsn(ASTORE, 5)

          case LoadRegister0 =>
            m.visitVarInsn(ALOAD, 5)

          case IAdd =>
            m.visitInsn(IADD)
        }
      }
      address
    }

    compile(main, 0)

    // store input index to be returned from method
    main.visitVarInsn(ALOAD, 3)
    main.visitInsn(ICONST_0)
    main.visitVarInsn(ILOAD, 4)
    main.visitInsn(IASTORE)

    main.visitInsn(ARETURN)
    main.visitMaxs(0, 0)
    main.visitEnd()

    (owner, c.toByteArray)
  }
}
