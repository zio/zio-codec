package zio.codec

import java.lang.reflect.Method
import java.util.UUID

import com.github.ghik.silencer.silent
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

  def decoder[A](codec: Codec[A]): Chunk[Input] => Either[DecodeError, (Int, A)] = {
    val CodecCompilationResult(program, _, _, _) = compileCodec(codec)
//    println(program.zipWithIndex.map { case (o, i) => i.toString.reverse.padTo(3, '0').reverse + ": " + o }.mkString("", "\n", "\n"))
    interpret(_, program).asInstanceOf[Either[DecodeError, (Int, A)]]
  }

  def encoder[A](codec: Codec[A]): A => Either[EncodeError, Chunk[Input]] = ???

  def printer[A](codec: Codec[A]): List[String] = ???

  // todo: rename functions, fix signature
  def decoder2[A](codec: Codec[A]): Chunk[Input] => (Int, A) = {
    val CodecCompilationResult(program, labels, values, lookups) = compileCodec(codec)
    println(program.zipWithIndex.map { case (o, i) => i.toString.reverse.padTo(3, '0').reverse + ": " + o }
      .mkString("", "\n", "\n"))

    val (name, bytes) = compileCodecVM(program, labels, values, lookups)
    val method        = new ByteClassLoader().load(name, bytes)

    println(method)
    println()

    s => (123, method.invoke(null, values, lookups, s.toArray).asInstanceOf[A])
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

    var methodCount: Int  = 0
    var invoked: Set[Int] = Set.empty

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

    val v0                    = VM.AValue(0, NoValue)
    val v1                    = VM.AValue(1, BoxedUnit.UNIT)
    val v2                    = VM.AValue(2, None)
    val v3                    = VM.AValue(3, Chunk.empty)
    var valueCount: Int       = 4
    var values: Array[AnyRef] = Array(NoValue, BoxedUnit.UNIT, None, Chunk.empty)
    def newValue(value: AnyRef): VM.AValue =
      value match {
        case NoValue        => v0
        case BoxedUnit.UNIT => v1
        case None           => v2
        case Chunk.empty    => v3
        case v =>
          val idx = valueCount
          values = values :+ v
          valueCount += 1
          VM.AValue(idx, v)
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
                VM.Push(newValue(NoValue))
              )

              val mapProgram: Array[CodecVM] = map.equiv match {
                case Equiv.Left(_)      => Array(VM.Call1(_.asInstanceOf[(AnyRef, _)]._1))
                case Equiv.Right(_)     => Array(VM.Call1(_.asInstanceOf[(_, AnyRef)]._2))
                case Equiv.Merge()      => Array(VM.Call1(_.asInstanceOf[Either[AnyRef, AnyRef]].merge))
                case Equiv.Ignore(_)    => Array(VM.Pop, VM.Push(newValue(BoxedUnit.UNIT)))
                case Equiv.As(b, _)     => Array(VM.Pop, VM.Push(newValue(b.asInstanceOf[AnyRef])))
                case Equiv.Chars.String => Array(VM.Call1(_.asInstanceOf[Chunk[Char]].mkString))
                case Equiv.Bits.UInt16  => ???

                case Equiv.ForTesting(f) => Array(VM.Call1(f.asInstanceOf[AnyRef => AnyRef]))
              }

              val lBail = newLabel(offset + program.length + 1 + mapProgram.length)

              program ++ Array(
                VM.ACmpEq(lBail)
              ) ++ mapProgram ++ Array(
                VM.Return(name)
              )

            case filter @ Codec.Filter(value, _, mod) =>
              val program = Array(
                VM.BeginMethod(name)
              ) ++ compile(value(), offset + 1, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(newValue(NoValue))
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
                VM.Push(newValue(NoValue)),
                VM.Return(name)                  // match, exit
              )
              // format: on

            case Codec.Zip(left, right) =>
              val programL = Array(
                VM.BeginMethod(name),
                VM.New(VM.ANew(UUID.randomUUID(), "scala/Tuple2")),
                VM.Duplicate
              ) ++ compile(left(), offset + 3, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(newValue(NoValue))
              )

              val programR = compile(right(), offset + programL.length + 1, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(newValue(NoValue))
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
                  VM.Push(newValue(NoValue)),
                  VM.Return(name)                // exit
                )
              // format: on

            case Codec.Opt(value) =>
              val program = Array(
                VM.BeginMethod(name),
                VM.InputIdxPush,
                VM.New(VM.ANew(UUID.randomUUID(), "scala/Some")),
                VM.Duplicate
              ) ++ compile(value(), offset + 4, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(newValue(NoValue))
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
                VM.InputIdxPopSet,
                VM.Push(newValue(None)),
                VM.Return(name)                  // exit
              )
              // format: on

            case Codec.Alt(left, right) =>
              val programL = Array(
                VM.BeginMethod(name),
                VM.InputIdxPush
              ) ++ compile(left(), offset + 2, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(newValue(NoValue))
              )

              val programR = compile(right(), offset + programL.length + 3, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(newValue(NoValue))
              )

              val lAcceptL = newLabel(offset + programL.length + 3 + programR.length + 3)
              val lExit    = newLabel(offset + programL.length + 3 + programR.length + 6)

              // format: off
              programL ++
                Array(
                  VM.ACmpNe(lAcceptL),
                  VM.Pop,                        // try another program
                  VM.InputIdxPopSet
                ) ++ programR ++
                Array(
                  VM.ACmpEq(lExit),
                  VM.Call1(Right.apply),         // accept right
                  VM.Jump(lExit),
                  VM.Call1(Left.apply),          // accept left
                  VM.Swap,
                  VM.InputIdxPop,
                  VM.Return(name)                // exit
                )
              // format: on

            case Codec.Rep(value, None, None) =>
              val program = Array(
                VM.BeginMethod(name),
                VM.Push(newValue(Chunk.empty)),
                VM.InputIdxPush // repeat
              ) ++ compile(value(), offset + 3, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(newValue(NoValue))
              )

              val lRepeat = newLabel(offset + 2)
              val lFinish = newLabel(offset + program.length + 5)

              // format: off
              program ++ Array(
                VM.ACmpEq(lFinish),

                VM.Swap,                         // accumulate
                VM.InputIdxPop,
                VM.Call2((l, a) => l.asInstanceOf[Chunk[AnyRef]] + a),
                VM.Jump(lRepeat),

                VM.Pop,                          // finish
                VM.InputIdxPopSet,
                VM.Return(name)
              )
              // format: on

            // todo: remove guard! bug in Scala prevents exhaustive matching
            case Codec.Rep(value, Some(min), Some(max)) if min == max =>
              val program = Array(
                VM.BeginMethod(name),
                VM.PushInt(0),
                VM.Push(newValue(Chunk.empty)),
                VM.InputIdxPush // repeat
              ) ++ compile(value(), offset + 4, chain1) ++ Array(
                VM.Duplicate,
                VM.Push(newValue(NoValue))
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
                VM.Call2((l, a) => l.asInstanceOf[Chunk[AnyRef]] + a),
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
                VM.InputIdxPopSet,
                VM.Pop,
                VM.Pop,
                VM.Push(newValue(NoValue)),
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

    val frameStack: Stack[AnyRef] = Stack()

    var r0: AnyRef = null.asInstanceOf[AnyRef]

    while (i < program.length) {
      val instr: CodecVM = program(i)
      instr match {
        case InputRead(None, None) =>
          inputIndex += 1
          if (inputIndex < inputLength) stack.push(input(inputIndex).asInstanceOf[AnyRef])
          else stack.push(NoValue)
          i += 1

        case InputRead(_, _) =>
          ???

        case InputIdxPush =>
          stack.push(inputIndex.asInstanceOf[AnyRef])
          i += 1

        case InputIdxPop =>
          stack.pop()
          i += 1

        case InputIdxPopSet =>
          inputIndex = stack.pop().asInstanceOf[Int]
          i += 1

        case Push(AValue(_, value)) =>
          stack.push(value)
          i += 1

        case PushInt(int) =>
          stack.push(int.asInstanceOf[AnyRef])
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

        case New(ins @ ANew(id, _)) =>
          createdInstances += (id -> null.asInstanceOf[AnyRef])
          stack.push(ins)
          i += 1

        case Call1(f) =>
          stack.push(f(stackPopResolve()))
          i += 1

        case Call2(f) =>
          val arg2 = stackPopResolve()
          val arg1 = stackPopResolve()
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
      val m = c.getMethods.head
      m
    }
  }

  @silent
  private def compileCodecVM(
    instructions: Array[CodecVM],
    addresses: Map[Int, Int],
    values: Array[AnyRef],
    lookups: Array[java.util.HashSet[Any]]
  ): (String, Array[Byte]) = {
    import CodecVM._
    import Opcodes._

    val labels: Map[Int, (Label, Int)] =
      addresses.map { case (key, address) => (key, (new Label(), address)) }
    val labelAddresses: Map[Int, List[Label]] =
      labels.groupBy(_._2._2).map { case (address, list) => (address, list.toList.map(_._2._1)) }

    def m_pushInt(m: MethodVisitor, i: Int): Unit =
      i match {
        case 0                              => m.visitInsn(ICONST_0)
        case 1                              => m.visitInsn(ICONST_1)
        case 2                              => m.visitInsn(ICONST_2)
        case 3                              => m.visitInsn(ICONST_3)
        case b if -128 <= b && b <= 127     => m.visitIntInsn(BIPUSH, b)
        case s if -32768 <= s && s <= 32767 => m.visitIntInsn(SIPUSH, s)
        case i                              => m.visitLdcInsn(i)
      }

    val c: ClassWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES)
    val name: String   = "MyCodec"

    c.visit(V1_8, ACC_PUBLIC | ACC_SUPER, name, null, "java/lang/Object", null)

    val m: MethodVisitor =
      c.visitMethod(
        ACC_PUBLIC | ACC_STATIC,
        "run",
        "([Ljava/lang/Object;[Ljava/util/HashSet;[C)Ljava/lang/Object;",
        null,
        null
      )
    m.visitCode()

    // var0: Array[Object]  - external values
    // var1: Array[HashSet] - inclusion sets
    // var2: Array[char]    - input
    // var3: int            - input index
    m.visitInsn(ICONST_M1)
    m.visitVarInsn(ISTORE, 3)

    instructions.zipWithIndex.foreach {
      case (ins, address) =>
        labelAddresses
          .get(address)
          .foreach(_.foreach(m.visitLabel))

        ins match {
          case InputRead(None, None) =>
            val labelNoInput = new Label()
            val labelDone    = new Label()

            m.visitIincInsn(3, 1)

            m.visitVarInsn(ILOAD, 3)
            m.visitVarInsn(ALOAD, 2)
            m.visitInsn(ARRAYLENGTH);
            m.visitJumpInsn(IF_ICMPGE, labelNoInput)

            m.visitVarInsn(ALOAD, 2)
            m.visitVarInsn(ILOAD, 3)
            m.visitInsn(CALOAD)
            m.visitMethodInsn(INVOKESTATIC, "java/lang/Character", "valueOf", "(C)Ljava/lang/Character;", false)
            m.visitJumpInsn(GOTO, labelDone)

            m.visitLabel(labelNoInput)
            m.visitVarInsn(ALOAD, 0)
            m.visitInsn(ICONST_0) // todo: do not assume NoValue is first, use PushNoValue
            m.visitInsn(AALOAD)

            m.visitLabel(labelDone)

          case InputRead(_, _) =>
            ???

          case InputIdxPush =>
            m.visitVarInsn(ILOAD, 3)

          case InputIdxPop =>
            m.visitInsn(POP)

          case InputIdxPopSet =>
            m.visitVarInsn(ISTORE, 3)

          case Push(AValue(0, NoValue)) =>
            m.visitVarInsn(ALOAD, 0)
            m.visitInsn(ICONST_0)
            m.visitInsn(AALOAD)

          case Push(AValue(idx, _)) =>
            m.visitVarInsn(ALOAD, 0)
            m_pushInt(m, idx)
            m.visitInsn(AALOAD)

          case PushInt(int) =>
            m_pushInt(m, int)

          case CheckSet(ASet(idx, _)) =>
            m.visitVarInsn(ALOAD, 1)
            m_pushInt(m, idx)
            m.visitInsn(AALOAD)

            m.visitInsn(SWAP)
            m.visitMethodInsn(INVOKEVIRTUAL, "java/util/HashSet", "contains", "(Ljava/lang/Object;)Z", false);

          case Jump(ALabel(idx, _)) =>
            m.visitJumpInsn(GOTO, labels(idx)._1)

          case ICmpEq(ALabel(idx, _)) =>
            m.visitJumpInsn(IF_ICMPEQ, labels(idx)._1)

          case ACmpEq(ALabel(idx, _)) =>
            m.visitJumpInsn(IF_ACMPEQ, labels(idx)._1)

          case ACmpNe(ALabel(idx, _)) =>
            m.visitJumpInsn(IF_ACMPNE, labels(idx)._1)

          case Call1(f) => ???
          case Call2(f) => ???

          case New(ANew(_, owner)) =>
            m.visitTypeInsn(NEW, owner)

          case InvokeSpecial1(owner, name, args, _) =>
            m.visitMethodInsn(INVOKESPECIAL, owner, name, args, false)

          case InvokeSpecial2(owner, name, args, _) =>
            m.visitMethodInsn(INVOKESPECIAL, owner, name, args, false)

          case Fail(err)                   => ???
          case BeginMethod(name)           => ???
          case Return(name)                => ???
          case InvokeStatic(name, address) => ???

          case Noop =>
            m.visitInsn(NOP)

          case Pop =>
            m.visitInsn(POP)

          case Duplicate =>
            m.visitInsn(DUP)

          case Swap =>
            m.visitInsn(SWAP)

          case StoreRegister0 => ???
          case LoadRegister0  => ???
          case IAdd           => ???
        }
    }

    m.visitInsn(ARETURN)
    m.visitMaxs(0, 0)
    m.visitEnd()

    (name, c.toByteArray)
  }
}
