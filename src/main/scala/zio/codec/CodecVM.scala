package zio.codec

import java.util.UUID

sealed trait CodecVM

object CodecVM {
  // format: off
  private[zio] final case class ALabel(idx: Int, address: Int)
  private[zio] final case class ASet(idx: Int, set: Set[Any])
  private[zio] final case class AValue(idx: Int, value: AnyRef)
  private[zio] final case class ANew(id: UUID, owner: String)

  private[zio] final case class  InputRead(min: Option[Int], max: Option[Int]) extends CodecVM // read from input, push to stack
  private[zio] final case object InputIdxLoad                                  extends CodecVM // push read index value to stack
  private[zio] final case object InputIdxPop                                   extends CodecVM // pop read index value from stack
  private[zio] final case object InputIdxStore                                 extends CodecVM // pop read index value from stack and assign to read index

  private[zio] final case class  Push(value: AValue)                       extends CodecVM // push value to stack
  private[zio] final case class  PushInt(value: Int)                       extends CodecVM // push int value to stack
  private[zio] final case object PushNoValue                               extends CodecVM // push NoValue to stack
  private[zio] final case object Box                                       extends CodecVM // box top primitive value on the stack
  private[zio] final case class  CheckSet(set: ASet)                       extends CodecVM // pop 1, test if in the set, push the result as boolean to stack

  private[zio] final case class Jump(label: ALabel)                       extends CodecVM // unconditional jump to new address
  private[zio] final case class IfLt(label: ALabel)                       extends CodecVM // pop 1 value and jump to new address if it is less than 0
  private[zio] final case class ICmpEq(label: ALabel)                     extends CodecVM // pop 2 integers and jump to new address if they are equal
  private[zio] final case class ICmpNe(label: ALabel)                     extends CodecVM // pop 2 integers and jump to new address if they are not equal
  private[zio] final case class ACmpEq(label: ALabel)                     extends CodecVM // pop 2 references and jump to new address if they are equal
  private[zio] final case class ACmpNe(label: ALabel)                     extends CodecVM // pop 2 references and jump to new address if they are not equal

  private[zio] final case class Fail(err: String)                         extends CodecVM // exit with failure

  private[zio] final case class BeginMethod(name: String)                 extends CodecVM // begin new method
  private[zio] final case class EndMethod(name: String)                   extends CodecVM // end new method
  private[zio] final case class CallMethod(name: String, address: Int)    extends CodecVM // call method

  private[zio] final case class New(ins: ANew)                            extends CodecVM // creates new instance, push reference on stack
  private[zio] final case class CheckCast(owner: String)                  extends CodecVM // check if value on the stack is of certain type
  private[zio] final case class GetStatic(owner: String, name: String, desc: String, v: AnyRef)                            extends CodecVM // place value of the static field on the stack
  private[zio] final case class InvokeSpecial1(owner: String, name: String, desc: String, f: AnyRef => AnyRef)             extends CodecVM // pop arg and instance, invoke constructor
  private[zio] final case class InvokeSpecial2(owner: String, name: String, desc: String, f: (AnyRef, AnyRef) => AnyRef)   extends CodecVM // pop 2 args and instance, invoke constructor
  private[zio] final case class InvokeVirtual0(owner: String, name: String, desc: String, f: AnyRef => AnyRef)             extends CodecVM // pop instance, invoke virtual method
  private[zio] final case class InvokeVirtual1(owner: String, name: String, desc: String, f: (AnyRef, AnyRef) => AnyRef)   extends CodecVM // pop arg and instance, invoke virtual method
  private[zio] final case class InvokeInterface0(owner: String, name: String, desc: String, f: AnyRef => AnyRef)           extends CodecVM // pop instance, invoke interface method
  private[zio] final case class InvokeInterface1(owner: String, name: String, desc: String, f: (AnyRef, AnyRef) => AnyRef) extends CodecVM // pop arg and instance, invoke interface method

  private[zio] final case object Noop                                     extends CodecVM // do nothing
  private[zio] final case object Pop                                      extends CodecVM // pop 1 thing
  private[zio] final case object Duplicate                                extends CodecVM // duplicate stack head
  private[zio] final case object Swap                                     extends CodecVM // swap 2 operands at the top of the stack
  private[zio] final case object StoreRegister0                           extends CodecVM // pop 1 thing from stack and store in var 0
  private[zio] final case object LoadRegister0                            extends CodecVM // push var 0
  private[zio] final case object IAdd                                     extends CodecVM // pop 2 integers, add, push result
  // format: on
}
