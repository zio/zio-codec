package zio.codec

sealed trait CodecVM

object CodecVM {
  private[zio] final case class ALabel(num: Int, address: Int)

  private[zio] final case class Read(min: Option[Int], max: Option[Int])  extends CodecVM // read from input, push to stack
  private[zio] final case class Push(value: AnyRef)                       extends CodecVM // push value
  private[zio] final case class CheckSet(s: Set[Any])                     extends CodecVM // pot 1, test value is in set, push the result as boolean on the stack
  private[zio] final case class Jump(label: ALabel)                       extends CodecVM // unconditional jump to new address
  private[zio] final case class ICmpEq(label: ALabel)                     extends CodecVM // pop 2 integers and jump to new address if they are equal
  private[zio] final case class ACmpEq(label: ALabel)                     extends CodecVM // pop 2 references and jump to new address if they are equal
  private[zio] final case class ACmpNe(label: ALabel)                     extends CodecVM // pop 2 references and jump to new address if they are not equal
  private[zio] final case class Construct1(f: AnyRef => AnyRef)           extends CodecVM // pop 1 thing, pass to f and push to stack
  private[zio] final case class Construct2(f: (AnyRef, AnyRef) => AnyRef) extends CodecVM // pop 2 things, pass to f and push to stack
  private[zio] final case class Fail(err: String)                         extends CodecVM // exit with failure
  private[zio] final case class BeginMethod(name: Int)                    extends CodecVM // denote start of a new method
  private[zio] final case class Return(name: Int)                         extends CodecVM // return from method
  private[zio] final case class InvokeStatic(name: Int, address: Int)     extends CodecVM // call a method
  private[zio] final case object Noop                                     extends CodecVM // do nothing
  private[zio] final case object Pop                                      extends CodecVM // pop 1 thing
  private[zio] final case object Duplicate                                extends CodecVM // duplicate stack head
  private[zio] final case object StoreRegister0                           extends CodecVM // pop 1 thing from stack and store in var 0
  private[zio] final case object LoadRegister0                            extends CodecVM // push var 0
  private[zio] final case object IAdd                                     extends CodecVM // pop 2 integers, add, push result
  private[zio] final case object IIndexPush                               extends CodecVM // push read index to stack
  private[zio] final case object IIndexPop                                extends CodecVM // pop read index from stack
  private[zio] final case object IIndexStore                              extends CodecVM // pop read index from stack and assign to read index
}
