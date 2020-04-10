package zio.codec

sealed trait CodecVM {

}

object CodecVM {
  final case class Push(value: AnyRef) extends CodecVM
  final case class Read(min: Option[Int], max: Option[Int]) extends CodecVM // read from input
  final case class CheckSet(s: Set[AnyRef]) extends CodecVM // test head of the stack is in bitset, push the result as boolean on the stack
  final case class CondJump(ifEqual: Int, otherwise: Int) extends CodecVM
  final case class Construct1(f: AnyRef => AnyRef) extends CodecVM // pop 1 thing, pass to f and push to stack
  final case class Construct2(f: (AnyRef, AnyRef) => AnyRef) extends CodecVM // pop 2 things, pass to f and push to stack
  final case class Fail(err: String) extends CodecVM
  final case object StoreRegister0 extends CodecVM // pop 1 thing from stack and store in var 0
//  final case class Loop(test: CodecVM, body: CodecVM) extends CodecVM // convention: push a boolean to indicate if need to continue
//  final case class Try(body: CodecVM, recover: CodecVM) extends CodecVM

}
