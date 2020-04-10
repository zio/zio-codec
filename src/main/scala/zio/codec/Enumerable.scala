package zio.codec

trait Enumerable[+A] {

  def enumerate: Iterable[A]

}
