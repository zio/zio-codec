package zio.codec

trait Enumerable[+A] {

  def enumerate: scala.Stream[A]

}
