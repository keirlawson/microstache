package microstache

import cats.data.NonEmptyList

//FIXME support block helpers

sealed trait Value[A]
case class Complex[A](value: A) extends Value[A]
case class StringLiteral[A](value: String) extends Value[A]

case class HelperParameters[A](
    params: NonEmptyList[(Int, Value[A])],
    named: Map[String, Value[A]]
)

case class HelperError(message: String) extends RuntimeException

trait Helper[A] {

  val name: String

  def apply(params: HelperParameters[A])(implicit
      renderable: Renderable[A]
  ): Either[HelperError, String]
}
