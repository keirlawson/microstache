package microstache

import cats.data.NonEmptyList

sealed trait Value[A]
case class Complex[A](value: A) extends Value[A]
case class StringLiteral[A](value: String) extends Value[A]

case class HelperParameters[A](
    params: NonEmptyList[(Int, Value[A])],
    named: Map[String, Value[A]],
    block: Option[Template]
)

case class HelperError(message: String) extends RuntimeException

trait Helper[A] {

  val name: String

  def apply(params: HelperParameters[A])(implicit
      renderable: Renderable[A]
  ): Either[HelperError, String] // FIXME allow outputting of whole template
}
