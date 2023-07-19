package microstache

import cats.data.NonEmptyList

object Ast {
  case class Template(contents: List[Term])
  case class Identifier(segments: NonEmptyList[String])
  sealed trait Term
  case class Text(value: String) extends Term
  case class Expression(value: Identifier) extends Term
}