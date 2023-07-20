package microstache

import cats.data.NonEmptyList
import cats.syntax.all._
import microstache.Ast.Text
import microstache.Ast.Expression

case class Template(contents: List[Ast.Term])

object Ast {
  case class Identifier(segments: NonEmptyList[String])
  sealed trait Term
  case class Text(value: String) extends Term
  case class Expression(value: Identifier) extends Term
}