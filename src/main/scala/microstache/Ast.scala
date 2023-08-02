package microstache

import cats.data.NonEmptyList
import cats.syntax.all._
import microstache.Ast.Text
import microstache.Ast.Expression

trait ValueResolver[A, B] {
  def resolve(hash: A, path: List[String])(implicit rendarable: Renderable[B]): Option[B]
}

trait Renderable[A] {
  def render(a: A): String
}

case class ResolutionError(message: String) extends RuntimeException

case class Template(contents: List[Ast.Term])

//FIXME support literals as params as well as IDs
object Ast {
  case class Identifier(segments: NonEmptyList[String]) extends Expression
  case class HelperInvocation(name: String, params: NonEmptyList[Identifier]) extends Expression
  sealed trait Term
  sealed trait Expression extends Term
  case class Text(value: String) extends Term
}