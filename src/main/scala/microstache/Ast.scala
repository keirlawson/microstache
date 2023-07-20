package microstache

import cats.data.NonEmptyList
import cats.syntax.all._
import microstache.Ast.Text
import microstache.Ast.Expression

trait ValueResolver[A] {
  def resolve(hash: A, path: NonEmptyList[String]): Option[String]
}

case class ResolutionError() extends RuntimeException

case class Template[A](contents: List[Ast.Term])(implicit resolver: ValueResolver[A]) {
  def render(hash: A): Either[ResolutionError, String] = {
    val strs = contents.traverse {
      case Text(value) => value.asRight[ResolutionError]
      case Expression(Ast.Identifier(segments)) => resolver.resolve(hash, segments).toRight(ResolutionError())
    }

    strs.map(_.mkString)
  }
}

object Ast {
  case class Identifier(segments: NonEmptyList[String])
  sealed trait Term
  case class Text(value: String) extends Term
  case class Expression(value: Identifier) extends Term
}