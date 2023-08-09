package microstache

import cats.data.NonEmptyList
import cats.syntax.all._
import microstache.Ast.Text
import microstache.Ast.Expression
import cats.Show
import microstache.Ast.Identifier
import microstache.Ast.HelperInvocation

trait ValueResolver[A, B] {
  def resolve(hash: A, path: List[String])(implicit rendarable: Renderable[B]): Option[B]
}

trait Renderable[A] {
  def render(a: A): String
}

case class ResolutionError(message: String) extends RuntimeException

case class Template(contents: List[Ast.Term])

object Template {
  implicit val showInstance: Show[Template] = new Show[Template] {

    def show(t: Template): String = {
      t.contents.map { 
        case Text(value) => value
        case e: Expression => showExpression(e)
      }.mkString
    }

    private def showIdentifier(id: Identifier): String = {
      id.segments.toList.mkString(".")
    }

    private def showExpression(exp: Expression): String = {
      val asStr = exp match {
        case id: Identifier => showIdentifier(id)
        case HelperInvocation(name, params, namedParams) => {
          val paramList = params.toList.map(showIdentifier).mkString(" ")
          s"$name $paramList"
        }
      }
      asStr.prependedAll("{{").appendedAll("}}")
    }

  }
}

//FIXME support literals as params as well as IDs
object Ast {
  case class Identifier(segments: NonEmptyList[String]) extends Expression
  case class HelperInvocation(name: String, params: NonEmptyList[Identifier], namedParams: Map[String, Identifier]) extends Expression
  sealed trait Term
  sealed trait Expression extends Term
  case class Text(value: String) extends Term
}