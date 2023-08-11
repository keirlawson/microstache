package microstache

import cats.data.NonEmptyList
import microstache.Ast._
import cats.Show

trait ValueResolver[A, B] {
  def resolve(hash: A, path: List[String])(implicit
      rendarable: Renderable[B]
  ): Option[B]
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
        case Text(value)   => value
        case e: Expression => showExpression(e)
      }.mkString
    }

    private def showIdentifier(id: Identifier): String = {
      id.segments.toList.mkString(".")
    }

    private def showValue(v: Value): String = {
      v match {
        case id: Identifier       => showIdentifier(id)
        case StringLiteral(value) => s"\"$value\""
      }
    }

    private def showExpression(exp: Expression): String = {
      val asStr = exp match {
        case id: Identifier => showIdentifier(id)
        case HelperInvocation(name, params, namedParams) => {
          val paramList = params.toList.map(showValue).mkString(" ")
          val namedParam = namedParams.toList
            .map { case (k, v) => s"$k=${showValue(v)}" }
            .mkString(" ")
          val namedParamList =
            if (namedParam.isEmpty()) "" else namedParam.prepended(' ')
          s"$name $paramList$namedParamList"
        }
      }
      asStr.prependedAll("{{").appendedAll("}}")
    }

  }
}

//FIXME support literals as params as well as IDs
object Ast {
  sealed trait Value
  case class Identifier(segments: NonEmptyList[String])
      extends Expression
      with Value
  case class StringLiteral(value: String) extends Value
  case class HelperInvocation(
      name: String,
      params: NonEmptyList[Value],
      namedParams: Map[String, Value]
  ) extends Expression
  sealed trait Term
  sealed trait Expression extends Term
  case class Text(value: String) extends Term
}
