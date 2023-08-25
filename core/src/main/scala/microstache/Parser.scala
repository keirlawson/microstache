package microstache

import cats.parse.{Parser => P}
import cats.parse.Rfc5234._
import cats.parse.Parser0
import cats.Defer

object Parser {

  val parser: Parser0[Template] = Defer[Parser0].fix[Template] { _ =>
    val openExpression = P.string("{{")
    val closeExpression = P.string("}}")
    val stringLiteral: P[Ast.StringLiteral] = (alpha | digit).rep0.string.with1
      .surroundedBy(P.char('"'))
      .map(s => Ast.StringLiteral(s))
    val expressionContents: P[Ast.Expression] = {
      val startsWithAlpha = (alpha ~ (alpha | digit).rep0).map {
        case (first, rest) => rest.prepended(first).mkString
      }
      val identifier = {
        val dot = P.char('.')
        val withSegments =
          startsWithAlpha.repSep(dot).map(nel => Ast.Identifier(nel.toList))
        val singleDot = dot.as(Ast.Identifier(List.empty))
        withSegments.orElse(singleDot)
      }
      val value: P[Ast.Value] = stringLiteral | identifier
      val anonParam = (value <* P.not(P.char('='))).backtrack
      val namedParameter = startsWithAlpha ~ (P.char('=') *> value)
      val allParams = wsp.rep *> anonParam.repSep(
        wsp.rep
      ) ~ (wsp.rep *> namedParameter.repSep(wsp.rep)).?
      val handlerInvocation =
        wsp.rep0.with1 *> (startsWithAlpha ~ allParams).map {
          case (name, (params, namedParams)) =>
            Ast.HelperInvocation(
              name,
              params,
              namedParams.map(_.toList.toMap).getOrElse(Map.empty)
            )
        }
      (handlerInvocation.backtrack | identifier)
    }
    val expression = expressionContents.between(
      openExpression ~ wsp.rep0,
      wsp.rep0 ~ closeExpression
    )

    val generalText = char.repUntilAs[String](openExpression).map(Ast.Text)

    (generalText.? ~ (expression ~ generalText.?).rep0).map {
      case (initial, rest) =>
        val combined = initial :: rest.flatMap { case (exp, after) =>
          List(Some(exp), after)
        }

        Template(combined.flatten)
    }
  }
}
