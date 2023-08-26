package microstache

import cats.parse.{Parser => P}
import cats.parse.Rfc5234._
import cats.parse.Parser0
import cats.Defer
import cats.data.NonEmptyList

object Parser {

  val parser: Parser0[Template] = Defer[Parser0].fix[Template] { recurse =>
    val openExpression = P.string("{{")
    val closeExpression = P.string("}}")
    val startsWithAlpha = (alpha ~ (alpha | digit).rep0).map {
      case (first, rest) => rest.prepended(first).mkString
    }

    val stringLiteral: P[Ast.StringLiteral] = (alpha | digit).rep0.string.with1
      .surroundedBy(P.char('"'))
      .map(s => Ast.StringLiteral(s))

    val identifier = {
      val dot = P.char('.')
      val withSegments =
        startsWithAlpha.repSep(dot).map(nel => Ast.Identifier(nel.toList))
      val singleDot = dot.as(Ast.Identifier(List.empty))
      withSegments.orElse(singleDot)
    }

    val handlerInvocation = {
      val value: P[Ast.Value] = stringLiteral | identifier
      val anonParam = (value <* P.not(P.char('='))).backtrack
      val namedParameter = startsWithAlpha ~ (P.char('=') *> value)
      val allParams = wsp.rep *> anonParam.repSep(
        wsp.rep
      ) ~ (wsp.rep *> namedParameter.repSep(wsp.rep)).?
      wsp.rep0.with1 *> (startsWithAlpha ~ allParams).map {
        case (name, (params, namedParams)) =>
          Ast.HelperInvocation(
            name,
            params,
            namedParams.map(_.toList.toMap).getOrElse(Map.empty)
          )
      }
    }

    val expressionContents: P[Ast.Expression] = {
      (handlerInvocation.backtrack | identifier)
    }
    val expression = expressionContents.between(
      openExpression ~ wsp.rep0,
      wsp.rep0 ~ closeExpression
    )

    val openingBlockExpression = handlerInvocation.between(
      openExpression ~ P.char('#') ~ wsp.rep0,
      wsp.rep0 ~ closeExpression
    )

    val closingBlockExpression = startsWithAlpha.between(
      openExpression ~ P.char('/') ~ wsp.rep0,
      wsp.rep0 ~ closeExpression
    )

    // FIXME add to generator
    val block =
      (openingBlockExpression ~ recurse ~ closingBlockExpression).collect {
        // FIXME use destructure
        case ((open, contents), close) if (open.name == close) =>
          Ast.BlockHelperInvocation(
            open.name,
            open.params,
            open.namedParams,
            contents.contents
          )
      }

    val generalText = char.repUntilAs[String](openExpression).map(Ast.Text)

    // FIXME actually do block or as part of expression
    (generalText.? ~ ((block.backtrack | expression) ~ generalText.?).repUntil0(
      closingBlockExpression
    )).map { case (initial, rest) =>
      val combined = initial :: rest.flatMap { case (exp, after) =>
        List(Some(exp), after)
      }

      Template(combined.flatten)
    }
  }
}
