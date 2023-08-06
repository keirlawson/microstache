package microstache

import cats.parse.{Parser => P}
import cats.parse.Rfc5234._
import cats.syntax.all._
import cats.parse.Parser0

object Parser {

  val parser: Parser0[Template] = {
    val openExpression = P.string("{{")
    val closeExpression = P.string("}}")
    val expressionContents: P[Ast.Expression] = {
      val startsWithAlpha = (alpha ~ (alpha | digit).rep0).map { case (first, rest) => rest.prepended(first).mkString }
      //FIXME support empty path ie .
      val identifier = {
        val dot = P.char('.')
        startsWithAlpha.repSep(dot).map(Ast.Identifier)
      }
      val handlerInvocation = (startsWithAlpha ~ (wsp.rep *> identifier).rep).map{ case (name, params) => Ast.HelperInvocation(name, params) }
      (handlerInvocation.backtrack | identifier)
    }
    val expression = expressionContents.between(openExpression ~ wsp.rep0, wsp.rep0 ~ closeExpression)

    val generalText = char.repUntilAs[String](openExpression).map(Ast.Text)
    
    (generalText.? ~ (expression ~ generalText.?).rep0).map { case (initial, rest) =>

      val combined = initial :: rest.flatMap { case (exp, after) => List(Some(exp), after) }

      Template(combined.flatten)
    }
  }
}