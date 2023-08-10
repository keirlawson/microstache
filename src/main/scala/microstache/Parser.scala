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
      val anonParam = (identifier <* P.not(P.char('='))).backtrack
      val namedParameter = startsWithAlpha ~ (P.char('=') *> identifier)
      val allParams = wsp.rep0 *> anonParam.repSep(wsp.rep) ~ (wsp.rep *> namedParameter.repSep(wsp.rep)).?
      val handlerInvocation = wsp.rep0.with1 *> (startsWithAlpha ~ allParams).map{ case (name, (params, namedParams)) => Ast.HelperInvocation(name, params, namedParams.map(_.toList.toMap).getOrElse(Map.empty)) }
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