package microstache

import cats.parse.{Parser => P}
import cats.parse.Rfc5234._
import cats.syntax.all._

object Parser {
  //FIXME support empty expressions
  val parser = {
    val openExpression = P.string("{{")
    val closeExpression = P.string("}}")
    val startsWithAlpha = (alpha ~ (alpha | digit).rep0).map { case (first, rest) => rest.prepended(first).mkString }
    //FIXME support empty path ie .
    val identifier = {
      val dot = P.char('.')
      startsWithAlpha.repSep(dot).map(Ast.Identifier)
    }
    val handlerInvocation = (startsWithAlpha ~ wsp.rep ~ identifier).map{ case ((name, _), id) => Ast.HelperInvocation(name, id) }
    val expressionContents: P[Ast.Expression] = (handlerInvocation.backtrack | identifier)
    val expression = expressionContents.between(openExpression ~ wsp.rep0, wsp.rep0 ~ closeExpression)

    val generalText = char.repUntilAs[String](openExpression).map(Ast.Text)
    
    (generalText.?.with1 ~ expression ~ generalText.?).rep0.map { lst =>
      val combined: List[Ast.Term] = lst.flatMap { case ((before, exp), after) => 
        List(before, Some(exp), after).flatten
      }
      Template(combined)
    }//.orElse(P.pure("").as(Ast.Template(Nil)))
  }
}