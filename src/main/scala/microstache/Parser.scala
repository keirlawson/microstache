package microstache

import cats.parse.{Parser => P}
import cats.parse.Rfc5234._

object Parser {
  val parser = {
    val openExpression = P.string("{{")
    val closeExpression = P.string("}}")
    val identifier = {
      val dot = P.char('.')
      val segment = (alpha ~ (alpha | digit).rep0).map { case (first, rest) => rest.prepended(first).mkString }
      segment.repSep(dot).map(Ast.Identifier)
    }
    val expression = identifier.between(openExpression ~ wsp.rep0, wsp.rep0 ~ closeExpression).map(Ast.Expression)

    val generalText = char.repUntilAs[String](openExpression).map(Ast.Text)
    
    (generalText.?.with1 ~ expression ~ generalText.?).rep0.map { lst =>
      val combined = lst.flatMap { case ((before, exp), after) => 
        List(before, Some(exp), after).flatten
      }
      Ast.Template(combined)
    }//.orElse(P.pure("").as(Ast.Template(Nil)))
  }
}