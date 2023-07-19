package microstache

import cats.parse.{Parser => P}
import cats.parse.Rfc5234._

object Parser {
      val parser = {
    val generalText = alpha.repAs[String].filter(s => !s.contains("{{")).map(Ast.Text)
    val openExpression = P.string("{{")
    val closeExpression = P.string("}}")
    val expression = alpha.repAs[String].between(openExpression, closeExpression).map(Ast.Expression)
    (generalText.?.with1 ~ expression ~ generalText.?).rep0.map { lst =>
      val combined = lst.flatMap { case ((before, exp), after) => 
        List(before, Some(exp), after).flatten
      }
      Ast.Template(combined)
    }
  }
}