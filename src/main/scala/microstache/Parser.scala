package microstache

import cats.parse.{Parser => P}
import cats.parse.Rfc5234._

object Parser {
  val parser = {
    val openExpression = P.string("{{")
    val closeExpression = P.string("}}")
    val expression = alpha.repAs[String].between(openExpression, closeExpression).map(Ast.Expression)

    val generalText = char.repUntilAs[String](openExpression).map(Ast.Text)
    
    (generalText.?.with1 ~ expression ~ generalText.?).rep0.map { lst =>
      val combined = lst.flatMap { case ((before, exp), after) => 
        List(before, Some(exp), after).flatten
      }
      Ast.Template(combined)
    }
  }
}