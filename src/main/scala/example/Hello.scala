package example

import cats.effect.IOApp
import cats.effect.IO
import cats.parse.Parser
import cats.parse.Rfc5234._

object Ast {
  case class Template(contents: List[Foo])
}

object Hello extends IOApp.Simple {
  val template = "foo{{bar}}baz"
  
  val parser = {
    val generalText = alpha.repAs0[String].filter(s => !s.contains("{{"))
    val openExpression = Parser.string("{{")
    val closeExpression = Parser.string("}}")
    val expression = alpha.repAs[String].between(openExpression, closeExpression)
    generalText ~ expression ~ generalText
  }
  
  val result = parser.parse(template)

  val run = IO.println(result)
}

