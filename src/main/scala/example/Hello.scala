package example

import cats.effect.IOApp
import cats.effect.IO

object Ast {
  case class Template(contents: List[Term])
  sealed trait Term
  case class Text(value: String) extends Term
  case class Expression(value: String) extends Term
}

object Hello extends IOApp.Simple {
  val template = "foo{{bar}}baz{{qux}}"
  

  
  val result = Parser.parser.parse(template)

  val run = IO.println(result)
}

