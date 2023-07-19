package microstache

import cats.effect.IOApp
import cats.effect.IO

object Hello extends IOApp.Simple {
  val template = "foo{{bar}}baz{{qux}}"
  
  val result = Parser.parser.parse(template)

  val run = IO.println(result)
}

