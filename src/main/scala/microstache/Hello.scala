package microstache

import cats.effect.IOApp
import cats.effect.IO

object Hello extends IOApp.Simple {
  val template = "foo{{bar}}baz{{qux}}"
  
  val result = Parser.parser.parseAll(template).toOption.get

  val run = IO.println(result.render())
}

