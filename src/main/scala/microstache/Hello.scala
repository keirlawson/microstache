package microstache

import cats.effect.IOApp
import cats.effect.IO
import cats.syntax.all._
import io.circe.Json

object Hello extends IOApp.Simple {

  val template = "foo{{lower bar}}baz{{qux}}"
  
  val result = Parser.parser.parseAll(template).toOption.get

  val hash = Json.obj("bar" -> Json.fromString("MOO"), "qux" -> Json.fromString("coo"))

  import Circe._

  val renderer = Renderer[Json, Json](List(Helpers.lower))
  val run = IO.println(renderer.render(result, hash))
}

