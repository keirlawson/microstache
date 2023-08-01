package microstache

import cats.effect.IOApp
import cats.effect.IO
import cats.syntax.all._
import io.circe.Json

object Hello extends IOApp.Simple {

  def render[A, B](template: Template, hash: A)(implicit resolver: ValueResolver[A, B], renderable: Renderable[B]): Either[ResolutionError, String] = {
    import Ast._
    val strs = template.contents.traverse {
      case Text(value) => value.asRight[ResolutionError]
      case Expression(Ast.Identifier(segments)) => {
        val res = resolver.resolve(hash, segments.toList)
        res.map(renderable.render).toRight(ResolutionError())
      }
    }

    strs.map(_.mkString)
  }

  val template = "foo{{bar}}baz{{qux}}"
  
  val result = Parser.parser.parseAll(template).toOption.get

  val hash = Json.obj("bar" -> Json.fromString("moo"), "qux" -> Json.fromString("coo"))

  import Circe._
  val run = IO.println(render[Json, Json](result, hash))
}

