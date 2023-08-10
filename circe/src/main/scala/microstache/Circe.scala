package microstache

import io.circe.Json
import io.circe.ACursor
import io.circe.Decoder
import cats.syntax.all._
import io.circe.Encoder

//FIXME add some more e2e-style tests around this
object Circe {

  // FIXME should be namespaced separately from the other stuff
  implicit val templateDecoder: Decoder[Template] =
    Decoder[String].emap(s => Parser.parser.parseAll(s).leftMap(e => e.show))

  implicit val templateEncoder: Encoder[Template] =
    Encoder[String].contramap(_.show)

  implicit val jsonRenderer: Renderable[Json] = new Renderable[Json] {
    def render(a: Json): String =
      a.spaces2 // FIXME is this right? - no, need to render literals without quotes
  }
  implicit val jsonResolver: ValueResolver[Json, Json] =
    new ValueResolver[Json, Json] {
      def resolve(hash: Json, path: List[String])(implicit
          rendarable: Renderable[Json]
      ): Option[Json] = {

        val result = path.foldLeft[ACursor](hash.hcursor)((cursor, segment) =>
          cursor.downField(segment)
        )

        result.focus

      }

    }
}
