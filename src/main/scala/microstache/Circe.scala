package microstache

import io.circe.Json
import cats.data.NonEmptyList
import io.circe.ACursor

object Circe {
    implicit val jsonRenderer: Renderable[Json] = new Renderable[Json] {
        def render(a: Json): String = a.spaces2 //FIXME is this right? - no, need to render literals without quotes
    }
    implicit val jsonResolver: ValueResolver[Json,Json] = new ValueResolver[Json, Json] {
        def resolve(hash: Json, path: List[String])(implicit rendarable: Renderable[Json]): Option[Json] = {
            
            val result = path.foldLeft[ACursor](hash.hcursor)((cursor, segment) => cursor.downField(segment))

            result.focus

        }

    }
}