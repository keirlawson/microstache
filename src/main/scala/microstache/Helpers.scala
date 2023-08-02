package microstache

import io.circe.Json

object Helpers {
    val lower = new Helper[Json] {

        val name = "lower"

        //FIXME handle error cases
      def apply(a: Json)(implicit renderable: Renderable[Json]): String = a.asString.get.toLowerCase()

    }
}