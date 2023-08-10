package microstache

import io.circe.Json

object Helpers {
  val lower = new Helper[Json] {
    val name = "lower"

    // FIXME handle error cases
    def apply(params: HelperParameters[Json])(implicit
        renderable: Renderable[Json]
    ): String = params.params.get(0).get.asString.get.toLowerCase()

  }
}
