package microstache

import io.circe.Json
import cats.syntax.all._

object Helpers {
  val lower = new Helper[Json] {
    val name = "lower"

    def apply(params: HelperParameters[Json])(implicit
        renderable: Renderable[Json]
    ): Either[HelperError, String] = {

      params.params.get(0).get match {
        case Complex(value) =>
          value.asString
            .toRight(HelperError("lower helper was passed a non-string type"))
            .map(_.toLowerCase())
        case StringLiteral(value) => value.toLowerCase().asRight
      }
    }

  }
}
