package microstache

import io.circe.Json
import cats.syntax.all._

object Helpers {
  val lower = new Helper[Json] {
    val name = "lower"

    def apply(params: HelperParameters[Json])(implicit
        renderable: Renderable[Json]
    ): Either[HelperError, String] = {

      params.params.head._2 match {
        case Complex(value) =>
          value.asString
            .toRight(HelperError("lower helper was passed a non-string type"))
            .map(_.toLowerCase())
        case StringLiteral(value) => value.toLowerCase().asRight
      }
    }

  }

  val urlEncode = new Helper[Json] {

    val name = "urlEncode"

    def apply(params: HelperParameters[Json])(implicit
        renderable: Renderable[Json]
    ): Either[HelperError, String] = {

      val urlEncodeString = (string: String) =>
        java.net.URLEncoder.encode(string, "UTF-8")

      params.params.head._2 match {
        case Complex(value) =>
          value.asString
            .toRight(
              HelperError("urlEncode helper was passed a non-string type")
            )
            .map(urlEncodeString)
        case StringLiteral(value) => urlEncodeString(value).asRight
      }
    }
  }
}
