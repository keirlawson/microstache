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

  val json = new Helper[Json] {
    val name = "json"

    def apply(params: HelperParameters[Json])(implicit
        renderable: Renderable[Json]
    ): Either[HelperError, String] = {

      val exclusions = params.named
        .get("exclude")
        .traverse {
          case Complex(_) => HelperError("exclude must be a string").asLeft
          case StringLiteral(value) => value.split(",").toSet.asRight
        }
        .map(_.getOrElse(Set.empty))

      val json =
        params.params.head._2 match {
          case Complex(value) => value.asRight
          case StringLiteral(_) =>
            HelperError("json helper was passed a string literal").asLeft
        }

      (exclusions, json).mapN { case (exclusions, json) =>
        val excluded =
          json.mapObject { obj =>
            obj.filterKeys(!exclusions.contains(_))
          }

        excluded.noSpaces // FIXME make formatting configurable
      }

    }
  }
}
