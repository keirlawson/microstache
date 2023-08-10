package microstache

import munit.FunSuite
import io.circe.Json

class CirceSpec extends FunSuite {

  def testJsonResolver(hash: Json, path: List[String], expected: Json) = {
    test(s"path of ${path.mkString(".")} corrently resolves") {
      import Circe.jsonRenderer
      assertEquals(Circe.jsonResolver.resolve(hash, path).get, expected)
    }
  }

  testJsonResolver(
    Json.obj("foo" -> Json.fromString("bar")),
    List("foo"),
    Json.fromString("bar")
  )

  testJsonResolver(
    Json.obj("foo" -> Json.obj("bar" -> Json.fromString("baz"))),
    List("foo", "bar"),
    Json.fromString("baz")
  )

  testJsonResolver(
    Json.obj("foo" -> Json.fromString("bar")),
    List.empty,
    Json.obj("foo" -> Json.fromString("bar"))
  )
}
