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

  test("template with helper renders") {
    val template = "foo{{lower bar}}baz{{qux}}"
    val parsed = Parser.parser.parseAll(template).toOption.get

    val hash =
      Json.obj("bar" -> Json.fromString("BUZ"), "qux" -> Json.fromString("Quz"))

    import microstache.Circe._

    val renderer = Renderer[Json, Json](List(Helpers.lower))

    val result = renderer.render(parsed, hash).toOption.get

    assertEquals(result, "foobuzbazQuz")
  }

  test("template with urlEncode helper renders") {
    val template = "{{urlEncode foo}}"
    val parsed = Parser.parser.parseAll(template).toOption.get

    val hash =
      Json.obj("foo" -> Json.fromString("bar baz"))

    import microstache.Circe._

    val renderer = Renderer[Json, Json](List(Helpers.urlEncode))

    val result = renderer.render(parsed, hash).toOption.get

    assertEquals(result, "bar+baz")
  }

  test("template with json helper renders") {
    val template = "{{json foo}}"
    val parsed = Parser.parser.parseAll(template).toOption.get

    val hash =
      Json.obj("foo" -> Json.obj("bar" -> Json.fromString("baz")))

    import microstache.Circe._

    val renderer = Renderer[Json, Json](List(Helpers.json()))

    val result = renderer.render(parsed, hash).toOption.get

    assertEquals(result, "{\"bar\":\"baz\"}")
  }

  test("template with json helper with exclusions renders") {
    val template = "{{json foo exclude=\"bar\"}}"
    val parsed = Parser.parser.parseAll(template).toOption.get

    val hash =
      Json.obj(
        "foo" -> Json.obj(
          "bar" -> Json.fromString("baz"),
          "qux" -> Json.fromString("quz")
        )
      )

    import microstache.Circe._

    val renderer = Renderer[Json, Json](List(Helpers.json()))

    val result = renderer.render(parsed, hash).toOption.get

    assertEquals(result, "{\"qux\":\"quz\"}")
  }
}
