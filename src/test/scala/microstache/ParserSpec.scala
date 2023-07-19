package microstache

import Parser.parser
import cats.syntax.all._
import cats.parse.Parser.Error

class HelloSpec extends munit.FunSuite {
  test("parses expression between two texts") {
    assertEquals(parser.parse("foo{{bar}}baz"), ("", Ast.Template(List(
      Ast.Text("foo"),
      Ast.Expression("bar"),
      Ast.Text("baz")
    ))).asRight[Error])
  }
}
