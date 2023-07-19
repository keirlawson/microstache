package microstache

import Parser.parser
import cats.syntax.all._
import cats.parse.Parser.Error

class HelloSpec extends munit.FunSuite {

  def testTemplate(description: String, template: String, expected: Ast.Template) = {
    test(description) {
      assertEquals(parser.parse(template), ("", expected).asRight[Error])
    }
  }

  testTemplate("parses expression between two texts", "foo{{bar}}baz", Ast.Template(List(
      Ast.Text("foo"),
      Ast.Expression("bar"),
      Ast.Text("baz")
    )))
  testTemplate("parses expression at the beginning", "{{bar}}baz", Ast.Template(List(
      Ast.Expression("bar"),
      Ast.Text("baz")
    )))
  testTemplate("parses expression at the end", "foo{{bar}}", Ast.Template(List(
      Ast.Text("foo"),
      Ast.Expression("bar")
    )))
  testTemplate("parses expression with no text", "{{bar}}", Ast.Template(List(
      Ast.Expression("bar")
    )))
    // testTemplate("parses text with no expression", "foo", Ast.Template(List(
    //   Ast.Text("foo")
    // )))
}
