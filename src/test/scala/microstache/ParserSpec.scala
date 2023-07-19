package microstache

import Parser.parser
import cats.syntax.all._
import cats.parse.Parser.Error
import cats.data.NonEmptyList

class HelloSpec extends munit.FunSuite {

  def testTemplate(description: String, template: String, expected: Ast.Template) = {
    test(description) {
      assertEquals(parser.parse(template), ("", expected).asRight[Error])
    }
  }

  testTemplate("parses expression between two texts", "foo{{bar}}baz", Ast.Template(List(
      Ast.Text("foo"),
      Ast.Expression(Ast.Identifier(NonEmptyList.of("bar"))),
      Ast.Text("baz")
    )))
  testTemplate("parses expression at the beginning", "{{bar}}baz", Ast.Template(List(
      Ast.Expression(Ast.Identifier(NonEmptyList.of("bar"))),
      Ast.Text("baz")
    )))
  testTemplate("parses expression at the end", "foo{{bar}}", Ast.Template(List(
      Ast.Text("foo"),
      Ast.Expression(Ast.Identifier(NonEmptyList.of("bar"))),
    )))
  testTemplate("parses expression with no text", "{{bar}}", Ast.Template(List(
      Ast.Expression(Ast.Identifier(NonEmptyList.of("bar"))),
    )))

  testTemplate("parses expression with spaces and other punctuation", " f0o! {{bar}} b4z! ", Ast.Template(List(
      Ast.Text(" f0o! "),
      Ast.Expression(Ast.Identifier(NonEmptyList.of("bar"))),
      Ast.Text(" b4z! ")
    )))

  testTemplate("parses template with multiple expressions", "foo{{bar}}baz{{qux}}", Ast.Template(List(
      Ast.Text("foo"),
      Ast.Expression(Ast.Identifier(NonEmptyList.of("bar"))),
      Ast.Text("baz"),
      Ast.Expression(Ast.Identifier(NonEmptyList.of("qux"))),
    )))

    testTemplate("parses expression surrounded by whitespace", "foo{{ bar  }}baz", Ast.Template(List(
      Ast.Text("foo"),
      Ast.Expression(Ast.Identifier(NonEmptyList.of("bar"))),
      Ast.Text("baz")
    )))

    testTemplate("parses identifier with multiple segments", "{{ foo.bar.baz }}", Ast.Template(List(
      Ast.Expression(Ast.Identifier(NonEmptyList.of("foo", "bar", "baz"))),
    )))

    // testTemplate("parses text with no expression", "foo", Ast.Template(List(
    //   Ast.Text("foo")
    // )))
}
