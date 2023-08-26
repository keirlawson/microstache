package microstache

import Parser.parser
import cats.syntax.all._
import cats.parse.Parser.Error
import cats.data.NonEmptyList

class ParserSpec extends munit.FunSuite {

  def testTemplate(
      description: String,
      template: String,
      expected: Template
  ) = {
    test(description) {
      val result = parser.parseAll(template)
      assertEquals(
        result,
        expected.asRight[Error],
        result.left.toOption.map(_.show).getOrElse("values are not the same")
      )
    }
  }

  testTemplate(
    "parses expression between two texts",
    "foo{{bar}}baz",
    Template(
      List(
        Ast.Text("foo"),
        Ast.Identifier(List("bar")),
        Ast.Text("baz")
      )
    )
  )
  testTemplate(
    "parses expression at the beginning",
    "{{bar}}baz",
    Template(
      List(
        Ast.Identifier(List("bar")),
        Ast.Text("baz")
      )
    )
  )
  testTemplate(
    "parses expression at the end",
    "foo{{bar}}",
    Template(
      List(
        Ast.Text("foo"),
        Ast.Identifier(List("bar"))
      )
    )
  )
  testTemplate(
    "parses expression with no text",
    "{{bar}}",
    Template(
      List(
        Ast.Identifier(List("bar"))
      )
    )
  )

  testTemplate(
    "parses expression with spaces and other punctuation",
    " f0o! {{bar}} b4z! ",
    Template(
      List(
        Ast.Text(" f0o! "),
        Ast.Identifier(List("bar")),
        Ast.Text(" b4z! ")
      )
    )
  )

  testTemplate(
    "parses template with multiple expressions",
    "foo{{bar}}baz{{qux}}",
    Template(
      List(
        Ast.Text("foo"),
        Ast.Identifier(List("bar")),
        Ast.Text("baz"),
        Ast.Identifier(List("qux"))
      )
    )
  )

  testTemplate(
    "parses expression surrounded by whitespace",
    "foo{{ bar  }}baz",
    Template(
      List(
        Ast.Text("foo"),
        Ast.Identifier(List("bar")),
        Ast.Text("baz")
      )
    )
  )

  testTemplate(
    "parses identifier with multiple segments",
    "{{ foo.bar.baz }}",
    Template(
      List(
        Ast.Identifier(List("foo", "bar", "baz"))
      )
    )
  )

  testTemplate(
    "parses identifier for root of hash",
    "{{ . }}",
    Template(
      List(
        Ast.Identifier(List.empty)
      )
    )
  )

  testTemplate(
    "parses text with no expression",
    "foo",
    Template(
      List(
        Ast.Text("foo")
      )
    )
  )

  testTemplate("parses empty string", "", Template(List.empty))

  testTemplate(
    "parses whitespace only string",
    " ",
    Template(
      List(
        Ast.Text(" ")
      )
    )
  )

  testTemplate(
    "parses helper invocation",
    "{{foo bar.baz}}",
    Template(
      List(
        Ast.HelperInvocation(
          "foo",
          NonEmptyList.of(Ast.Identifier(List("bar", "baz"))),
          Map.empty
        )
      )
    )
  )

  testTemplate(
    "parses helper with multiple args",
    "{{foo bar.baz qux}}",
    Template(
      List(
        Ast.HelperInvocation(
          "foo",
          NonEmptyList.of(
            Ast.Identifier(List("bar", "baz")),
            Ast.Identifier(List("qux"))
          ),
          Map.empty
        )
      )
    )
  )

  testTemplate(
    "parses helper with single named arg",
    "{{foo bar baz=qux}}",
    Template(
      List(
        Ast.HelperInvocation(
          "foo",
          NonEmptyList.of(Ast.Identifier(List("bar"))),
          Map("baz" -> Ast.Identifier(List("qux")))
        )
      )
    )
  )

  testTemplate(
    "parses helper with multiple named args",
    "{{foo bar baz=qux x=y}}",
    Template(
      List(
        Ast.HelperInvocation(
          "foo",
          NonEmptyList.of(Ast.Identifier(List("bar"))),
          Map(
            "baz" -> Ast.Identifier(List("qux")),
            "x" -> Ast.Identifier(List("y"))
          )
        )
      )
    )
  )

  testTemplate(
    "parses helper with multiple named args and multiple whitespace",
    "{{foo   bar  baz=qux  x=y    }}",
    Template(
      List(
        Ast.HelperInvocation(
          "foo",
          NonEmptyList.of(Ast.Identifier(List("bar"))),
          Map(
            "baz" -> Ast.Identifier(List("qux")),
            "x" -> Ast.Identifier(List("y"))
          )
        )
      )
    )
  )

  testTemplate(
    "parses helper with string literal arguments",
    "{{foo \"bar\" baz=\"qux\"}}",
    Template(
      List(
        Ast.HelperInvocation(
          "foo",
          NonEmptyList.of(Ast.StringLiteral("bar")),
          Map("baz" -> Ast.StringLiteral("qux"))
        )
      )
    )
  )

  testTemplate(
    "parses block helper",
    "{{#foo bar}}baz{{/foo}}",
    Template(
      List(
        Ast.BlockHelperInvocation(
          "foo",
          NonEmptyList.of(Ast.Identifier(List("bar"))),
          Map.empty,
          List(Ast.Text("baz"))
        )
      )
    )
  )
}
