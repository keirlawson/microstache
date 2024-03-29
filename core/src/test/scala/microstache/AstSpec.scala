package microstache

import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._
import cats.syntax.all._
import cats.data.NonEmptyList

class AstSpec extends ScalaCheckSuite {

  def nonEmptyStr(gen: Gen[Char]): Gen[String] =
    Gen.posNum[Int].flatMap(i => Gen.stringOfN(i, gen))

  val genIdentifier: Gen[Ast.Identifier] = Gen
    .listOf(nonEmptyStr(Gen.alphaChar))
    .map(l => Ast.Identifier(l))

  val genValue: Gen[Ast.Value] = Gen.oneOf(
    genIdentifier,
    nonEmptyStr(Gen.alphaNumChar).map(Ast.StringLiteral)
  )

  val genText: Gen[Ast.Text] = nonEmptyStr(Gen.alphaNumChar).map(Ast.Text)

  val genNamedParam: Gen[(String, Ast.Value)] = for {
    k <- nonEmptyStr(Gen.alphaChar)
    v <- genValue
  } yield (k, v)

  val genHelper: Gen[Ast.HelperInvocation] = for {
    name <- nonEmptyStr(Gen.alphaChar)
    params <- Gen.nonEmptyListOf(genValue)
    namedParams <- Gen.mapOf(genNamedParam)
  } yield {
    Ast.HelperInvocation(name, NonEmptyList.fromListUnsafe(params), namedParams)
  }

  val genTerm: Gen[Ast.Term] = Gen.oneOf(genText, genIdentifier, genHelper)

  val genTemplate: Gen[Template] = Gen
    .listOf(genTerm)
    .map { lst =>
      lst.sliding2.flatMap {
        case (Ast.Text(_), Ast.Text(_)) => None
        case x @ _                      => Some(x._1)
      }
    }
    .map(Template.apply)

  property("show/parse round trips") {
    forAll(genTemplate) { template =>
      Parser.parser.parseAll(template.show) == template.asRight
    }

  }

}
