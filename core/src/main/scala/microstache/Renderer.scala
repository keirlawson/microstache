package microstache

import cats.syntax.all._
import cats.data.NonEmptyList

trait Renderer[A] {
  def render(template: Template, hash: A): Either[ResolutionError, String]
}

object Renderer {

  def apply[A, B](helpers: List[Helper[B]] = List.empty[Helper[B]])(implicit
      resolver: ValueResolver[A, B],
      renderable: Renderable[B]
  ): Renderer[A] = {

    def undefinedError(segments: List[String]) = ResolutionError(
      s"Unable to resolve path ${segments.mkString(".")} against supplied hash"
    )

    val helperLookup = helpers.map(helper => (helper.name -> helper)).toMap

    def resolveValue(
        value: Ast.Value,
        hash: A
    ): Value[B] = {
      value match {
        case id: Ast.Identifier =>
          Complex(
            resolver
              .resolve(hash, id.segments)
              .toRight(undefinedError(id.segments))
          )
        case Ast.StringLiteral(value) =>
          StringLiteral(value)
      }
    }

    def invokeHelper(
        helperName: String,
        params: NonEmptyList[Ast.Value],
        namedParams: Map[String, Ast.Value],
        hash: A,
        block: Option[Template]
    ): Either[ResolutionError, String] = {
      val helper = helperLookup
        .get(helperName)
        .toRight(
          ResolutionError(s"helper with name ${helperName} not configured")
        )

      helper.flatMap { h =>
        val res =
          params.map(p => resolveValue(p, hash))
        val resolvedNamed = namedParams.toList.map { case (k, v) =>
          (k, resolveValue(v, hash))
        }
        val ps = HelperParameters[B](
          res.zipWithIndex
            .map(pair => (pair._2, pair._1)),
          resolvedNamed.toMap,
          block
        )
        h.apply(ps)
          .leftMap(e =>
            ResolutionError(s"Helper execution failed: ${e.message}")
          )
      }
    }

    new Renderer[A] {
      def render(
          template: Template,
          hash: A
      ): Either[ResolutionError, String] = {
        import Ast._

        val strs = template.contents.traverse {
          case Text(value) => value.asRight[ResolutionError]
          case Identifier(segments) =>
            resolver
              .resolve(hash, segments)
              .map(renderable.render(_))
              .toRight(undefinedError(segments))
          case HelperInvocation(name, params, namedParams) => {
            invokeHelper(name, params, namedParams, hash, None)
          }
          case BlockHelperInvocation(name, params, namedParams, block) => {
            invokeHelper(name, params, namedParams, hash, Some(Template(block)))
          }
        }

        strs.map(_.mkString)
      }
    }
  }
}
