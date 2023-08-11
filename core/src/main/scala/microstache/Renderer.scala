package microstache

import cats.syntax.all._
import cats.data.NonEmptyList

trait Renderer[A] {
  def render(template: Template, hash: A): Either[ResolutionError, String]
}

//FIXME this needs tests
object Renderer {
  def apply[A, B](helpers: List[Helper[B]] = List.empty[Helper[B]])(implicit
      resolver: ValueResolver[A, B],
      renderable: Renderable[B]
  ): Renderer[A] = {

    val helperLookup = helpers.map(helper => (helper.name -> helper)).toMap

    def resolveIdentifier(segments: List[String], hash: A) = {
      val res = resolver.resolve(hash, segments)
      res.toRight(
        ResolutionError(
          s"Unable to resolve path ${segments.mkString(".")} against supplied hash"
        )
      )
    }

    def resolveValue(
        value: Ast.Value,
        hash: A
    ): Either[ResolutionError, Value[B]] = {
      value match {
        case id: Ast.Identifier =>
          resolveIdentifier(id.segments.toList, hash).map(Complex(_))
        case Ast.StringLiteral(value) =>
          StringLiteral(value).asRight[ResolutionError]
      }
    }

    def invokeHelper(
        h: Helper[B],
        params: NonEmptyList[Ast.Value],
        namedParams: Map[String, Ast.Value],
        hash: A
    ): Either[ResolutionError, String] = {
      // FIXME map value to value here
      val res =
        params.traverse(p => resolveValue(p, hash))
      val resolvedNamed = namedParams.toList.traverse { case (k, v) =>
        resolveValue(v, hash).map((k, _))
      }
      (res, resolvedNamed)
        .mapN { case (ps, nps) =>
          HelperParameters[B](
            ps.zipWithIndex
              .map(pair => (pair._2, pair._1))
              .toList
              .toMap,
            nps.toMap
          )
        }
        .map(h.apply)
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
            resolveIdentifier(segments.toList, hash).map(renderable.render)
          case HelperInvocation(name, params, namedParams) => {
            val helper = helperLookup
              .get(name)
              .toRight(
                ResolutionError(s"helper with name ${name} not configured")
              )
            helper.flatMap { h =>
              invokeHelper(h, params, namedParams, hash)
            }
          }
        }

        strs.map(_.mkString)
      }
    }
  }
}
