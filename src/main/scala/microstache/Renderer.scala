package microstache

import cats.data.NonEmptyList
import cats.syntax.all._

trait Renderer[A] {
  def render(template: Template, hash: A): Either[ResolutionError, String]
}

object Renderer {
  def apply[A, B](helpers: List[Helper[B]] = List.empty[Helper[B]])(implicit
      resolver: ValueResolver[A, B],
      renderable: Renderable[B]
  ): Renderer[A] = {

    val helperLookup = helpers.map(helper => (helper.name -> helper)).toMap

    new Renderer[A] {
      def render(
          template: Template,
          hash: A
      ): Either[ResolutionError, String] = {
        import Ast._

        def resolveIdentifier(segments: List[String]) = {
          val res = resolver.resolve(hash, segments)
            res.toRight(ResolutionError(s"Unable to resolve path ${segments.mkString(".")} against supplied hash"))
        }

        val strs = template.contents.traverse {
          case Text(value) => value.asRight[ResolutionError]
          case Identifier(segments) => resolveIdentifier(segments.toList).map(renderable.render)
          case HelperInvocation(name, identifier) => {
            val helper = helperLookup.get(name).toRight(ResolutionError(s"helper with name ${name} not configured"))
            helper.flatMap { h =>
              val res = resolveIdentifier(identifier.segments.toList)
              res.map(h.apply)
            }
          }
        }

        strs.map(_.mkString)
      }
    }
  }
}
