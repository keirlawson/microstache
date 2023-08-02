package microstache

import cats.data.NonEmptyList
import cats.syntax.all._

trait Renderer[A] {
  def render(template: Template, hash: A): Either[ResolutionError, String]
}

object Renderer {
  //FIXME name should be self packaged with helpers, not up to user
  def apply[A, B](helpers: Map[String, Helper[B]] = Map.empty[String, Helper[B]])(implicit
      resolver: ValueResolver[A, B],
      renderable: Renderable[B]
  ): Renderer[A] = {
    new Renderer[A] {
      def render(
          template: Template,
          hash: A
      ): Either[ResolutionError, String] = {
        import Ast._

        def resolveIdentifier(segments: List[String]) = {
          val res = resolver.resolve(hash, segments)
            res.toRight(ResolutionError())
        }

        val strs = template.contents.traverse {
          case Text(value) => value.asRight[ResolutionError]
          case Identifier(segments) => resolveIdentifier(segments.toList).map(renderable.render)
          case HelperInvocation(name, identifier) => {
            val helper = helpers.get(name).toRight(ResolutionError())
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
