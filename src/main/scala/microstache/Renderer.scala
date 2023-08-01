package microstache

import cats.data.NonEmptyList
import cats.syntax.all._

trait Renderer[A] {
  def render(template: Template, hash: A): Either[ResolutionError, String]
}

object Renderer {
  def apply[A, B](implicit
      resolver: ValueResolver[A, B],
      renderable: Renderable[B]
  ): Renderer[A] = {
    new Renderer[A] {
      def render(
          template: Template,
          hash: A
      ): Either[ResolutionError, String] = {
        import Ast._
        val strs = template.contents.traverse {
          case Text(value) => value.asRight[ResolutionError]
          case Expression(Ast.Identifier(segments)) => {
            val res = resolver.resolve(hash, segments.toList)
            res.map(renderable.render).toRight(ResolutionError())
          }
        }

        strs.map(_.mkString)
      }
    }
  }
}
