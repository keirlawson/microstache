package microstache

import cats.data.NonEmptyList
import cats.syntax.all._

trait ValueResolver[A] {
  def resolve(hash: A, path: NonEmptyList[String]): Option[String]
}

case class ResolutionError() extends RuntimeException

trait Renderer[A] {
  def render(template: Template, hash: A): Either[ResolutionError, String]
}

object Renderer {
  def apply[A](implicit resolver: ValueResolver[A]): Renderer[A] = {
    new Renderer[A] {
      def render(template: Template, hash: A): Either[ResolutionError, String] = {
        //FIXME microbench this
        val strs = template.contents.traverse {
          case Ast.Text(value) => value.asRight[ResolutionError]
          case Ast.Expression(identifier) => {
            resolver.resolve(hash, identifier.segments).toRight(ResolutionError())
          }
        }
        
        strs.map(_.mkString)
      }
    }
  }
}


