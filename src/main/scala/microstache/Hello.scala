package microstache

import cats.effect.IOApp
import cats.effect.IO
import cats.data.NonEmptyList

object Hello extends IOApp.Simple {
  val template = "foo{{bar}}baz{{qux}}"
  
  val result = Parser.parser.parseAll(template).toOption.get

  implicit val mapResolver: ValueResolver[Map[String, String]] = new ValueResolver[Map[String, String]] {
    def resolve(hash: Map[String, String], path: NonEmptyList[String]): Option[String] = {
      path.toList match {
        case Nil => None
        case head :: Nil => hash.get(head)
        case head :: tail => None
      }
    }
  }


  val run = IO.println(Renderer[Map[String, String]].render(result, Map("bar" -> "BAR", "qux" -> "QUX")))
}

