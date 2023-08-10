package microstache

//FIXME support block helpers
//FIXME lambdas should be able to error

case class HelperParameters[A](params: Map[Int, A], named: Map[String, A])

trait Helper[A] {

  val name: String

  def apply(params: HelperParameters[A])(implicit
      renderable: Renderable[A]
  ): String
}
