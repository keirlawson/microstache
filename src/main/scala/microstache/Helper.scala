package microstache

//FIXME support block helpers
//FIXME support hash params
//FIXME lambdas should be able to error

case class HelperParameters[A](params: Map[Int, A], hash: Map[String, A])

trait Helper[A] {

  val name: String

  def apply(params: HelperParameters[A])(implicit renderable: Renderable[A]): String
}