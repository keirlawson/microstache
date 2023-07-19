package microstache

object Ast {
  case class Template(contents: List[Term])
  sealed trait Term
  case class Text(value: String) extends Term
  case class Expression(value: String) extends Term
}