package stormfly

import org.jsoup.select.Elements

sealed trait Condition {
  def evaluate(root: Elements, context: Elements)(implicit bindings: Bindings): Boolean
}
case class Eq(lhs: DSL, rhs: DSL) extends Condition {
  override def evaluate(root: Elements, context: Elements)(implicit bindings: Bindings): Boolean = lhs.parse(root, context) == rhs.parse(root, context)
}

case class Exists(child: DSL) extends Condition {
  override def evaluate(root: Elements, context: Elements)(implicit bindings: Bindings): Boolean = child.parse(root, context) != Undefined
}
