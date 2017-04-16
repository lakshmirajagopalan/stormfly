package stormfly

import org.jsoup.select.Elements
import scala.collection.JavaConversions._

case class CSSSelector(selector: String) {
  def findIn(element: Elements):Elements = element.select(selector)
}

sealed trait Extraction{
  def extractFrom(element: Elements): String
}
case object Text extends Extraction {
  override def extractFrom(elements: Elements): String = if(!elements.isEmpty) elements.text(false) else null
}
case object OwnText extends Extraction {
  override def extractFrom(elements: Elements): String = elements.headOption.map(_.ownText()).getOrElse(null)
}
case class Attribute(name: String) extends Extraction {
  override def extractFrom(elements: Elements): String = if(!elements.isEmpty) elements.attr(name) else null
}