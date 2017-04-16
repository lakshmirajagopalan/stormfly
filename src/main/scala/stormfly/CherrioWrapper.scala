package stormfly

import org.jsoup.select.Elements


class CherrioWrapper(val elements:Elements) {
  def html() = elements.html()
  def text() = elements.text()

  def func(selector: String) = {
    new CherrioWrapper(elements.select(selector))
  }
}
