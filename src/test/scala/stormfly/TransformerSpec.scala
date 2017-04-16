package stormfly

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import java.lang.Double

import TransformerFactory._
class TransformerSpec  extends FlatSpec {
  "CLEAN transformer" should "extract double value" in {
    CLEANPRICE(List.empty)("12.345") should be (new Double(12.345))
  }

  "CLEAN transformer" should "extract double value from price string" in {
    CLEANPRICE(List.empty)("$12.345") should be (new Double(12.345))
  }

  "RESOLVE transformer" should "resolve a url with the baseUrl" in {
    RESOLVE(List("http://www.foo.com"))("/bar.html") should be ("http://www.foo.com/bar.html")
    RESOLVE(List("http://www.foo.com"))("http://cdn.foo.com/bar.html") should be ("http://cdn.foo.com/bar.html")
  }

  "REGEX transformer" should "apply regex" in {
    REGEX(List("(.*) Lumia"))("Nokia Lumia") should be ("Nokia")
    REGEX(List("unmatched"))("Nokia Lumia") should be ("")
  }

  "REGEX transformer" should "capture regex" in {
    REGEX(List("Nokia (.*)"))("Nokia Lumia") should be ("Lumia")
  }

  "DROPONE transformer" should "drop head from iterable" in {
    DROPONE(List.empty)(Iterable(1, 2, 3)) should be (Iterable(2, 3))
    DROPONE(List.empty)(Iterable.empty) should be (Iterable.empty)
    DROPONE(List.empty)(Iterable(1)) should be (Iterable())
  }

  "ATLEASTONCE transformer" should "return a list with atleast one element" in {
    ATLEASTONCE(List.empty)(Iterable(1, 2, 3)) should be (Iterable(1, 2, 3))
    ATLEASTONCE(List.empty)(Iterable.empty) should be (Iterable(null))
  }

  "TABLEEX transformer" should "extract a table from string" in {
    TABLEEX(List.empty)("\n\n\nSeries:\n\n\nM12\n\n\nCordless:\n\n\nYes\n\n\n") should be (Map("Series" -> "M12", "Cordless" -> "Yes"))
    TABLEEX(List.empty)("\nSeries:\t\t\tM12\nCordless:Yes\n\n\n") should be (Map("Series" -> "M12", "Cordless" -> "Yes"))
  }
}
