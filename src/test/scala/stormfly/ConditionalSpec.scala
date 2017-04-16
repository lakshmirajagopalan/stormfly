package stormfly

import org.jsoup.select.Elements
import org.mockito.Mockito._
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class ConditionalSpec extends FlatSpec {
  val context: Elements = mock(classOf[Elements])
  val root: Elements = mock(classOf[Elements])
  implicit val bindings = mock(classOf[Bindings])

  "Equality" should "parse lhs and test if lhs == rhs" in {
    val lhs: DSL = mock(classOf[DSL])
    val equalRhs: DSL = mock(classOf[DSL])
    when(lhs.parse(root, context)).thenReturn(ParseResult("sku" -> new Integer(123)))
    when(equalRhs.parse(root, context)).thenReturn(ParseResult("sku" -> new Integer(123)))

    Eq(lhs, equalRhs).evaluate(root, context) should be (true)
  }

  "Equality" should "return false if lhs and rhs are not equal" in {
    val lhs: DSL = mock(classOf[DSL])
    val unequalRhs: DSL = mock(classOf[DSL])
    val unequalRhs2: DSL = mock(classOf[DSL])
    when(lhs.parse(root, context)).thenReturn(ParseResult("sku" -> new Integer(123)))
    when(unequalRhs.parse(root, context)).thenReturn(Undefined)
    when(unequalRhs2.parse(root, context)).thenReturn(ParseResult("sku" -> "123"))

    Eq(lhs, unequalRhs).evaluate(root, context) should be (false)
    Eq(lhs, unequalRhs2).evaluate(root, context) should be (false)
  }

  "Exists" should "return true if DSL passed" in {
    val existsDSL: DSL = mock(classOf[DSL])
    when(existsDSL.parse(root, context)).thenReturn(ParseResult("sku" -> new Integer(123)))

    Exists(existsDSL).evaluate(root, context) should be (true)
  }

  "Exists" should "return false if DSL failed" in {
    val nonExistsDSL: DSL = mock(classOf[DSL])
    when(nonExistsDSL.parse(root, context)).thenReturn(Undefined)

    Exists(nonExistsDSL).evaluate(root, context) should be (false)
  }
}
