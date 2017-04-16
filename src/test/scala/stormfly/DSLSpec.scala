package stormfly

import java.util

import org.hamcrest.{CoreMatchers, Matcher, BaseMatcher, Description}
import org.jsoup.nodes.Element
import org.jsoup.select.Elements
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.{BeforeAndAfterEach, FlatSpec}
import org.scalatest.Matchers._
import org.mockito.Matchers.{eq => equal}
import scala.collection.JavaConversions._
class DSLSpec extends FlatSpec with BeforeAndAfterEach {
  val context: Elements = mock(classOf[Elements])
  val root: Elements = mock(classOf[Elements])
  implicit val bindings = mock(classOf[Bindings])

  override def beforeEach: Unit = {
    reset(bindings)
  }


  "Extract" should "do extraction and return ParseResult of key-value pairs in case of As" in {
    val extraction: Extraction = mock(classOf[Extraction])
    val extract = Extract(extraction, As("title"))
    when(extraction.extractFrom(context)).thenReturn("Nokia Lumia")

    extract.parse(root, context) should be (ParseResult("title" -> "Nokia Lumia"))
  }

  "Extract" should "do extraction and return updated bindings in case of To" in {
    val extraction: Extraction = mock(classOf[Extraction])
    val extract = Extract(extraction, To("title"))
    when(extraction.extractFrom(context)).thenReturn("Nokia Lumia")

    extract.parse(root, context) should be (Undefined)
    verify(bindings, times(1)).put("title", "Nokia Lumia")
  }

  "Extract" should "apply the given transformers " in {
    val extraction: Extraction = mock(classOf[Extraction])
    val extract = Extract(extraction, To("title"), List(TransformerFactory.REGEX(List("Nokia (.*)"))))
    when(extraction.extractFrom(context)).thenReturn("Nokia Lumia")

    extract.parse(root, context) should be (Undefined)
    verify(bindings, times(1)).put("title", "Lumia")
  }

  "Select" should "find elements in root matching selector and pass it as context to its children" in {
    val child: DSL = mock(classOf[DSL])
    val selector: CSSSelector = mock(classOf[CSSSelector])
    val select = Select(selector, child)
    val newContext: Elements = mock(classOf[Elements])
    when(child.parse(root, newContext)).thenReturn(ParseResult("size" -> new Integer(10)))
    when(selector.findIn(root)).thenReturn(newContext)

    select.parse(root, context) should be (ParseResult("size" -> new Integer(10)))
  }

  "Relative" should "apply selector on context and pass it as context to its children" in {
    val child: DSL = mock(classOf[DSL])
    val selector: CSSSelector = mock(classOf[CSSSelector])
    val relative = Relative(selector, child)
    val newContext: Elements = mock(classOf[Elements])
    when(child.parse(root, newContext)).thenReturn(ParseResult("size" -> new Integer(10)))
    when(selector.findIn(context)).thenReturn(newContext)

    relative.parse(root, context) should be (ParseResult("size" -> new Integer(10)))
  }

  "Block" should "parse its children and accumulate into the ParseResult" in {
    val child1 = mock(classOf[DSL])
    val child2: DSL = mock(classOf[DSL])
    val block = Block(List(child1, child2))
    when(child1.parse(root, context)).thenReturn(ParseResult("size" -> new Integer(10)))
    when(child2.parse(root, context)).thenReturn(ParseResult("title" -> "Nokia Lumia"))

    block.parse(root, context) should be (ParseResult("size" -> new Integer(10), "title" -> "Nokia Lumia"))
  }

  "ForEach" should "pass the DSL to the elements matched on select" in {
    val element1: Element = mock(classOf[Element])
    val element2: Element = mock(classOf[Element])
    val context = new Elements(element1, element2)
    val child: DSL = mock(classOf[DSL])
    val forEach = ForEach(child, As("additionalAttributes"))

    when(child.parse(equal(root), argThat(withElement(element1)))(equal(bindings))).thenReturn(ParseResult("Release-Datum" -> "20.05.2013"))
    when(child.parse(equal(root), argThat(withElement(element2)))(equal(bindings))).thenReturn(ParseResult("Artikelnummer" -> new Integer(435679)))

    forEach.parse(root, context) should be (ParseResult("additionalAttributes" -> Vector(Map("Release-Datum" -> "20.05.2013"), Map("Artikelnummer" -> new Integer(435679)))))
  }

  "Set" should "set the value in the ParseResult in case of As" in {
    val set = Set(Value("123"), As("brandId"))
    set.parse(root, context) should be (ParseResult("brandId" -> "123"))
  }

  "Set" should "set the value in the Bindings in case of To" in {
    val set = Set(Value("123"), To("brandId"))
    set.parse(root, context) should be (Undefined)

    verify(bindings).put("brandId", "123")
    verifyNoMoreInteractions(bindings)
  }

  "Set" should "read value from bindings and set the value in the ParseResult in case of As" in {
    val set = Set(Variable("brand"), As("brandId"))
    implicit val bindings = Bindings("brand" -> 123)
    set.parse(root, context) should be (ParseResult("brandId" -> new Integer(123)))
  }

  "Set" should "read value from bindings and set the value in the Bindings in case of To" in {
    val set = Set(Variable("brand"), To("brandId"))
    implicit val bindings = Bindings("brand" -> 123)
    set.parse(root, context) should be (Undefined)

    bindings.bindings.size() should be (2)
    bindings.bindings.get("brand") should be (123)
    bindings.bindings.get("brandId") should be (123)
  }

  "Iterate" should "iterate on a value and apply child DSL and set in result in case of As" in {
    implicit val bindings = Bindings() //TODO: Verify
    val iterate = Iterate(Value(List(1, 2, 3)), Set(Variable("_"), As("sku")), As("variants"))
    iterate.parse(root, context) should be (ParseResult("variants" -> (Arr(Vector(Map("sku" -> 1), Map("sku" -> 2), Map("sku" -> 3))))))
  }

  "Iterate" should "iterate on a value and apply child DSL and update bindings in case of To" in {
    implicit val bindings = Bindings()
    val iterate = Iterate(Value(List(1, 2, 3)), Set(Variable("_"), As("sku")), To("variants"))
    iterate.parse(root, context) should be (Undefined)
    bindings.bindings.size() should be (2)
    bindings.bindings.get("variants") should be (Arr(Vector(Map("sku" -> 1), Map("sku" -> 2), Map("sku" -> 3))))
    bindings.bindings.get("_") should be (new Integer(3))
  }

  "Iterate" should "iterate on a variable and apply child DSL" in {
    implicit val bindings = Bindings("skus" -> List("s123", "s234", "s345"))
    val iterate = Iterate(Variable("skus"), Set(Variable("_skus"), As("sku")), As("variants"))
    iterate.parse(root, context) should be (ParseResult("variants" -> (Arr(Vector(Map("sku" -> "s123"), Map("sku" -> "s234"), Map("sku" -> "s345"))))))
  }

  "Eval" should "evaluate the given javascript code and set return in ParseResult in case of As" in {
    implicit val bindings = new Bindings()
    val eval = Eval("(1+2);  ", As("minSalePrice"))
    eval.parse(root, context) should be (ParseResult("minSalePrice" -> new Integer(3)))
  }

  "Eval"  should "evaluate the given javascript code and set return in Bindings in case of To" in {
    implicit val bindings = new Bindings()
    val eval = Eval("(1+2);  ", To("minSalePrice"))
    eval.parse(root, context) should be (Undefined)
    bindings.get("minSalePrice") should be(new Integer(3))
  }

  "Eval"  should "evaluate the given javascript code should not set local variables in bindings" in {
    implicit val bindings = new Bindings()
    val eval = Eval("function(){var a = 10; return a;}(); ", To("minSalePrice"))
    eval.parse(root, context) should be (Undefined)
    bindings.get("minSalePrice" ) should be(new Integer(10))
  }

  "Eval" should "take values from bindings and return computed value" in {
    implicit val bindings = new Bindings()
    bindings.put("minListPrice", new Integer(10))
    val eval = Eval("minListPrice - 2", To("minSalePrice"))
    eval.parse(root, context) should be (Undefined)
    bindings.get("minSalePrice") should be(8)
  }


  "Conditional" should "execute the branch if condition evaluates to true" in {
    val condTrue: Condition = mock(classOf[Condition])
    val condFalse: Condition = mock(classOf[Condition])
    val childTrue: DSL = mock(classOf[DSL])
    val childFalse: DSL = mock(classOf[DSL])
    val conditionalTrue = Conditional(condTrue, childTrue)
    val conditionalFalse = Conditional(condFalse, childFalse)

    when(condTrue.evaluate(root, context)).thenReturn(true)
    when(condFalse.evaluate(root, context)).thenReturn(false)
    when(childTrue.parse(root, context)).thenReturn(ParseResult("title" -> "Nokia Lumia"))

    conditionalTrue.parse(root, context) should be (ParseResult("title" -> "Nokia Lumia"))
    conditionalFalse.parse(root, context) should be (Undefined)
  }

  def withElement(element:Element) = {
    new BaseMatcher[Elements] {
      override def matches(item: scala.Any): Boolean = item match {
        case elem:Elements => elem.size() == 1 && elem.get(0) == element
        case _ => false
      }

      override def describeTo(description: Description): Unit = s"didnt not have it as a single element of ${element}"
    }
  }
}
