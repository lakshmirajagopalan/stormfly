package stormfly

import javax.script.{Compilable, ScriptEngineManager}

import org.jsoup.select.Elements

import scala.collection.JavaConversions._
import scala.collection.immutable.HashMap

sealed trait DSL {
  def parse(root: Elements, context: Elements)(implicit bindings: Bindings): ParseResult
}
case class Select(selector: CSSSelector, child: DSL) extends DSL {
  override def parse(root: Elements, context: Elements)(implicit bindings: Bindings) = child.parse(root, selector.findIn(root))
}

case class Extract(extract: Extraction, assignment: Assignment, transformers:List[Transformer] = List.empty) extends DSL {
  override def parse(root: Elements, context: Elements)(implicit bindings: Bindings) = {
    assignment.assign(Transformer.compose(transformers).transform(extract.extractFrom(context)), bindings)
  }
}

case class Relative(selector: CSSSelector, child: DSL) extends DSL {
  override def parse(root: Elements, context: Elements)(implicit bindings: Bindings) = child.parse(root, selector.findIn(context))
}

case class Block(children: List[DSL]) extends DSL {
  override def parse(root: Elements, context: Elements)(implicit bindings: Bindings) =
    children.foldLeft(Undefined:ParseResult)({case (soFar, child) => soFar + child.parse(root, context)})
}

case class ForEach(child: DSL, assignment: Assignment, transformers: List[Transformer] = List.empty) extends DSL {
  override def parse(root: Elements, context: Elements)(implicit bindings: Bindings) = {
    assignment.assign(Transformer.compose(transformers).transform(context.foldLeft(Arr.empty)((soFar,elem) => soFar.add (child.parse(root, new Elements(elem)))).value), bindings)
  }
}

case class Iterate(on: Property, child: DSL, assignment: Assignment) extends DSL {
  override def parse(root: Elements, context: Elements)(implicit bindings: Bindings): ParseResult = on.value(bindings) match {
    case iterable: Traversable[AnyRef] => assignment.assign(Arr(iterable.map({ elem => bindings.put("_"+on.name, elem)
      child.parse(root, context)(bindings).value}).toVector), bindings)
    case e => throw new Exception("Iterating on non traversable " + e)
  }
}

case class Set[T](property: Property, assignment: Assignment) extends DSL {
  override def parse(root: Elements, context: Elements)(implicit bindings: Bindings) = assignment.assign(property.value(bindings), bindings)
}

case class Eval(code: String, assignment: Assignment) extends DSL {
  //lazy val compiled = Eval.engine.compile(s"var result = {}; (function(result){$code})(result); return result;")
  lazy val compiled = Eval.engine.compile(s"(function(){return $code})();")

  override def parse(root: Elements, context: Elements)(implicit bindings: Bindings) =
    assignment.assign(compiled.eval(bindings.bindings), bindings)
}


case class Conditional(cond: Condition, child: DSL) extends DSL {
  override def parse(root: Elements, context: Elements)(implicit bindings: Bindings) =
    if(cond.evaluate(root, context))
      child.parse(root, context)
    else Undefined
}

sealed trait Property{
  def value(bindings: Bindings):AnyRef
  def name: String
}
case class Variable(varName: String) extends Property {
  def value(bindings: Bindings) = bindings.get(name)
  def name = varName
}
case class Value(value: AnyRef) extends Property {
  def name = ""
  def value(bindings: Bindings):AnyRef = value
}

object Eval {
  lazy val engine = new ScriptEngineManager().getEngineByName("nashorn").asInstanceOf[Compilable]
}




