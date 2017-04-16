package stormfly

import javax.script.SimpleBindings

import scala.collection.JavaConversions._

sealed trait ParseResult {
  def value: AnyRef
  def merge:PartialFunction[ParseResult,ParseResult]
  def +(other:ParseResult) = {
    if(merge.isDefinedAt(other)) merge(other)
    else throw new RuntimeException(s"Unsupported merge of ${this.getClass} and ${other.getClass}")
  }
}
case class Dict(value: Map[String, AnyRef]) extends ParseResult {
  override def merge = {
    case Dict(more:Map[String,Any]) => Dict(more ++ value)
    case Undefined => this
  }
}
case class Arr(value: Vector[AnyRef]) extends ParseResult {
  override def merge = {
    case Arr(more: Vector[AnyRef]) => Arr(value ++ more)
    case Undefined => this
  }
  def add(other:ParseResult):Arr = other match {
    case Undefined => this
    case more => Arr(this.value :+ more.value)
  }
}

object Arr {
  def empty = Arr(Vector.empty)
}
case class Val(value: AnyRef) extends ParseResult {
  override def merge = Map.empty
}
case object Undefined extends ParseResult {
  override def merge = {
    case other => other
  }
  override val value = null
}

object ParseResult {
  def apply(kv: (String, AnyRef)*): ParseResult = {
    Dict(kv.filterNot(_._2 == null).toMap)
  }
}

class Bindings(var bindings: SimpleBindings = new SimpleBindings()){
  def get(key: String) = bindings.get(key)

  def put(key:String, value:AnyRef) = bindings.put(key,value)

  override def toString() = "Bindings: {\n" + bindings.toMap + "\n}"
}

object Bindings {
  def apply(kv: (String, Any)*): Bindings = {
    val simpleBindings = new SimpleBindings()
    kv.foreach({case (k, v) => simpleBindings.put(k, v)})
    new Bindings(simpleBindings)
  }
}

