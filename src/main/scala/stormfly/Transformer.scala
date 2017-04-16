package stormfly
import java.net.URI

trait Transformer extends PartialFunction[AnyRef, AnyRef] {
    def transform(value: AnyRef) = if(value != null && isDefinedAt(value)) apply(value) else null
}

object Transformer {
  def apply(fn: PartialFunction[AnyRef,AnyRef]) = new Transformer {
    override def isDefinedAt(x: AnyRef): Boolean = fn.isDefinedAt(x)
    override def apply(v1: AnyRef): AnyRef = fn(v1)
  }

  def compose(transformers:List[Transformer]) = Transformer({case value => transformers.foldLeft(value)((sofar, fn) => fn(sofar))})
}

trait TransformerFactory {
  def name:String
  def apply(args:List[AnyRef]):Transformer
}

object TransformerFactory {
  def apply(nameOfTransformer:String, function: PartialFunction[List[AnyRef], Transformer]) = new TransformerFactory {
    override def name: String = nameOfTransformer
    override def apply(args: List[AnyRef]): Transformer = if(function.isDefinedAt(args)) function(args) else throw new RuntimeException("Arity doesnt match")
  }

  val REGEX = TransformerFactory("REGEX",{
    case List(regex:String) => val REGEX = regex.r
      Transformer{
        case value:String => REGEX.findFirstMatchIn(value.trim).map(_.group(1)).getOrElse("")
      }
  })

  val CLEANPRICE = TransformerFactory("CLEANPRICE", {
    case attr:List[AnyRef] if attr.isEmpty => val REGEX = "(\\d*\\.\\d+)".r
      Transformer{
        case value:String => REGEX.findFirstIn(value.trim).map(new java.lang.Double(_)).getOrElse(new java.lang.Double(0.0))
      }})

  val RESOLVE = TransformerFactory("RESOLVE", {
    case List(baseUrl: String) => Transformer{
      case url: String =>URI.create(baseUrl).normalize().resolve(url).normalize().toString
    }
  })

  val DROPONE = TransformerFactory("DROPONE", {
    case attr:List[AnyRef] if attr.isEmpty  => Transformer{case value: Traversable[AnyRef] => if(value.nonEmpty) value.tail else Iterable.empty}
  })

  val DROPIFNOTLONER = TransformerFactory("DROPIFNOTLONER", {
    case attr:List[AnyRef] if attr.isEmpty  => Transformer{case value: Traversable[AnyRef] => if(value.size > 1) value.tail else value}
  })

  val ATLEASTONCE = TransformerFactory("ATLEASTONCE", {
    case attr:List[AnyRef] if attr.isEmpty => Transformer{case value: Traversable[AnyRef] => if(value.isEmpty) List(null) else value}
  })

  val TABLEEX = TransformerFactory("TABLEEX", {
    case attr: List[AnyRef] if attr.isEmpty => Transformer{case value: String =>
      var text = value.split("\n")

      val kvs: Array[Array[String]] = text
      .filterNot({line => val trimmed = line.trim
            trimmed=="" || trimmed == "\"" })
        .map(_.split(":"))

      kvs match {
        case arr if arr.isEmpty => Map[String, String]()
        case arr if arr.head.length == 2 => kvs.foldLeft(Map[String, String]()){(soFar, keyValue) => soFar ++ Map(keyValue(0).trim -> keyValue(1).trim)}
        case arr if arr.length%2 ==0  => val (keyList, valueList) = kvs.map(_.head).zipWithIndex.partition({case(key, index) => index % 2 == 0})
          keyList.zip(valueList).map(kvWithIndex => kvWithIndex._1._1.trim -> kvWithIndex._2._1.trim ).toMap
        case arr => arr.head.toList
      }
    }
  })
}

