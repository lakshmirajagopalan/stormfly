package stormfly

sealed trait Assignment{
  def assign(value: AnyRef, bindings: Bindings): ParseResult
}
case class To(name: String) extends Assignment {
  override def assign(value: AnyRef, bindings: Bindings): ParseResult = {
    if(value != null)
      bindings.put (name , value)
    Undefined
  }

}
case class As(name: String) extends Assignment {
  override def assign(value: AnyRef, bindings: Bindings): ParseResult =
    if(value != null || value != "") ParseResult(name -> value) else Undefined
}
case object AsIs extends Assignment {
  override def assign(value: AnyRef, bindings: Bindings): ParseResult = if(value != null) Val(value) else Undefined
}
