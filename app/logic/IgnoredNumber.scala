package logic

object IgnoredNumber extends Enumeration {
  type IgnoredNumber = Value
  val NONE, SINGULAR, PLURAL = Value
  
  implicit def toString(in: IgnoredNumber.Value) = in.toString()
  
  implicit def parse(str: String) = str.toLowerCase() match {
    case "none" => NONE
    case "singular" => SINGULAR
    case "plural" => PLURAL
    case _ => throw new IllegalArgumentException("NounPair.ignoredNumber, invalid 'ignored' value: " + str)
  }
}