package logic

object HardSoftMode extends Enumeration {
  type PLMode = Value;
  val HARD, SOFT = Value;
  
  implicit def toString(v: HardSoftMode.Value) = v.toString()
  
  implicit def parse(str: String): HardSoftMode.Value = str.toLowerCase() match {
    case "hard" => HARD
    case "soft" => SOFT
    case _ => throw new IllegalArgumentException("Unrecognized adjective mode: " + str)
  }
}
