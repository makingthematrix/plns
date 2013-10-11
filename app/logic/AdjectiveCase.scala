package logic

object AdjectiveGender extends Enumeration {
  type AdjectiveGender = Value
  val MASCULINE, FEMININE, NEUTER, PLURAL_MASCULINE, PLURAL_NONMASCULINE = Value
  
  implicit def toString(gender: AdjectiveGender.Value) = gender match {
    case MASCULINE => "m"
    case FEMININE => "f"
    case NEUTER => "n"
    case PLURAL_MASCULINE => "p"
    case PLURAL_NONMASCULINE => "P"
  }
  
  implicit def parse(str: String) = str match {
    case "m" => MASCULINE
    case "f" => FEMININE
    case "n" => NEUTER
    case "p" => PLURAL_MASCULINE
    case "P" => PLURAL_NONMASCULINE
    case _ => throw new IllegalArgumentException("AdjectiveCase.Gender, invalid value: " + str)
  }
  
  val singular = Set(MASCULINE, FEMININE, NEUTER)
  val plural = Set(PLURAL_MASCULINE, PLURAL_NONMASCULINE)
}

object AdjectiveDegree extends Enumeration {
  type AdjectiveDegree = Value
  val INDICATIVE, COMPARATIVE, SUPERLATIVE = Value
  
  implicit def toString(degree: AdjectiveDegree.Value) = degree match {
    case INDICATIVE => "i"
    case COMPARATIVE => "c"
    case SUPERLATIVE => "s"
  }
  
  implicit def parse(str: String) = str.toLowerCase() match {
    case "i" => INDICATIVE
    case "c" => COMPARATIVE
    case "s" => SUPERLATIVE
  }
}

case class AdjectiveCase(val gender:AdjectiveGender.Value, val degree:AdjectiveDegree.Value, val declCase: Decl.Value){
  override def toString() = AdjectiveCase.toString(this)
}

object AdjectiveCase {
  implicit def toString(ac: AdjectiveCase):String = 
    AdjectiveGender.toString(ac.gender) + AdjectiveDegree.toString(ac.degree) + Decl.toString(ac.declCase)
    
  implicit def parse(key: String):AdjectiveCase = {
    val gender: AdjectiveGender.Value = key.substring(0, 1)
    val degree: AdjectiveDegree.Value = key.substring(1, 2)
    val declCase: Decl.Value = key.substring(2)
    AdjectiveCase(gender,degree,declCase)
  }
  
  def key(lang: String, gender: AdjectiveGender.Value, degree: AdjectiveDegree.Value, decl: Decl.Value): String 
  	= lang + gender + degree + decl
  def key(gender: AdjectiveGender.Value, degree: AdjectiveDegree.Value, decl: Decl.Value): String = gender + degree + decl
}