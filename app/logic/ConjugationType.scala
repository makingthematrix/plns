package logic

/**
 * The Verb companion object splits all conjugation cases into three sets:
 * * infinitive (TYPE_INF) - those using the infinitive root; ie. the infinitive, the past tense and the passive participle
 * * imperative (TYPE_IMP) - those using the imperative root; ie. the imperatives, the present tense and the active participle
 * * conditional (TYPE_COND) - conditionals, which use the infinitive root but their construction is a bit more complex
 * These three types can be conjugated using the same method, Verb.translateConjugation, which takes the ConjugationType
 * in order to differentiate among them.
 * There is also an artificial fourth set in the Verb object, which is a subset of TYPE_INF containing only the past tense. 
 * It was created because Neoslavonic uses "the long past tense", ie. the copula (to be) in the present tense + the perfect, 
 * and Polish uses "the short past tense", ie. the perfect + endings. This way the translator may distinguish that 
 * "hey! you! language-specific conjugation pattern! for these cases you should handle the conjugation logic by yourself!". 
 * @todo: It should be probably refactored somehow ;)
 */
object ConjugationType extends Enumeration {
  type conjugationType = Value
  val TYPE_INF, TYPE_IMP, TYPE_COND = Value
  
  implicit def toString(t: ConjugationType.Value):String = t.toString()
  
  implicit def parse(str: String) = str.toLowerCase() match {
    case "type_inf" => TYPE_INF
    case "type_imp" => TYPE_IMP
    case "type_cond" => TYPE_COND
    case _ => throw new IllegalArgumentException("No conjugation type found: " + str)
  }
}

