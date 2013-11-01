package logic

import scala.collection.mutable

abstract class SpeechPart[T <: SpeechPart[T]](val lang: String) {
  def generate(t: T, speechPartId: Long):Seq[DictEntry]
  def mainRoot: String
  def toRoot(speechPartId: Long):Root

  val speechPart: SpeechPart.Value
  
  /**
   * adds an exception exception
   * @param key the declension/conjugation/other of the exceptional case; it should be a valid case for the given speech part 
   * @param word the exception form for this case
   */
  def except(key: String, word: String) = exceptions.put(validateExceptionKey(key), word)
  
  /** check if the given key is valid for this speech part */
  protected def validateExceptionKey(key: String): String

  /** the map of exceptions, case -> word */
  protected val exceptions = new mutable.HashMap[String,String]()
}

object SpeechPart extends Enumeration {
  type SpeechPart = Value
  val UNINFLECTED, ADVERB, ADJECTIVE, NOUN, VERB = Value
  
  implicit def toString(sp: SpeechPart.Value) = sp.toString.toLowerCase
  
  implicit def parse(str: String) = str.toLowerCase match {
    case "uninflected" => UNINFLECTED
    case "adverb" => ADVERB
    case "adjective" => ADJECTIVE
    case "noun" => NOUN
    case "verb" => VERB
    case _ => throw new IllegalArgumentException("Unable to parse SpeechPart.Value: " + str)
  }
}