package logic

import scala.collection.mutable

abstract class SpeechPart[T <: SpeechPart[T]] {
  def generate(t: T):Seq[DictEntry]
  def mainRoot: String
  def toRoot:Root
  override def toString = toRoot.toString

  val speechPart: String
  val lang: String
  
  def addRoots(t: T):(Long,Long) = NSTranslator.addRoots(this.toRoot, t.toRoot)
  
  def translateTo(t: T){ 
    val (plRootId,nsRootId) = addRoots(t)
    val translations = generate(t)
    translations.foreach{ entry => NSTranslator.add(entry.wordPair(plRootId, nsRootId)) }
  }  
  
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