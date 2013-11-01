package logic
import SpeechPart._

class Uninflected(val word: String, override val lang: String) extends SpeechPart[Uninflected](lang) {
  override def mainRoot = word
  override val speechPart = UNINFLECTED
  override def toRoot(speechPartId: Long) = new Root(mainRoot, speechPart, lang, speechPartId)  
  override def generate(un: Uninflected, id: Long) = 
    Seq(new DictEntry(word,lang,un.word, un.lang, Uninflected.caseId, speechPart, id))
  override def validateExceptionKey(key: String): String = key // uninflected don't have exceptions
}

object Uninflected {
  val caseId = ""
}