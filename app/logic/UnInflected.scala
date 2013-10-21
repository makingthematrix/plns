package logic
import SpeechPart._

class UnInflected(val word: String, val lang: String) extends SpeechPart[UnInflected](lang) {
  override def mainRoot = word
  override val speechPart = UNINFLECTED
  override def toRoot = new Root(mainRoot, speechPart, lang)  
  override def generate(un: UnInflected, id: Long) = 
    Seq(new DictEntry(word,lang,un.word, un.lang, UnInflected.caseId, speechPart, id))
  override def validateExceptionKey(key: String): String = key // uninflected don't have exceptions
}

object UnInflected {
  val caseId = ""
}