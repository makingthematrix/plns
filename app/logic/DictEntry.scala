package logic

import SpeechPart._
import models.SpeechPartPair

case class DictEntry(id: Long, plWord: String, plLang: String, 
                     nsWord: String, nsLang: String, 
                     caseId: String, speechPart: SpeechPart.Value, speechPartId: Long) extends Contentized {
  def this(plWord: String, plLang: String, nsWord: String, nsLang: String) 
    = this(DictEntry.noId, plWord, plLang, nsWord, nsLang, DictEntry.undef, UNINFLECTED, SpeechPartPair.noId)

  def this(plWord: String, plLang: String, nsWord: String, nsLang: String, 
           caseId: String, speechPart: SpeechPart.Value, speechPartId: Long) 
    = this(DictEntry.noId, plWord, plLang, nsWord, nsLang, caseId, speechPart, speechPartId)
    
  override protected def contentize = Seq(
    plWord, plLang, nsWord, nsLang, 
    caseId, speechPart.toString(), speechPartId
  ).mkString(",")
}

object DictEntry {
  val undef = "undef" // undefined case
  val noId = -1L;
}