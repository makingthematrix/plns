package logic

import SpeechPart._
import AdjectiveDegree._

class Adverb(val ind: String, val cmp: String, val sup: String, val cmpIgnored: Boolean, override val lang: String) 
			extends SpeechPart[Adverb](lang) {
  override def mainRoot = ind
  override val speechPart = ADVERB
  override def toRoot(speechPartId: Long) = new Root(mainRoot, speechPart, lang, speechPartId)
  
  override def generate(adv: Adverb, id: Long) = { 
    val indDE = new DictEntry(ind, lang, adv.ind, adv.lang, INDICATIVE, speechPart, id)
    if(!cmpIgnored){
      val cmpDE = new DictEntry(cmp, lang, adv.cmp, adv.lang, COMPARATIVE, speechPart, id)
      val supDE = new DictEntry(sup, lang, adv.sup, adv.lang, SUPERLATIVE, speechPart, id)
      Seq(indDE,cmpDE,supDE)
    } else Seq(indDE)
  }  
  
  override def validateExceptionKey(key: String) = key // adverbs don't have exceptions
}
