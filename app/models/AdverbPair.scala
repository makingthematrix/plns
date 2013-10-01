package models

import logic.Adverb
import logic.PLAdverb
import logic.PLAdjective
import logic.NSAdverb
import logic.PLMode

case class AdverbPair(plInd: String,plCmp: Option[String],plMode: String,
					  nsInd: String,nsCmp: Option[String], cmpIgnored: Option[String]) 
  extends SpeechPartPair[Adverb]{

  def pl: Adverb = {
    val cmp = plCmp match {
      case None => plInd
      case Some(str) => str
    }
    val ignored = cmpIgnored.getOrElse("").equals("on")
    PLAdverb.word(plInd,cmp,plMode,ignored);
  }
  
  def ns: Adverb = {
    val cmp = nsCmp match {
      case None => nsInd
      case Some(str) => str
    }
    val ignored = cmpIgnored.getOrElse("").equals("on")
    NSAdverb.word(nsInd,cmp,ignored);
  }
}