package models

import logic.Noun
import logic.PLNoun
import logic.NSNoun

case class NounPair(plStem: String,plPattern: String,plExceptions: Option[String], 
                    nsStem: String,nsPattern: String,nsExceptions: Option[String], ignored: String) 
    extends SpeechPartPair[Noun] {
  
  override def pl: Noun = {
	val word = PLNoun.word(plStem, plPattern, ignored)
	plExceptions match {
      case Some(str) => SpeechPartPair.parseExceptions(str).foreach( ne => word.except(ne._1,ne._2) )
      case None => 
	}
	word
  }
	
  override def ns: Noun = {
	val word = NSNoun.word(nsStem, nsPattern, ignored)
	nsExceptions match {
      case Some(str) => SpeechPartPair.parseExceptions(str).foreach( ne => word.except(ne._1,ne._2) )
      case None => 
	}
	word
  }
}

object NounPair { 
  val ignoredOptions = Map("none" -> "nie ignoruj", "singular" -> "pojedynczej", "plural" -> "mnogiej")
}