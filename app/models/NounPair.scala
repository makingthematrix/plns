package models

import logic.Noun
import logic.PLNoun
import logic.NSNoun
import logic.NounException

case class NounPair(plStem: String,plPattern: String,plExceptions: Option[String], 
                    nsStem: String,nsPattern: String,nsExceptions: Option[String], ignored: String) 
    extends SpeechPartPair[Noun] {
  
  override def pl: Noun = {
	val (ignoredSingular,ignoredPlural) = NounPair.ignoredNumber(ignored)
	val word = PLNoun.word(plStem, plPattern, ignoredSingular, ignoredPlural)
	plExceptions match {
      case Some(str) => NounPair.parse(str).foreach(word.except)
      case None => 
	}
	word
  }
	
  override def ns: Noun = {
	val (ignoredSingular,ignoredPlural) = NounPair.ignoredNumber(ignored)
	val word = NSNoun.word(nsStem, nsPattern, ignoredSingular, ignoredPlural)
	nsExceptions match {
      case Some(str) => NounPair.parse(str).foreach(word.except)
      case None => 
	}
	word
  }
}

object NounPair {
  // exceptions should be in the format "case1:word1,case2:word2,..."
  def parse(exceptions: String):Seq[NounException] = {
    exceptions.split(",").map(str => {
      val t = str.split(":");
      val key = t(0)
      val word = t(1)
      new NounException(key,word);
    });
  }
  
  private def ignoredNumber(ignored: String) = ignored match {
	case "none" => (false,false)
	case "singular" => (true,false)
	case "plural" => (false,true)
	case _ => throw new IllegalArgumentException("NounPair.ignoredNumber, invalid 'ignored' value: " + ignored)
  }
  
  val ignoredOptions = Map("none" -> "nie ignoruj", "singular" -> "pojedynczej", "plural" -> "mnogiej")
}