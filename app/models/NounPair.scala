package models

import logic.Noun
import logic.PLNoun
import logic.NSNoun
import logic.IgnoredNumber

case class NounPair(override val id: Long, plStem: String, plPattern: String, plExceptions: Option[String], 
                                           nsStem: String, nsPattern: String, nsExceptions: Option[String], ignored: String) 
    extends SpeechPartPair[Noun]("noun") {

  def this(plStem: String, plPattern: String, plExceptions: Option[String], 
           nsStem: String, nsPattern: String, nsExceptions: Option[String], ignored: String) =
    this(SpeechPartPair.noId, plStem, plPattern, plExceptions, nsStem, nsPattern, nsExceptions, ignored)
  
  override def pl = {
	val word = PLNoun.word(plStem, plPattern, ignored)
	addExceptions(word,plExceptions)
	word
  }
	
  override def ns = {
	val word = NSNoun.word(nsStem, nsPattern, ignored)
	addExceptions(word,nsExceptions)
	word
  }
  
  override def copyWithId(id: Long) = NounPair(id, plStem, plPattern, plExceptions, nsStem, nsPattern, nsExceptions, ignored)
  
  override protected def contentize = Seq(
      plStem, plPattern, plExceptions.getOrElse(""), nsStem, nsPattern, nsExceptions.getOrElse(""), ignored
  ).mkString(",")
}

object NounPair { 
  /** @todo: design the localization module */
  val ignoredOptions = Map("none" -> "nie ignoruj", "singular" -> "pojedynczej", "plural" -> "mnogiej")
}