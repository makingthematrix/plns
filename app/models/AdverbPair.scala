package models

import logic.Adverb
import logic.PLAdverb
import logic.NSAdverb

/**
 * Holds all data needed for generating translations of all degrees of an adverb.
 * @constructor creates a pair of adverbs
 * @param plInd the stem for the INDICATIVE degree of the adverb in the source language
 * @param plCmp the stem for the COMPARATIVE and SUPERLATIVE degrees of the adverb in the source language
 * @param plMode the adverb declension mode - HARD or SOFT. Used in the Polish language. We have an asymetry here: 
 * Polish is more complex than Neoslavonic not only quantitatively (*whoa*), ie. in number of declension patterns etc.,
 * but also qualitatively - it has a feature which is lacking in Neoslavonic.
 * @param nsInd the stem for the INDICATIVE degree of the adverb in the target language
 * @param nsCmp the stem for the COMPARATIVE and SUPERLATIVE degrees of the adverb in the target language
 */
case class AdverbPair(override val id: Long, plInd: String, plCmp: String, plMode: String,
			     nsInd: String, nsCmp: String, cmpIgnored: String) extends SpeechPartPair[Adverb]("adverb"){
  def this(plInd: String, plCmp: String, plMode: String, nsInd: String, nsCmp: String, cmpIgnored: String) =
    this(SpeechPartPair.noId, plInd, plCmp, plMode, nsInd, nsCmp, cmpIgnored)
  override protected def pl = PLAdverb.word(plInd,plCmp,plMode,cmpIgnored.equals("true"))
  override protected def ns = NSAdverb.word(nsInd,nsCmp,cmpIgnored.equals("true"))
  
  override def copyWithId(id: Long) = AdverbPair(id, plInd, plCmp, plMode, nsInd, nsCmp, cmpIgnored)
  
}