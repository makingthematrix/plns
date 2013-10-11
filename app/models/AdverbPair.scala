package models

import logic.Adverb
import logic.PLAdverb
import logic.NSAdverb
import logic.PLMode

/**
 * Holds all data needed for generating translations of all degrees of an adverb.
 * @constructor creates a pair of adverbs
 * @param plInd the stem for the INDICATIVE degree of the adverb in the source language
 * @param plCmp the stem for the COMPARATIVE and SUPERLATIVE degrees of the adverb in the source language
 * @param plMode the adverb declension mode - HARD or SOFT. Used in the Polish language
 * @param nsInd the stem for the INDICATIVE degree of the adverb in the target language
 * @param nsCmp the stem for the COMPARATIVE and SUPERLATIVE degrees of the adverb in the target language
 */
case class AdverbPair(plInd: String,plCmp: String,plMode: String,
			     nsInd: String,nsCmp: String, cmpIgnored: String) extends SpeechPartPair[Adverb]{
  override def pl = PLAdverb.word(plInd,plCmp,plMode,cmpIgnored.equals("true"))
  override def ns = NSAdverb.word(nsInd,nsCmp,cmpIgnored.equals("true"))
}