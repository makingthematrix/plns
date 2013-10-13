package models

import logic.Adjective
import logic.PLAdjective
import logic.NSAdjective
import logic.AdjectiveCase

/**
 * Holds all data needed for generating translations of all cases of an adjective and the associated adverb.
 * @constructor creates an adjective pair
 * @param plInd the stem for the INDICATIVE degree of the adjective in the source language
 * @param plAdvInd the stem for the INDICATIVE degree of the adverb in the source language
 * @param plCmp the stem for the COMPARATIVE and SUPERLATIVE degrees of the adjective in the source language
 * @param plAdvCmp the stem for the COMPARATIVE and SUPERLATIVE degrees of the adverb in the source language
 * @param plMode the adjective declension mode - HARD or SOFT. Used in the Polish language.
 * @param plAdvMode the adverb declension mode - HARD or SOFT. Used in the Polish language.
 * @param plExceptions a string composed of exceptions from the regular declension in the source language, if there are any, or None
 * @param nsInd the stem for the INDICATIVE degree of the adjective in the target language
 * @param nsAdvInd the stem for the INDICATIVE degree of the adverb in the target language
 * @param nsCmp the stem for the COMPARATIVE and SUPERLATIVE degrees of the adjective in the target language
 * @param nsAdvCmp the stem for the COMPARATIVE and SUPERLATIVE degrees of the adverb in the target language
 * @param nsExceptions a string composed of exceptions from the regular declension in the target language, if there are any, or None
 * @see logic.AdjectiveCase
 */
case class AdjectivePair(override val id: Long, plInd: String, plAdvInd: String, plCmp: String, plAdvCmp: String,
                         plMode: String, plAdvMode: String, plExceptions: Option[String],
                         nsInd: String, nsAdvInd: String, nsCmp: String,nsAdvCmp: String,
                         nsExceptions: Option[String], cmpIgnored: String) extends SpeechPartPair[Adjective]{
  def this(plInd: String, plAdvInd: String, plCmp: String, plAdvCmp: String, plMode: String, plAdvMode: String, plExceptions: Option[String],
           nsInd: String, nsAdvInd: String, nsCmp: String, nsAdvCmp: String, nsExceptions:Option[String], cmpIgnored: String) =
    this(SpeechPartPair.noId, plInd, plAdvInd, plCmp, plAdvCmp, plMode, plAdvMode, plExceptions, 
                              nsInd, nsAdvInd, nsCmp, nsAdvCmp, nsExceptions, cmpIgnored)
  def this(plInd:String, plCmp:String, plMode:String, plExceptions:Option[String],
           nsInd:String, nsCmp:String, nsExceptions:Option[String]) =
    this(SpeechPartPair.noId, plInd, plInd, plCmp, plCmp, plMode, plMode, plExceptions, 
                              nsInd, nsInd, nsCmp, nsCmp, nsExceptions, "false")
  def this(plInd:String, plMode:String, plExceptions:Option[String], nsInd:String, nsExceptions:Option[String]) =
    this(SpeechPartPair.noId, plInd, plInd, plInd, plInd, plMode, plMode, plExceptions,
                              nsInd, nsInd, nsInd, nsInd, nsExceptions, "true")
  
  /**
   * Build an Adjective of the source language based on the available data
   */
  override def pl = {	
    val word = PLAdjective.word(plInd, plCmp, plAdvInd, plAdvCmp, plMode, plAdvMode, cmpIgnored.equals("true"))
    addExceptions(word,plExceptions) 
    word
  }

  /**
   * Build a n Adjective of the target language based on the available data
   */
  override def ns = {
    val word = NSAdjective.word(nsInd, nsCmp, nsAdvInd, nsAdvCmp, cmpIgnored.equals("true"))
    addExceptions(word,nsExceptions)
    word
  }
  
}