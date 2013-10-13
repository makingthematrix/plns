package models

import logic.UnInflected

/**
 * Holds all data needed for generating a translation between two words.
 * @constructor creates a pair of words
 * @param plWord a word in the source language
 * @param nsWord a word in the target language
 */
case class UninflectedPair(override val id: Long, plWord: String, nsWord: String) 
  extends SpeechPartPair[UnInflected] {
  def this(plWord: String, nsWord: String) = this(SpeechPartPair.noId, plWord, nsWord)
  override def pl = new UnInflected(plWord,"pl")
  override def ns = new UnInflected(nsWord,"ns")
}