package models

import logic.Uninflected

/**
 * Holds all data needed for generating a translation between two words.
 * @constructor creates a pair of words
 * @param plWord a word in the source language
 * @param nsWord a word in the target language
 */
case class UninflectedPair(override val id: Long, plWord: String, nsWord: String) 
  extends SpeechPartPair[Uninflected]("uninflected"){
  def this(plWord: String, nsWord: String) = this(SpeechPartPair.noId, plWord, nsWord)
  override def pl = new Uninflected(plWord, "pl")
  override def ns = new Uninflected(nsWord, "ns")

  override def copyWithId(id: Long) = UninflectedPair(id, plWord, nsWord)
}