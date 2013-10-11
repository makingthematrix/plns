package models

import logic.UnInflected

/**
 * Holds all data needed for generating a translation between two words.
 * @constructor creates a pair of words
 * @param plWord a word in the source language
 * @param nsWord a word in the target language
 */
case class UninflectedPair(val plWord: String, val nsWord: String) extends SpeechPartPair[UnInflected] {
  override def pl = new UnInflected(plWord,"pl")
  override def ns = new UnInflected(nsWord,"ns")
}