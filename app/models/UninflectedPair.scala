package models

import logic.UnInflected

case class UninflectedPair(val plWord: String, val nsWord: String) extends SpeechPartPair[UnInflected] {
  def pl: UnInflected = new UnInflected(plWord)
  def ns: UnInflected = new UnInflected(nsWord)
}