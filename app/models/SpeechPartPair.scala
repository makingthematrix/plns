package models

import logic.SpeechPart

trait SpeechPartPair[T <: SpeechPart[T]] {
	def pl: T;
	def ns: T;
}