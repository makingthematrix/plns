package logic

trait SpeechPart[T <: SpeechPart[T]] {
	def translateTo(speechPart: T): Unit;
	def mainRoot: String;
}