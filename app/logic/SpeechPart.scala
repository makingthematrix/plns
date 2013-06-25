package logic

trait SpeechPart[T <: SpeechPart[T]] {
  def translateTo(speechPart: T): Unit;
  def mainRoot: String;
  val speechPart: String;
  val lang: String;
  
  def toRoot():Root;
	
  def addRoots(t: T):(Long,Long) = NSTranslator.addRoots(this.toRoot(), t.toRoot())
}